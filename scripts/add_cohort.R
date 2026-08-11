#!/usr/bin/env Rscript

# Add one cohort to a Living Evidence Hub study.
#
# Run from the hub-data repository root:
#   Rscript scripts/add_cohort.R STUDY_ID COHORT META_CSV [TRAJECTORY_CSV]

add_cohort_usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript scripts/add_cohort.R STUDY_ID COHORT META_CSV [TRAJECTORY_CSV]\n\n",
    "Examples:\n",
    "  Rscript scripts/add_cohort.R ezdy6 2 exports/Study2_META.csv\n",
    paste0(
      "  Rscript scripts/add_cohort.R ezdy6 2 exports/Study2_META.csv ",
      "exports/Study2_trajectory.csv\n"
    ),
    sep = ""
  )
}

add_cohort_script_path <- function() {
  file_argument <- grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )
  if (length(file_argument) != 1L) return(NA_character_)
  normalizePath(
    sub("^--file=", "", file_argument),
    winslash = "/",
    mustWork = FALSE
  )
}

add_cohort_default_root <- function() {
  script_path <- add_cohort_script_path()
  if (!is.na(script_path) && basename(script_path) == "add_cohort.R") {
    return(dirname(dirname(script_path)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

add_cohort_load_validator <- function(hub_root, validator_path = NULL) {
  if (is.null(validator_path)) {
    validator_path <- file.path(hub_root, "scripts", "validate_hub_data.R")
  }
  validator_path <- normalizePath(
    validator_path,
    winslash = "/",
    mustWork = FALSE
  )
  if (!file.exists(validator_path)) {
    stop("Validator is missing: ", validator_path, ".")
  }

  validator <- new.env(parent = baseenv())
  sys.source(validator_path, envir = validator)
  required_functions <- c(
    "hub_read_csv",
    "hub_safe_relative_path",
    "hub_sha256",
    "validate_hub_data"
  )
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1),
            envir = validator, inherits = FALSE)
  ]
  if (length(missing_functions) > 0L) {
    stop(
      "Validator does not provide required function(s): ",
      paste(missing_functions, collapse = ", "),
      "."
    )
  }
  validator
}

add_cohort_validation_message <- function(result, heading) {
  details <- if (length(result$errors) > 0L) {
    paste0("\n  - ", paste(result$errors, collapse = "\n  - "))
  } else {
    ""
  }
  paste0(heading, details)
}

add_cohort_regular_file <- function(path, label) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(path) || dir.exists(path)) {
    stop(label, " does not exist or is not a regular file: ", path, ".")
  }
  path
}

add_cohort_read_raw <- function(path) {
  size <- file.info(path)$size
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = size)
}

add_cohort_write_raw <- function(path, contents, append = FALSE) {
  connection <- file(path, open = if (append) "ab" else "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(contents, connection)
  invisible(path)
}

add_cohort_csv_field <- function(value) {
  value <- as.character(value)
  if (length(value) == 0L || is.na(value)) value <- ""
  value <- enc2utf8(value[1])
  escaped <- gsub('"', '""', value, fixed = TRUE)
  if (grepl('[,"\r\n]', value)) paste0('"', escaped, '"') else escaped
}

add_cohort_manifest_lines <- function(rows) {
  apply(rows, 1L, function(row) {
    paste(vapply(row, add_cohort_csv_field, character(1)), collapse = ",")
  })
}

add_cohort_append_manifest <- function(path, rows) {
  original_size <- file.info(path)$size
  needs_newline <- FALSE
  if (original_size > 0) {
    connection <- file(path, open = "rb")
    on.exit(close(connection), add = TRUE)
    seek(connection, where = original_size - 1, origin = "start")
    final_byte <- readBin(connection, what = "raw", n = 1L)
    close(connection)
    on.exit(NULL, add = FALSE)
    needs_newline <- length(final_byte) == 1L &&
      !as.integer(final_byte) %in% c(10L, 13L)
  }

  text <- paste0(
    if (needs_newline) "\n" else "",
    paste(add_cohort_manifest_lines(rows), collapse = "\n"),
    "\n"
  )
  add_cohort_write_raw(path, charToRaw(enc2utf8(text)), append = TRUE)
}

add_cohort_normalize_root <- function(hub_root) {
  hub_root <- normalizePath(hub_root, winslash = "/", mustWork = FALSE)
  if (!dir.exists(hub_root)) {
    stop("Hub data directory does not exist: ", hub_root, ".")
  }
  hub_root
}

add_cohort <- function(study_id, cohort, meta_csv, trajectory_csv = NULL,
                       hub_root = add_cohort_default_root(),
                       validator_path = NULL, quiet = FALSE) {
  hub_root <- add_cohort_normalize_root(hub_root)
  validator <- add_cohort_load_validator(hub_root, validator_path)

  study_id <- trimws(as.character(study_id)[1])
  cohort_text <- trimws(as.character(cohort)[1])
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", study_id)) {
    stop(
      "STUDY_ID may contain only letters, numbers, periods, underscores, ",
      "and hyphens."
    )
  }
  if (!grepl("^[1-9][0-9]*$", cohort_text)) {
    stop("COHORT must be a positive integer.")
  }

  meta_csv <- add_cohort_regular_file(meta_csv, "META_CSV")
  if (!grepl("_META\\.csv$", basename(meta_csv), ignore.case = TRUE)) {
    stop("META_CSV filename must end in _META.csv.")
  }

  has_trajectory <- !is.null(trajectory_csv) &&
    length(trajectory_csv) > 0L &&
    !is.na(trajectory_csv[1]) &&
    nzchar(trimws(as.character(trajectory_csv)[1]))
  if (has_trajectory) {
    trajectory_csv <- add_cohort_regular_file(
      trajectory_csv,
      "TRAJECTORY_CSV"
    )
    if (!grepl("_trajectory\\.csv$", basename(trajectory_csv),
               ignore.case = TRUE)) {
      stop("TRAJECTORY_CSV filename must end in _trajectory.csv.")
    }
    if (identical(tolower(meta_csv), tolower(trajectory_csv))) {
      stop("META_CSV and TRAJECTORY_CSV must be different files.")
    }
  }

  initial_validation <- validator$validate_hub_data(hub_root, verbose = FALSE)
  if (!isTRUE(initial_validation$valid)) {
    stop(add_cohort_validation_message(
      initial_validation,
      "The Hub data is already invalid. Fix these errors before adding a cohort:"
    ))
  }

  directory_path <- file.path(hub_root, "directory.csv")
  directory <- validator$hub_read_csv(directory_path)
  study_matches <- which(directory$Study_ID == study_id)
  if (length(study_matches) != 1L) {
    stop("STUDY_ID is not listed exactly once in directory.csv: ", study_id, ".")
  }
  study_row <- directory[study_matches, , drop = FALSE]
  expected_study_path <- paste0("studies/", study_id)
  if (!identical(as.character(study_row$Study_Path[1]), expected_study_path)) {
    stop("Study_Path must equal ", expected_study_path, ".")
  }

  study_directory <- file.path(hub_root, "studies", study_id)
  manifest_path <- file.path(study_directory, "manifest.csv")
  upload_directory <- file.path(study_directory, "cohort_uploads")
  manifest <- validator$hub_read_csv(manifest_path)

  sources <- c(meta_csv, if (has_trajectory) trajectory_csv)
  roles <- c("meta", if (has_trajectory) "trajectory")
  basenames <- basename(sources)
  relative_paths <- paste0("cohort_uploads/", basenames)

  for (relative_path in relative_paths) {
    path_problem <- validator$hub_safe_relative_path(relative_path)
    if (!is.null(path_problem)) {
      stop("Destination path ", path_problem, ": ", relative_path, ".")
    }
  }

  manifest_path_lower <- tolower(trimws(as.character(manifest$path)))
  duplicate_path <- relative_paths[
    tolower(relative_paths) %in% manifest_path_lower
  ]
  if (length(duplicate_path) > 0L) {
    stop(
      "Manifest already contains path(s): ",
      paste(duplicate_path, collapse = ", "),
      "."
    )
  }

  manifest_role <- tolower(trimws(as.character(manifest$role)))
  manifest_cohort <- trimws(as.character(manifest$cohort))
  duplicate_role <- roles[vapply(roles, function(role) {
    any(manifest_role == role & manifest_cohort == cohort_text)
  }, logical(1))]
  if (length(duplicate_role) > 0L) {
    stop(
      "Manifest already contains role/cohort combination(s): ",
      paste0(duplicate_role, "/", cohort_text, collapse = ", "),
      "."
    )
  }

  if (dir.exists(upload_directory)) {
    existing_names <- list.files(
      upload_directory,
      all.files = TRUE,
      no.. = TRUE
    )
    overwrite_names <- basenames[
      tolower(basenames) %in% tolower(existing_names)
    ]
    if (length(overwrite_names) > 0L) {
      stop(
        "Refusing to overwrite existing file(s): ",
        paste(overwrite_names, collapse = ", "),
        "."
      )
    }
  }

  source_bytes <- vapply(
    sources,
    function(path) file.info(path)$size,
    numeric(1)
  )
  if (any(source_bytes > validator$hub_max_data_file_bytes)) {
    too_large <- basenames[source_bytes > validator$hub_max_data_file_bytes]
    stop(
      "META and trajectory files may not exceed 50 MiB: ",
      paste(too_large, collapse = ", "),
      "."
    )
  }
  existing_data_rows <- manifest_role %in% c("meta", "trajectory")
  existing_data_bytes <- suppressWarnings(
    as.numeric(trimws(as.character(manifest$bytes[existing_data_rows])))
  )
  combined_data_bytes <- sum(existing_data_bytes) + sum(source_bytes)
  if (!is.finite(combined_data_bytes) ||
      combined_data_bytes > validator$hub_max_data_bundle_bytes) {
    stop(
      "META and trajectory files may not exceed 100 MiB in total for one study."
    )
  }
  source_sha256 <- vapply(sources, validator$hub_sha256, character(1))

  original_manifest <- add_cohort_read_raw(manifest_path)
  copied_files <- character()
  upload_directory_created <- FALSE
  committed <- FALSE

  rollback <- function() {
    tryCatch(
      add_cohort_write_raw(manifest_path, original_manifest, append = FALSE),
      error = function(e) warning(
        "Could not restore manifest during rollback: ",
        conditionMessage(e)
      )
    )
    for (path in rev(copied_files)) {
      if (file.exists(path) && !dir.exists(path)) unlink(path, force = TRUE)
    }
    if (upload_directory_created && dir.exists(upload_directory)) {
      unlink(upload_directory, recursive = FALSE, force = TRUE)
    }
  }

  on.exit({
    if (!committed) rollback()
  }, add = TRUE)

  if (!dir.exists(upload_directory)) {
    if (!dir.create(upload_directory, recursive = FALSE)) {
      stop("Could not create cohort_uploads directory: ", upload_directory, ".")
    }
    upload_directory_created <- TRUE
  }

  destination_paths <- file.path(upload_directory, basenames)
  for (i in seq_along(sources)) {
    copied <- file.copy(
      from = sources[i],
      to = destination_paths[i],
      overwrite = FALSE,
      copy.date = FALSE
    )
    if (!isTRUE(copied)) {
      stop("Could not copy ", sources[i], " to ", destination_paths[i], ".")
    }
    copied_files <- c(copied_files, destination_paths[i])

    copied_bytes <- file.info(destination_paths[i])$size
    copied_sha256 <- validator$hub_sha256(destination_paths[i])
    if (!identical(as.numeric(copied_bytes), as.numeric(source_bytes[i])) ||
        !identical(copied_sha256, unname(source_sha256[i]))) {
      stop("Copied content differs from source file: ", basenames[i], ".")
    }
  }

  manifest_rows <- data.frame(
    path = relative_paths,
    role = roles,
    cohort = rep(cohort_text, length(roles)),
    display_name = paste0(
      "Cohort ", cohort_text, " ",
      ifelse(roles == "meta", "metadata", "trajectory"),
      " (", basenames, ")"
    ),
    media_type = rep("text/csv", length(roles)),
    bytes = format(source_bytes, scientific = FALSE, trim = TRUE),
    sha256 = source_sha256,
    stringsAsFactors = FALSE
  )
  manifest_rows <- manifest_rows[
    , c("path", "role", "cohort", "display_name", "media_type", "bytes",
        "sha256"),
    drop = FALSE
  ]
  add_cohort_append_manifest(manifest_path, manifest_rows)

  final_validation <- validator$validate_hub_data(hub_root, verbose = FALSE)
  if (!isTRUE(final_validation$valid)) {
    stop(add_cohort_validation_message(
      final_validation,
      paste0(
        "The new cohort failed repository validation. ",
        "The copied files and manifest changes were rolled back:"
      )
    ))
  }

  committed <- TRUE
  result <- list(
    study_id = study_id,
    cohort = as.integer(cohort_text),
    files = relative_paths,
    manifest_rows = manifest_rows,
    validation = final_validation
  )

  if (!quiet) {
    cat(sprintf("Added cohort %s to study %s.\n", cohort_text, study_id))
    cat(paste0("  ", roles, ": ", relative_paths, "\n"), sep = "")
    cat("Repository validation passed.\n")
  }

  result
}

add_cohort_cli <- function(args = commandArgs(trailingOnly = TRUE),
                           hub_root = add_cohort_default_root(),
                           validator_path = NULL) {
  if (any(args %in% c("-h", "--help"))) {
    add_cohort_usage()
    return(0L)
  }
  if (!length(args) %in% c(3L, 4L)) {
    add_cohort_usage()
    cat("\nExpected three or four positional arguments.\n")
    return(2L)
  }

  status <- tryCatch(
    {
      add_cohort(
        study_id = args[1],
        cohort = args[2],
        meta_csv = args[3],
        trajectory_csv = if (length(args) == 4L) args[4] else NULL,
        hub_root = hub_root,
        validator_path = validator_path,
        quiet = FALSE
      )
      0L
    },
    error = function(e) {
      cat("ERROR ", conditionMessage(e), "\n", sep = "")
      1L
    }
  )
  status
}

if (sys.nframe() == 0L) {
  quit(status = add_cohort_cli(), save = "no")
}
