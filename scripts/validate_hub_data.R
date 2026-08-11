#!/usr/bin/env Rscript

# Validate the checked-in data used by the Living Evidence Hub.
#
# Usage from the repository root:
#   Rscript scripts/validate_hub_data.R .
#
# The script uses base R. For SHA-256 it uses, in order, the digest or openssl
# R package, sha256sum, shasum, or Windows certutil.

hub_directory_columns <- c(
  "Study_ID", "Study_Title", "Study_Path", "Abstract", "DOI"
)

hub_manifest_columns <- c(
  "path", "role", "cohort", "display_name", "media_type", "bytes", "sha256"
)

hub_roles <- c("meta", "trajectory", "abstract", "material")

# These match the runtime limits in app.R. Contributions that pass validation
# must also be loadable by the deployed Hub.
hub_max_data_file_bytes <- 50 * 1024^2
hub_max_data_bundle_bytes <- 100 * 1024^2

hub_meta_columns <- c(
  "Group", "Status", "Prereg_Delta", "Prereg_Dir", "Final_E",
  "Total_N", "Timestamp"
)

hub_trajectory_columns <- c("n", "log_e")

hub_primary_sources <- c(
  "adjusted_phase1",
  "adjusted_phase2_shrunk",
  "adjusted_phase2_unshrunk",
  "safe_t_test",
  "anytime_valid_regression"
)

hub_comparison_sources <- c(
  "raw_unadjusted",
  "adjusted_phase2_unshrunk_comparison"
)

hub_trim <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

hub_is_blank <- function(x) {
  !nzchar(hub_trim(x))
}

hub_is_missing_value <- function(x) {
  value <- hub_trim(x)
  !nzchar(value) | toupper(value) == "NA"
}

hub_read_csv <- function(path) {
  # read.csv warns when a valid legacy CSV lacks a final newline. readr, which
  # the app uses, accepts the same file silently. Parsing failures still error.
  suppressWarnings(utils::read.csv(
      path,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = character(),
      comment.char = "",
      fileEncoding = "UTF-8-BOM"
    ))
}

hub_safe_relative_path <- function(path) {
  path <- hub_trim(path)[1]

  if (!nzchar(path)) {
    return("is blank")
  }
  if (grepl("\\\\", path)) {
    return("must use forward slashes")
  }
  if (grepl("^[A-Za-z]:", path) || startsWith(path, "/")) {
    return("must be relative")
  }

  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (any(parts %in% c("", ".", ".."))) {
    return("contains an empty, current-directory, or parent-directory segment")
  }
  if (any(grepl("[<>:\"|?*]", parts))) {
    return("contains a character that is not portable across Git clients")
  }
  if (any(grepl("[[:cntrl:]]", parts))) {
    return("contains a control character")
  }

  NULL
}

hub_path <- function(root, relative_path) {
  parts <- strsplit(relative_path, "/", fixed = TRUE)[[1]]
  do.call(file.path, c(list(root), as.list(parts)))
}

hub_format_values <- function(x, limit = 5L) {
  x <- unique(as.character(x))
  if (length(x) > limit) {
    x <- c(x[seq_len(limit)], sprintf("... (%d more)", length(x) - limit))
  }
  paste(x, collapse = ", ")
}

hub_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(tolower(digest::digest(
      file = path,
      algo = "sha256",
      serialize = FALSE
    )))
  }

  if (requireNamespace("openssl", quietly = TRUE)) {
    size <- file.info(path)$size
    connection <- file(path, open = "rb")
    on.exit(close(connection), add = TRUE)
    contents <- readBin(connection, what = "raw", n = size)
    return(tolower(as.character(openssl::sha256(contents))))
  }

  run_hash_command <- function(command, args) {
    output <- suppressWarnings(tryCatch(
      system2(command, args = args, stdout = TRUE, stderr = TRUE),
      error = function(e) character()
    ))
    compact <- gsub("[[:space:]]+", "", output)
    matches <- compact[grepl("^[0-9A-Fa-f]{64}$", compact)]
    if (length(matches) == 0L) NULL else tolower(matches[1])
  }

  sha256sum <- Sys.which("sha256sum")
  if (nzchar(sha256sum)) {
    hash <- run_hash_command(sha256sum, shQuote(path))
    if (!is.null(hash)) return(hash)
  }

  shasum <- Sys.which("shasum")
  if (nzchar(shasum)) {
    hash <- run_hash_command(shasum, c("-a", "256", shQuote(path)))
    if (!is.null(hash)) return(hash)
  }

  certutil <- Sys.which("certutil")
  if (nzchar(certutil)) {
    hash <- run_hash_command(certutil, c("-hashfile", shQuote(path), "SHA256"))
    if (!is.null(hash)) return(hash)
  }

  stop(
    "No SHA-256 implementation is available. Install the R package 'digest' ",
    "or make sha256sum, shasum, or certutil available on PATH."
  )
}

hub_check_columns <- function(data, expected, context, add_error) {
  duplicate_columns <- unique(names(data)[duplicated(names(data))])
  if (length(duplicate_columns) > 0L) {
    add_error(
      context,
      paste0("duplicate column name(s): ",
             hub_format_values(duplicate_columns), ".")
    )
  }

  missing_columns <- setdiff(expected, names(data))
  unexpected_columns <- setdiff(names(data), expected)

  if (length(missing_columns) > 0L) {
    add_error(
      context,
      paste0("missing column(s): ", hub_format_values(missing_columns), ".")
    )
  }
  if (length(unexpected_columns) > 0L) {
    add_error(
      context,
      paste0("unexpected column(s): ",
             hub_format_values(unexpected_columns), ".")
    )
  }

  length(duplicate_columns) == 0L &&
    length(missing_columns) == 0L &&
    length(unexpected_columns) == 0L
}

hub_check_required_columns <- function(data, required, context, add_error) {
  duplicate_columns <- unique(names(data)[duplicated(names(data))])
  if (length(duplicate_columns) > 0L) {
    add_error(
      context,
      paste0("duplicate column name(s): ",
             hub_format_values(duplicate_columns), ".")
    )
  }

  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    add_error(
      context,
      paste0("missing required column(s): ",
             hub_format_values(missing_columns), ".")
    )
  }

  length(duplicate_columns) == 0L && length(missing_columns) == 0L
}

hub_validate_meta <- function(data, context, add_error) {
  schema_ok <- hub_check_required_columns(
    data,
    hub_meta_columns,
    context,
    add_error
  )
  if (!schema_ok) return(character())

  if (nrow(data) == 0L) {
    add_error(context, "contains no data rows.")
    return(character())
  }

  for (column in c("Group", "Status", "Prereg_Dir", "Total_N",
                   "Timestamp")) {
    bad_rows <- which(hub_is_missing_value(data[[column]]))
    if (length(bad_rows) > 0L) {
      add_error(
        context,
        sprintf(
          "%s is blank on row(s): %s.",
          column,
          hub_format_values(bad_rows)
        )
      )
    }
  }

  direction <- hub_trim(data$Prereg_Dir)
  known_directions <- c("greater", "less", "two.sided", "twoSided")
  bad_direction <- which(
    !hub_is_missing_value(direction) & !direction %in% known_directions
  )
  if (length(bad_direction) > 0L) {
    add_error(
      context,
      sprintf(
        "Prereg_Dir has an unsupported value on row(s): %s.",
        hub_format_values(bad_direction)
      )
    )
  }

  final_e <- suppressWarnings(as.numeric(hub_trim(data$Final_E)))
  final_log_e <- if ("Final_Log_E" %in% names(data)) {
    suppressWarnings(as.numeric(hub_trim(data$Final_Log_E)))
  } else {
    rep(NA_real_, nrow(data))
  }
  has_valid_evidence <- (is.finite(final_e) & final_e > 0) |
    is.finite(final_log_e)
  bad_final_e <- which(!has_valid_evidence)
  if (length(bad_final_e) > 0L) {
    add_error(
      context,
      sprintf(
        paste0(
          "Final_E must be positive and finite, or Final_Log_E must be ",
          "finite, on row(s): %s."
        ),
        hub_format_values(bad_final_e)
      )
    )
  }

  total_n <- suppressWarnings(as.numeric(hub_trim(data$Total_N)))
  bad_total_n <- which(!is.finite(total_n) | total_n <= 0)
  if (length(bad_total_n) > 0L) {
    add_error(
      context,
      sprintf(
        "Total_N must be positive and finite on row(s): %s.",
        hub_format_values(bad_total_n)
      )
    )
  }

  test_type <- if ("Test_Type" %in% names(data)) {
    hub_trim(data$Test_Type)
  } else {
    rep("", nrow(data))
  }
  delta <- suppressWarnings(as.numeric(hub_trim(data$Prereg_Delta)))
  missing_delta <- hub_is_missing_value(data$Prereg_Delta) | !is.finite(delta)
  avreg <- test_type == "anytime_valid_regression"
  bad_delta <- which((missing_delta & !avreg) |
                       (!missing_delta & delta <= 0))
  if (length(bad_delta) > 0L) {
    add_error(
      context,
      sprintf(
        paste0(
          "Prereg_Delta must be positive, except that it may be missing for ",
          "anytime-valid regression, on row(s): %s."
        ),
        hub_format_values(bad_delta)
      )
    )
  }

  if ("Final_Log_E" %in% names(data)) {
    supplied <- !hub_is_missing_value(data$Final_Log_E)
    final_log_e <- suppressWarnings(as.numeric(hub_trim(data$Final_Log_E)))
    bad_final_log_e <- which(supplied & !is.finite(final_log_e))
    if (length(bad_final_log_e) > 0L) {
      add_error(
        context,
        sprintf(
          "Final_Log_E must be finite when supplied on row(s): %s.",
          hub_format_values(bad_final_log_e)
        )
      )
    }
  }

  if ("DV" %in% names(data)) {
    bad_dv <- which(hub_is_missing_value(data$DV))
    if (length(bad_dv) > 0L) {
      add_error(
        context,
        sprintf("DV is blank on row(s): %s.", hub_format_values(bad_dv))
      )
    }
    duplicated_dv <- unique(hub_trim(data$DV)[duplicated(hub_trim(data$DV))])
    if (length(duplicated_dv) > 0L) {
      add_error(
        context,
        paste0("contains duplicate DV value(s): ",
               hub_format_values(duplicated_dv), ".")
      )
    }
    return(unique(hub_trim(data$DV)[!hub_is_missing_value(data$DV)]))
  }

  character()
}

hub_validate_trajectory <- function(data, context, add_error) {
  schema_ok <- hub_check_required_columns(
    data,
    hub_trajectory_columns,
    context,
    add_error
  )
  if (!schema_ok) return(character())

  if (nrow(data) == 0L) {
    add_error(context, "contains no data rows.")
    return(character())
  }

  n <- suppressWarnings(as.numeric(hub_trim(data$n)))
  log_e <- suppressWarnings(as.numeric(hub_trim(data$log_e)))

  bad_n <- which(!is.finite(n) | n < 0)
  if (length(bad_n) > 0L) {
    add_error(
      context,
      sprintf(
        "n must be non-negative and finite on row(s): %s.",
        hub_format_values(bad_n)
      )
    )
  }

  bad_log_e <- which(!is.finite(log_e))
  if (length(bad_log_e) > 0L) {
    add_error(
      context,
      sprintf(
        "log_e must be finite on row(s): %s.",
        hub_format_values(bad_log_e)
      )
    )
  }

  primary_rows <- rep(TRUE, nrow(data))
  if ("Source" %in% names(data)) {
    source <- hub_trim(data$Source)
    known_sources <- c(hub_primary_sources, hub_comparison_sources)
    bad_source <- which(hub_is_missing_value(source) |
                          !source %in% known_sources)
    if (length(bad_source) > 0L) {
      add_error(
        context,
        sprintf(
          "Source is blank or unsupported on row(s): %s.",
          hub_format_values(bad_source)
        )
      )
    }
    primary_rows <- source %in% hub_primary_sources
    if (!any(primary_rows)) {
      add_error(context, "contains no primary trajectory rows.")
    }
  }

  if ("DV" %in% names(data)) {
    bad_dv <- which(primary_rows & hub_is_missing_value(data$DV))
    if (length(bad_dv) > 0L) {
      add_error(
        context,
        sprintf(
          "DV is blank on primary row(s): %s.",
          hub_format_values(bad_dv)
        )
      )
    }

    primary_dv <- hub_trim(data$DV[primary_rows])
    primary_n <- n[primary_rows]
    usable <- nzchar(primary_dv) & is.finite(primary_n)
    keys <- paste(primary_dv[usable], format(primary_n[usable], trim = TRUE),
                  sep = "\r")
    duplicate_keys <- unique(keys[duplicated(keys)])
    if (length(duplicate_keys) > 0L) {
      display_keys <- gsub("\r", " at n=", duplicate_keys, fixed = TRUE)
      add_error(
        context,
        paste0("contains duplicate primary DV/checkpoint pair(s): ",
               hub_format_values(display_keys), ".")
      )
    }

    return(unique(primary_dv[nzchar(primary_dv)]))
  }

  character()
}

hub_study_inventory <- function(study_directory) {
  files <- list.files(
    study_directory,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    include.dirs = FALSE
  )
  if (length(files) == 0L) return(character())

  root <- normalizePath(study_directory, winslash = "/", mustWork = TRUE)
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  relative <- substring(files, nchar(prefix) + 1L)
  relative[tolower(relative) != "manifest.csv"]
}

validate_hub_data <- function(root = "hub-data", verbose = TRUE) {
  errors <- character()
  warnings <- character()
  studies_checked <- 0L
  files_checked <- 0L

  add_error <- function(context, message) {
    errors <<- c(errors, sprintf("[%s] %s", context, message))
  }
  add_warning <- function(context, message) {
    warnings <<- c(warnings, sprintf("[%s] %s", context, message))
  }

  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!dir.exists(root)) {
    add_error("hub-data", paste0("directory does not exist: ", root, "."))
    result <- list(
      valid = FALSE,
      errors = errors,
      warnings = warnings,
      studies = studies_checked,
      files = files_checked,
      root = root
    )
    if (verbose) hub_print_validation(result)
    return(result)
  }

  directory_path <- file.path(root, "directory.csv")
  if (!file.exists(directory_path)) {
    add_error("directory.csv", "file is missing.")
    result <- list(
      valid = FALSE,
      errors = errors,
      warnings = warnings,
      studies = studies_checked,
      files = files_checked,
      root = root
    )
    if (verbose) hub_print_validation(result)
    return(result)
  }

  directory <- tryCatch(
    hub_read_csv(directory_path),
    error = function(e) {
      add_error("directory.csv", paste0("could not be read: ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(directory)) {
    result <- list(
      valid = FALSE,
      errors = errors,
      warnings = warnings,
      studies = studies_checked,
      files = files_checked,
      root = root
    )
    if (verbose) hub_print_validation(result)
    return(result)
  }

  directory_schema_ok <- hub_check_columns(
    directory,
    hub_directory_columns,
    "directory.csv",
    add_error
  )
  if (!directory_schema_ok) {
    result <- list(
      valid = FALSE,
      errors = errors,
      warnings = warnings,
      studies = studies_checked,
      files = files_checked,
      root = root
    )
    if (verbose) hub_print_validation(result)
    return(result)
  }

  if (nrow(directory) == 0L) {
    add_error("directory.csv", "contains no studies.")
  }

  study_id <- hub_trim(directory$Study_ID)
  study_title <- hub_trim(directory$Study_Title)
  study_path <- hub_trim(directory$Study_Path)

  blank_id <- which(!nzchar(study_id))
  if (length(blank_id) > 0L) {
    add_error(
      "directory.csv",
      sprintf("Study_ID is blank on row(s): %s.", hub_format_values(blank_id))
    )
  }
  bad_id <- which(
    nzchar(study_id) & !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", study_id)
  )
  if (length(bad_id) > 0L) {
    add_error(
      "directory.csv",
      sprintf(
        "Study_ID is not a portable identifier on row(s): %s.",
        hub_format_values(bad_id)
      )
    )
  }
  blank_title <- which(!nzchar(study_title))
  if (length(blank_title) > 0L) {
    add_error(
      "directory.csv",
      sprintf(
        "Study_Title is blank on row(s): %s.",
        hub_format_values(blank_title)
      )
    )
  }

  duplicate_id <- unique(study_id[duplicated(tolower(study_id))])
  if (length(duplicate_id) > 0L) {
    add_error(
      "directory.csv",
      paste0("Study_ID is duplicated (case-insensitive): ",
             hub_format_values(duplicate_id), ".")
    )
  }
  duplicate_path <- unique(study_path[duplicated(tolower(study_path))])
  if (length(duplicate_path) > 0L) {
    add_error(
      "directory.csv",
      paste0("Study_Path is duplicated (case-insensitive): ",
             hub_format_values(duplicate_path), ".")
    )
  }

  safe_directory_rows <- logical(nrow(directory))
  for (i in seq_len(nrow(directory))) {
    context <- sprintf("directory.csv row %d", i)
    path_problem <- hub_safe_relative_path(study_path[i])
    if (!is.null(path_problem)) {
      add_error(context, paste0("Study_Path ", path_problem, "."))
      next
    }

    expected_path <- paste0("studies/", study_id[i])
    if (!identical(study_path[i], expected_path)) {
      add_error(
        context,
        sprintf("Study_Path must be '%s'.", expected_path)
      )
      next
    }
    safe_directory_rows[i] <- nzchar(study_id[i]) &&
      grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", study_id[i])
  }

  studies_directory <- file.path(root, "studies")
  if (!dir.exists(studies_directory)) {
    add_error("hub-data", "studies directory is missing.")
  } else {
    actual_study_directories <- list.dirs(
      studies_directory,
      full.names = FALSE,
      recursive = FALSE
    )
    expected_study_directories <- study_id[safe_directory_rows]
    orphan_study_directories <- actual_study_directories[
      !tolower(actual_study_directories) %in%
        tolower(expected_study_directories)
    ]
    if (length(orphan_study_directories) > 0L) {
      add_error(
        "hub-data/studies",
        paste0(
          "study directory/directories are absent from directory.csv: ",
          hub_format_values(orphan_study_directories),
          "."
        )
      )
    }
  }

  rows_to_check <- which(safe_directory_rows &
                           !duplicated(tolower(study_id)) &
                           !duplicated(tolower(study_path)))

  for (i in rows_to_check) {
    studies_checked <- studies_checked + 1L
    id <- study_id[i]
    relative_study_path <- study_path[i]
    study_directory <- hub_path(root, relative_study_path)
    context <- paste0("study ", id)

    if (!dir.exists(study_directory)) {
      add_error(context, paste0("directory is missing: ", relative_study_path, "."))
      next
    }

    manifest_path <- file.path(study_directory, "manifest.csv")
    if (!file.exists(manifest_path)) {
      add_error(context, "manifest.csv is missing.")
      next
    }

    manifest <- tryCatch(
      hub_read_csv(manifest_path),
      error = function(e) {
        add_error(context, paste0("manifest.csv could not be read: ",
                                  conditionMessage(e)))
        NULL
      }
    )
    if (is.null(manifest)) next

    manifest_context <- paste0(context, " manifest.csv")
    manifest_schema_ok <- hub_check_columns(
      manifest,
      hub_manifest_columns,
      manifest_context,
      add_error
    )
    if (!manifest_schema_ok) next

    if (nrow(manifest) == 0L) {
      add_error(manifest_context, "contains no files.")
      next
    }

    path <- hub_trim(manifest$path)
    role <- hub_trim(manifest$role)
    cohort <- hub_trim(manifest$cohort)
    display_name <- hub_trim(manifest$display_name)
    media_type <- hub_trim(manifest$media_type)
    bytes <- hub_trim(manifest$bytes)
    sha256 <- tolower(hub_trim(manifest$sha256))

    blank_path <- which(!nzchar(path))
    if (length(blank_path) > 0L) {
      add_error(
        manifest_context,
        sprintf("path is blank on row(s): %s.", hub_format_values(blank_path))
      )
    }

    path_is_safe <- logical(nrow(manifest))
    for (j in seq_len(nrow(manifest))) {
      path_problem <- hub_safe_relative_path(path[j])
      if (!is.null(path_problem)) {
        add_error(
          sprintf("%s row %d", manifest_context, j),
          paste0("path ", path_problem, ".")
        )
      } else if (tolower(path[j]) == "manifest.csv") {
        add_error(
          sprintf("%s row %d", manifest_context, j),
          "path must not point to manifest.csv itself."
        )
      } else {
        path_is_safe[j] <- TRUE
      }
    }

    duplicate_manifest_paths <- unique(path[duplicated(tolower(path))])
    if (length(duplicate_manifest_paths) > 0L) {
      add_error(
        manifest_context,
        paste0("path is duplicated (case-insensitive): ",
               hub_format_values(duplicate_manifest_paths), ".")
      )
    }

    bad_role <- which(!role %in% hub_roles)
    if (length(bad_role) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "role must be meta, trajectory, abstract, or material on row(s): %s.",
          hub_format_values(bad_role)
        )
      )
    }

    cohort_role <- role %in% c("meta", "trajectory")
    valid_cohort <- grepl("^[1-9][0-9]*$", cohort)
    bad_data_cohort <- which(cohort_role & !valid_cohort)
    if (length(bad_data_cohort) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "meta and trajectory rows need a positive integer cohort on row(s): %s.",
          hub_format_values(bad_data_cohort)
        )
      )
    }
    bad_material_cohort <- which(!cohort_role & nzchar(cohort))
    if (length(bad_material_cohort) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "abstract and material rows must leave cohort blank on row(s): %s.",
          hub_format_values(bad_material_cohort)
        )
      )
    }

    role_cohort_key <- paste(role[cohort_role & valid_cohort],
                             cohort[cohort_role & valid_cohort], sep = "\r")
    duplicate_role_cohort <- unique(
      role_cohort_key[duplicated(role_cohort_key)]
    )
    if (length(duplicate_role_cohort) > 0L) {
      display_keys <- gsub("\r", " cohort ", duplicate_role_cohort,
                           fixed = TRUE)
      add_error(
        manifest_context,
        paste0("contains more than one file for: ",
               hub_format_values(display_keys), ".")
      )
    }

    meta_cohort <- unique(cohort[role == "meta" & valid_cohort])
    trajectory_cohort <- unique(cohort[role == "trajectory" & valid_cohort])
    if (length(meta_cohort) == 0L) {
      add_error(manifest_context, "contains no meta file.")
    }
    if (sum(role == "abstract") > 1L) {
      add_error(manifest_context, "contains more than one abstract file.")
    }
    orphan_trajectory <- setdiff(trajectory_cohort, meta_cohort)
    if (length(orphan_trajectory) > 0L) {
      add_error(
        manifest_context,
        paste0("trajectory has no matching meta file for cohort(s): ",
               hub_format_values(orphan_trajectory), ".")
      )
    }

    bad_meta_name <- which(
      role == "meta" & !grepl("_META\\.csv$", path, ignore.case = TRUE)
    )
    bad_trajectory_name <- which(
      role == "trajectory" &
        !grepl("_trajectory\\.csv$", path, ignore.case = TRUE)
    )
    if (length(bad_meta_name) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "meta paths must end in _META.csv on row(s): %s.",
          hub_format_values(bad_meta_name)
        )
      )
    }
    if (length(bad_trajectory_name) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "trajectory paths must end in _trajectory.csv on row(s): %s.",
          hub_format_values(bad_trajectory_name)
        )
      )
    }

    blank_display_name <- which(!nzchar(display_name))
    if (length(blank_display_name) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "display_name is blank on row(s): %s.",
          hub_format_values(blank_display_name)
        )
      )
    }
    bad_media_type <- which(
      !grepl(
        "^[A-Za-z0-9!#$&^_.+-]+/[A-Za-z0-9!#$&^_.+-]+$",
        media_type
      )
    )
    if (length(bad_media_type) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "media_type is blank or invalid on row(s): %s.",
          hub_format_values(bad_media_type)
        )
      )
    }

    valid_bytes <- grepl("^[0-9]+$", bytes)
    bad_bytes <- which(!valid_bytes)
    if (length(bad_bytes) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "bytes must be a non-negative whole number on row(s): %s.",
          hub_format_values(bad_bytes)
        )
      )
    }
    byte_values <- suppressWarnings(as.numeric(bytes))
    data_rows <- role %in% c("meta", "trajectory")
    oversized_data_rows <- which(
      data_rows & valid_bytes & byte_values > hub_max_data_file_bytes
    )
    if (length(oversized_data_rows) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "meta and trajectory files may not exceed 50 MiB on row(s): %s.",
          hub_format_values(oversized_data_rows)
        )
      )
    }
    known_data_bytes <- sum(byte_values[data_rows & valid_bytes], na.rm = TRUE)
    if (known_data_bytes > hub_max_data_bundle_bytes) {
      add_error(
        manifest_context,
        paste0(
          "meta and trajectory files may not exceed 100 MiB in total; ",
          "the manifest lists ",
          format(known_data_bytes, scientific = FALSE, trim = TRUE),
          " bytes."
        )
      )
    }
    valid_sha256 <- grepl("^[0-9a-f]{64}$", sha256)
    bad_sha256 <- which(!valid_sha256)
    if (length(bad_sha256) > 0L) {
      add_error(
        manifest_context,
        sprintf(
          "sha256 must contain exactly 64 hexadecimal characters on row(s): %s.",
          hub_format_values(bad_sha256)
        )
      )
    }

    inventory <- hub_study_inventory(study_directory)
    inventory_lower <- tolower(inventory)
    listed_lower <- tolower(path[path_is_safe])

    unlisted <- inventory[!inventory_lower %in% listed_lower]
    if (length(unlisted) > 0L) {
      add_error(
        manifest_context,
        paste0("file(s) are not listed: ", hub_format_values(unlisted), ".")
      )
    }

    listed_but_missing <- path[path_is_safe &
                                  !tolower(path) %in% inventory_lower]
    if (length(listed_but_missing) > 0L) {
      add_error(
        manifest_context,
        paste0("listed file(s) do not exist: ",
               hub_format_values(listed_but_missing), ".")
      )
    }

    case_mismatch <- character()
    for (listed_path in path[path_is_safe]) {
      match_index <- match(tolower(listed_path), inventory_lower)
      if (!is.na(match_index) && !identical(listed_path, inventory[match_index])) {
        case_mismatch <- c(
          case_mismatch,
          sprintf("%s (disk: %s)", listed_path, inventory[match_index])
        )
      }
    }
    if (length(case_mismatch) > 0L) {
      add_error(
        manifest_context,
        paste0("path case does not match the file system: ",
               hub_format_values(case_mismatch), ".")
      )
    }

    meta_dv <- list()
    trajectory_dv <- list()

    for (j in seq_len(nrow(manifest))) {
      if (!path_is_safe[j] || duplicated(tolower(path))[j]) next

      file_path <- hub_path(study_directory, path[j])
      if (!file.exists(file_path) || dir.exists(file_path)) next

      files_checked <- files_checked + 1L
      row_context <- sprintf("%s/%s", relative_study_path, path[j])

      if (valid_bytes[j]) {
        expected_bytes <- suppressWarnings(as.numeric(bytes[j]))
        actual_bytes <- file.info(file_path)$size
        if (!isTRUE(all.equal(expected_bytes, actual_bytes, tolerance = 0))) {
          add_error(
            row_context,
            sprintf(
              "bytes is %s; the file contains %s bytes.",
              bytes[j],
              format(actual_bytes, scientific = FALSE, trim = TRUE)
            )
          )
        }
      }

      if (valid_sha256[j]) {
        actual_sha256 <- tryCatch(
          hub_sha256(file_path),
          error = function(e) {
            add_error(row_context, conditionMessage(e))
            NA_character_
          }
        )
        if (!is.na(actual_sha256) && !identical(sha256[j], actual_sha256)) {
          add_error(
            row_context,
            paste0("sha256 does not match the file (actual: ",
                   actual_sha256, ").")
          )
        }
      }

      if (!role[j] %in% c("meta", "trajectory")) next

      data <- tryCatch(
        hub_read_csv(file_path),
        error = function(e) {
          add_error(row_context, paste0("could not be read as CSV: ",
                                        conditionMessage(e)))
          NULL
        }
      )
      if (is.null(data)) next

      if (role[j] == "meta") {
        meta_dv[[cohort[j]]] <- hub_validate_meta(data, row_context, add_error)
      } else {
        trajectory_dv[[cohort[j]]] <- hub_validate_trajectory(
          data,
          row_context,
          add_error
        )
      }
    }

    shared_cohorts <- intersect(names(meta_dv), names(trajectory_dv))
    for (cohort_value in shared_cohorts) {
      meta_values <- meta_dv[[cohort_value]]
      trajectory_values <- trajectory_dv[[cohort_value]]
      if (length(meta_values) == 0L || length(trajectory_values) == 0L) next

      missing_from_trajectory <- setdiff(meta_values, trajectory_values)
      extra_in_trajectory <- setdiff(trajectory_values, meta_values)
      if (length(missing_from_trajectory) > 0L ||
          length(extra_in_trajectory) > 0L) {
        add_error(
          context,
          paste0(
            "DV values differ between meta and trajectory files for cohort ",
            cohort_value, "."
          )
        )
      }
    }
  }

  result <- list(
    valid = length(errors) == 0L,
    errors = errors,
    warnings = warnings,
    studies = studies_checked,
    files = files_checked,
    root = root
  )
  if (verbose) hub_print_validation(result)
  result
}

hub_print_validation <- function(result) {
  if (length(result$errors) > 0L) {
    cat(paste0("ERROR ", result$errors, "\n"), sep = "")
  }
  if (length(result$warnings) > 0L) {
    cat(paste0("WARNING ", result$warnings, "\n"), sep = "")
  }

  if (isTRUE(result$valid)) {
    cat(sprintf(
      "OK Validated %d study/studies and %d listed file(s) in %s.\n",
      result$studies,
      result$files,
      result$root
    ))
  } else {
    cat(sprintf(
      "FAILED Found %d error(s) while checking %d study/studies and %d listed file(s).\n",
      length(result$errors),
      result$studies,
      result$files
    ))
  }

  invisible(result)
}

hub_validation_usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript scripts/validate_hub_data.R [data-repository-directory]\n\n",
    "If no directory is supplied, the script checks the current directory.\n",
    sep = ""
  )
}

hub_validation_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (any(args %in% c("-h", "--help"))) {
    hub_validation_usage()
    return(0L)
  }
  if (length(args) > 1L) {
    hub_validation_usage()
    cat("\nExpected zero or one positional argument.\n")
    return(2L)
  }

  root <- if (length(args) == 0L) "." else args[1]
  result <- validate_hub_data(root, verbose = TRUE)
  if (isTRUE(result$valid)) 0L else 1L
}

if (sys.nframe() == 0L) {
  quit(status = hub_validation_cli(), save = "no")
}
