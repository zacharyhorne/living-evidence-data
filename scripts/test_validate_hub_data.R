#!/usr/bin/env Rscript

# Self-tests for the standalone Living Evidence data-repository validator.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run this test with Rscript.")
}
script_path <- sub("^--file=", "", script_argument)
script_directory <- dirname(normalizePath(script_path, mustWork = TRUE))
source(file.path(script_directory, "validate_hub_data.R"))

write_fixture_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
}

make_fixture <- function(root, duplicate_path = FALSE,
                         final_e = "2.5", final_log_e = NULL) {
  study_directory <- file.path(root, "studies", "demo-study")
  cohort_directory <- file.path(study_directory, "cohort_uploads")
  materials_directory <- file.path(study_directory, "materials")
  dir.create(cohort_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(materials_directory, recursive = TRUE, showWarnings = FALSE)

  meta_path <- file.path(cohort_directory, "Cohort_1_META.csv")
  trajectory_path <- file.path(cohort_directory, "Cohort_1_trajectory.csv")
  abstract_path <- file.path(materials_directory, "abstract.txt")

  meta_data <- data.frame(
    Group = "Cohort 1",
    DV = "score",
    Status = "Complete",
    Test_Type = "safe_twoSample",
    Prereg_Delta = "0.30",
    Prereg_Dir = "greater",
    Final_E = final_e,
    Total_N = "40",
    Timestamp = "2026-08-11 12:00",
    stringsAsFactors = FALSE
  )
  if (!is.null(final_log_e)) meta_data$Final_Log_E <- final_log_e
  write_fixture_csv(meta_data, meta_path)
  write_fixture_csv(
    data.frame(
      DV = c("score", "score"),
      Source = c("safe_t_test", "safe_t_test"),
      n = c("0", "40"),
      log_e = c("0", log(2.5)),
      stringsAsFactors = FALSE
    ),
    trajectory_path
  )
  writeLines("Fixture abstract.", abstract_path, useBytes = TRUE)

  relative_paths <- c(
    "cohort_uploads/Cohort_1_META.csv",
    "cohort_uploads/Cohort_1_trajectory.csv",
    "materials/abstract.txt"
  )
  roles <- c("meta", "trajectory", "abstract")
  cohorts <- c("1", "1", "")
  display_names <- c("Cohort 1 metadata", "Cohort 1 trajectory", "Abstract")
  media_types <- c("text/csv", "text/csv", "text/plain")
  full_paths <- vapply(
    relative_paths,
    function(path) hub_path(study_directory, path),
    character(1)
  )

  manifest <- data.frame(
    path = relative_paths,
    role = roles,
    cohort = cohorts,
    display_name = display_names,
    media_type = media_types,
    bytes = vapply(full_paths, function(path) file.info(path)$size, numeric(1)),
    sha256 = vapply(full_paths, hub_sha256, character(1)),
    stringsAsFactors = FALSE
  )
  if (duplicate_path) manifest <- rbind(manifest, manifest[1, , drop = FALSE])
  write_fixture_csv(manifest, file.path(study_directory, "manifest.csv"))

  directory <- data.frame(
    Study_ID = "demo-study",
    Study_Title = "Demo study",
    Study_Path = "studies/demo-study",
    Abstract = "Fixture abstract.",
    DOI = "https://doi.org/10.0000/example",
    stringsAsFactors = FALSE
  )
  write_fixture_csv(directory, file.path(root, "directory.csv"))

  invisible(list(
    trajectory_path = trajectory_path,
    manifest_path = file.path(study_directory, "manifest.csv")
  ))
}

test_root <- tempfile("living-evidence-validator-")
dir.create(test_root)
on.exit(unlink(test_root, recursive = TRUE, force = TRUE), add = TRUE)

valid_root <- file.path(test_root, "valid", "hub-data")
fixture <- make_fixture(valid_root)
valid_result <- validate_hub_data(valid_root, verbose = FALSE)
stopifnot(isTRUE(valid_result$valid))

cat("tampered\n", file = fixture$trajectory_path, append = TRUE)
tampered_result <- validate_hub_data(valid_root, verbose = FALSE)
stopifnot(!tampered_result$valid)
stopifnot(any(grepl("sha256 does not match", tampered_result$errors,
                   fixed = TRUE)))

duplicate_root <- file.path(test_root, "duplicate", "hub-data")
make_fixture(duplicate_root, duplicate_path = TRUE)
duplicate_result <- validate_hub_data(duplicate_root, verbose = FALSE)
stopifnot(!duplicate_result$valid)
stopifnot(any(grepl("path is duplicated", duplicate_result$errors,
                   fixed = TRUE)))

underflow_root <- file.path(test_root, "underflow", "hub-data")
make_fixture(underflow_root, final_e = "0", final_log_e = "-1000")
underflow_result <- validate_hub_data(underflow_root, verbose = FALSE)
stopifnot(isTRUE(underflow_result$valid))

file_limit_root <- file.path(test_root, "file-limit", "hub-data")
file_limit_fixture <- make_fixture(file_limit_root)
file_limit_manifest <- hub_read_csv(file_limit_fixture$manifest_path)
file_limit_manifest$bytes[file_limit_manifest$role == "meta"] <-
  as.character(hub_max_data_file_bytes + 1)
write_fixture_csv(file_limit_manifest, file_limit_fixture$manifest_path)
file_limit_result <- validate_hub_data(file_limit_root, verbose = FALSE)
stopifnot(!file_limit_result$valid)
stopifnot(any(grepl("may not exceed 50 MiB", file_limit_result$errors,
                   fixed = TRUE)))

bundle_limit_root <- file.path(test_root, "bundle-limit", "hub-data")
bundle_limit_fixture <- make_fixture(bundle_limit_root)
bundle_limit_manifest <- hub_read_csv(bundle_limit_fixture$manifest_path)
data_rows <- bundle_limit_manifest$role %in% c("meta", "trajectory")
bundle_limit_manifest$bytes[data_rows] <- as.character(40 * 1024^2)
extra_meta <- bundle_limit_manifest[bundle_limit_manifest$role == "meta", ,
                                    drop = FALSE]
extra_meta$path <- "cohort_uploads/Cohort_2_META.csv"
extra_meta$cohort <- "2"
extra_meta$bytes <- as.character(40 * 1024^2)
bundle_limit_manifest <- rbind(bundle_limit_manifest, extra_meta)
write_fixture_csv(bundle_limit_manifest, bundle_limit_fixture$manifest_path)
bundle_limit_result <- validate_hub_data(bundle_limit_root, verbose = FALSE)
stopifnot(!bundle_limit_result$valid)
stopifnot(any(grepl("may not exceed 100 MiB", bundle_limit_result$errors,
                   fixed = TRUE)))

cat("OK Validator self-tests passed.\n")
