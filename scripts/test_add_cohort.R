#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run this test with Rscript.")
script_path <- sub("^--file=", "", script_argument)
script_directory <- dirname(normalizePath(script_path, mustWork = TRUE))
source(file.path(script_directory, "add_cohort.R"))

validator_path <- file.path(script_directory, "validate_hub_data.R")
validator <- add_cohort_load_validator(
  dirname(script_directory),
  validator_path = validator_path
)

write_fixture_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
}

valid_meta <- function(group, final_e = "2.5") {
  data.frame(
    Group = group,
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
}

valid_trajectory <- function(final_e = 2.5) {
  data.frame(
    DV = c("score", "score"),
    Source = c("safe_t_test", "safe_t_test"),
    n = c("0", "40"),
    log_e = c("0", format(log(final_e), digits = 16)),
    stringsAsFactors = FALSE
  )
}

make_hub_fixture <- function(root) {
  study_directory <- file.path(root, "studies", "demo-study")
  upload_directory <- file.path(study_directory, "cohort_uploads")
  dir.create(upload_directory, recursive = TRUE, showWarnings = FALSE)

  existing_meta <- file.path(upload_directory, "Cohort_1_META.csv")
  write_fixture_csv(valid_meta("Cohort 1"), existing_meta)

  manifest <- data.frame(
    path = "cohort_uploads/Cohort_1_META.csv",
    role = "meta",
    cohort = "1",
    display_name = "Cohort 1 metadata",
    media_type = "text/csv",
    bytes = file.info(existing_meta)$size,
    sha256 = validator$hub_sha256(existing_meta),
    stringsAsFactors = FALSE
  )
  write_fixture_csv(manifest, file.path(study_directory, "manifest.csv"))

  directory <- data.frame(
    Study_ID = "demo-study",
    Study_Title = "Demo study",
    Study_Path = "studies/demo-study",
    Abstract = "Fixture study.",
    DOI = "",
    stringsAsFactors = FALSE
  )
  write_fixture_csv(directory, file.path(root, "directory.csv"))
  invisible(study_directory)
}

expect_error_message <- function(expression, pattern) {
  message <- tryCatch(
    {
      force(expression)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
  stopifnot(!is.na(message), grepl(pattern, message, fixed = TRUE))
  invisible(message)
}

test_root <- tempfile("living-evidence-add-cohort-")
dir.create(test_root)
on.exit(unlink(test_root, recursive = TRUE, force = TRUE), add = TRUE)

hub_root <- file.path(test_root, "hub-data")
study_directory <- make_hub_fixture(hub_root)
incoming_directory <- file.path(test_root, "incoming")
dir.create(incoming_directory)

initial_validation <- validator$validate_hub_data(hub_root, verbose = FALSE)
stopifnot(isTRUE(initial_validation$valid))

# Successful paired META/trajectory addition.
meta_2 <- file.path(incoming_directory, "Cohort_2_META.csv")
trajectory_2 <- file.path(incoming_directory, "Cohort_2_trajectory.csv")
write_fixture_csv(valid_meta("Cohort 2"), meta_2)
write_fixture_csv(valid_trajectory(), trajectory_2)
meta_2_raw <- add_cohort_read_raw(meta_2)
trajectory_2_raw <- add_cohort_read_raw(trajectory_2)
manifest_path <- file.path(study_directory, "manifest.csv")
manifest_before_success <- add_cohort_read_raw(manifest_path)

success <- add_cohort(
  "demo-study",
  "2",
  meta_2,
  trajectory_2,
  hub_root = hub_root,
  validator_path = validator_path,
  quiet = TRUE
)
stopifnot(success$cohort == 2L, length(success$files) == 2L)

destination_meta_2 <- file.path(
  study_directory,
  "cohort_uploads",
  basename(meta_2)
)
destination_trajectory_2 <- file.path(
  study_directory,
  "cohort_uploads",
  basename(trajectory_2)
)
stopifnot(
  identical(meta_2_raw, add_cohort_read_raw(destination_meta_2)),
  identical(trajectory_2_raw, add_cohort_read_raw(destination_trajectory_2))
)

manifest_after_success <- add_cohort_read_raw(manifest_path)
stopifnot(
  length(manifest_after_success) > length(manifest_before_success),
  identical(
    manifest_after_success[seq_along(manifest_before_success)],
    manifest_before_success
  )
)
manifest <- validator$hub_read_csv(manifest_path)
new_rows <- manifest[manifest$cohort == "2", , drop = FALSE]
stopifnot(
  nrow(new_rows) == 2L,
  identical(new_rows$role, c("meta", "trajectory")),
  identical(new_rows$media_type, c("text/csv", "text/csv")),
  all(nzchar(new_rows$display_name)),
  identical(
    as.numeric(new_rows$bytes),
    c(file.info(meta_2)$size, file.info(trajectory_2)$size)
  ),
  identical(
    new_rows$sha256,
    c(validator$hub_sha256(meta_2), validator$hub_sha256(trajectory_2))
  )
)
stopifnot(isTRUE(
  validator$validate_hub_data(hub_root, verbose = FALSE)$valid
))

# A second meta file for the same cohort is rejected before mutation.
alternative_meta_2 <- file.path(incoming_directory, "Alternative_META.csv")
write_fixture_csv(valid_meta("Alternative cohort 2"), alternative_meta_2)
manifest_before_duplicate <- add_cohort_read_raw(manifest_path)
expect_error_message(
  add_cohort(
    "demo-study",
    "2",
    alternative_meta_2,
    hub_root = hub_root,
    validator_path = validator_path,
    quiet = TRUE
  ),
  "role/cohort"
)
stopifnot(
  identical(manifest_before_duplicate, add_cohort_read_raw(manifest_path)),
  !file.exists(file.path(
    study_directory,
    "cohort_uploads",
    basename(alternative_meta_2)
  ))
)

# A path already in the manifest is rejected; no overwrite occurs.
replacement_meta_2 <- file.path(test_root, "replacement", "Cohort_2_META.csv")
write_fixture_csv(valid_meta("Replacement cohort 2", final_e = "3.0"),
                  replacement_meta_2)
destination_before_overwrite <- add_cohort_read_raw(destination_meta_2)
expect_error_message(
  add_cohort(
    "demo-study",
    "3",
    replacement_meta_2,
    hub_root = hub_root,
    validator_path = validator_path,
    quiet = TRUE
  ),
  "already contains path"
)
stopifnot(identical(
  destination_before_overwrite,
  add_cohort_read_raw(destination_meta_2)
))

# Invalid new data reaches whole-repository validation, then rolls back.
invalid_meta_3 <- file.path(incoming_directory, "Cohort_3_META.csv")
bad_meta <- valid_meta("Cohort 3")
bad_meta$Status <- NULL
write_fixture_csv(bad_meta, invalid_meta_3)
manifest_before_rollback <- add_cohort_read_raw(manifest_path)
invalid_destination <- file.path(
  study_directory,
  "cohort_uploads",
  basename(invalid_meta_3)
)
expect_error_message(
  add_cohort(
    "demo-study",
    "3",
    invalid_meta_3,
    hub_root = hub_root,
    validator_path = validator_path,
    quiet = TRUE
  ),
  "rolled back"
)
stopifnot(
  identical(manifest_before_rollback, add_cohort_read_raw(manifest_path)),
  !file.exists(invalid_destination),
  file.exists(invalid_meta_3),
  isTRUE(validator$validate_hub_data(hub_root, verbose = FALSE)$valid)
)

# The trajectory is optional.
meta_4 <- file.path(incoming_directory, "Cohort_4_META.csv")
write_fixture_csv(valid_meta("Cohort 4"), meta_4)
meta_only <- add_cohort(
  "demo-study",
  "4",
  meta_4,
  hub_root = hub_root,
  validator_path = validator_path,
  quiet = TRUE
)
stopifnot(length(meta_only$files) == 1L)
manifest <- validator$hub_read_csv(manifest_path)
stopifnot(
  sum(manifest$role == "meta" & manifest$cohort == "4") == 1L,
  sum(manifest$role == "trajectory" & manifest$cohort == "4") == 0L,
  isTRUE(validator$validate_hub_data(hub_root, verbose = FALSE)$valid)
)

# The CLI parser accepts the documented three-argument META-only form.
meta_5 <- file.path(incoming_directory, "Cohort_5_META.csv")
write_fixture_csv(valid_meta("Cohort 5"), meta_5)
cli_output <- capture.output(
  cli_status <- add_cohort_cli(
    c("demo-study", "5", meta_5),
    hub_root = hub_root,
    validator_path = validator_path
  )
)
stopifnot(
  cli_status == 0L,
  any(grepl("Added cohort 5", cli_output, fixed = TRUE)),
  isTRUE(validator$validate_hub_data(hub_root, verbose = FALSE)$valid)
)

# Invalid identifiers and cohorts fail before any mutation.
manifest_before_bad_arguments <- add_cohort_read_raw(manifest_path)
expect_error_message(
  add_cohort(
    "../demo-study",
    "5",
    meta_4,
    hub_root = hub_root,
    validator_path = validator_path,
    quiet = TRUE
  ),
  "STUDY_ID"
)
expect_error_message(
  add_cohort(
    "demo-study",
    "0",
    meta_4,
    hub_root = hub_root,
    validator_path = validator_path,
    quiet = TRUE
  ),
  "positive integer"
)
stopifnot(identical(
  manifest_before_bad_arguments,
  add_cohort_read_raw(manifest_path)
))

# Oversized files are rejected before copying or hashing.
oversized_meta <- file.path(incoming_directory, "Oversized_META.csv")
oversized_connection <- file(oversized_meta, open = "wb")
invisible(seek(
  oversized_connection,
  where = validator$hub_max_data_file_bytes,
  origin = "start"
))
writeBin(as.raw(0), oversized_connection)
close(oversized_connection)
manifest_before_oversized <- add_cohort_read_raw(manifest_path)
expect_error_message(
  add_cohort(
    "demo-study",
    "6",
    oversized_meta,
    hub_root = hub_root,
    validator_path = validator_path,
    quiet = TRUE
  ),
  "may not exceed 50 MiB"
)
stopifnot(
  identical(manifest_before_oversized, add_cohort_read_raw(manifest_path)),
  !file.exists(file.path(
    study_directory,
    "cohort_uploads",
    basename(oversized_meta)
  ))
)

cat("OK add_cohort self-tests passed.\n")
