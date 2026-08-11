# Living Evidence data

This repository is the public data source for the Living Evidence Hub. It was migrated from OSF on 11 August 2026. The existing five-character study IDs remain stable.

## Structure

- `directory.csv` lists every study. `Study_Path` points to `studies/<Study_ID>`.
- Each study has a `manifest.csv`.
- `cohort_uploads/` contains paired `*_META.csv` and `*_trajectory.csv` files.
- `materials/` contains abstracts and study materials.

Manifest paths are relative to the study directory. Each row records `path`, `role`, `cohort`, `display_name`, `media_type`, `bytes`, and `sha256`. Use roles `meta`, `trajectory`, `abstract`, or `material`. Cohort numbers are positive integers for meta and trajectory files and blank for other files.

## Add a cohort

The helper script is the recommended workflow. It copies the exported CSVs without changing their bytes, calculates their byte counts and SHA-256 hashes, updates the study manifest, and validates the complete repository. It restores the original manifest and removes copied files if validation fails.

1. Clone the repository and create a branch for the study and cohort.
2. Export the cohort's `*_META.csv` and optional `*_trajectory.csv` from the app.
3. Open a terminal in the repository root, where `directory.csv` is located.
4. Run one of these commands:

```text
Rscript scripts/add_cohort.R STUDY_ID COHORT path/to/Cohort_META.csv
Rscript scripts/add_cohort.R STUDY_ID COHORT path/to/Cohort_META.csv path/to/Cohort_trajectory.csv
```

For example:

```text
Rscript scripts/add_cohort.R ezdy6 2 exports/Study2_META.csv exports/Study2_trajectory.csv
```

If `Rscript` is not available in a Windows terminal, open the repository as an
RStudio project and run this in the R console:

```r
source("scripts/add_cohort.R")
add_cohort(
  study_id = "ezdy6",
  cohort = 2,
  meta_csv = "exports/Study2_META.csv",
  trajectory_csv = "exports/Study2_trajectory.csv"
)
```

5. Review the added files and manifest rows.
6. Commit the changes, publish the branch, and open a pull request.

The helper refuses to overwrite a file or reuse a role and cohort number. The trajectory file is optional because the Hub can display a cohort-level summary from the META file alone. Run one copy of the helper at a time. META and trajectory files are limited to 50 MiB each and 100 MiB in total for one study, matching the app.

Run the validator independently at any time:

```text
Rscript scripts/validate_hub_data.R .
```

## Upload a cohort on the GitHub website

1. Export the cohort's `*_META.csv` and `*_trajectory.csv` files from the app.
2. Open the study's `cohort_uploads/` folder on GitHub.
3. Select **Add file → Upload files** and create a new branch when prompted.
4. Add one manifest row for each uploaded file. Use the same positive cohort number for the pair.
5. Open a pull request. Ask a maintainer if you need help calculating `bytes` or `sha256`.

## Upload a cohort with GitHub Desktop

1. Clone the repository and create a branch named for the study and cohort.
2. In GitHub Desktop, select **Repository -> Open in Terminal**.
3. Run `scripts/add_cohort.R` using the commands above.
4. Review the changes in GitHub Desktop.
5. Commit the files and manifest together, publish the branch, and open a pull request.

In PowerShell, this prints the required byte count and lowercase SHA-256 value:

```powershell
$file = Get-Item -LiteralPath "path/to/file.csv"
$hash = (Get-FileHash -LiteralPath $file.FullName -Algorithm SHA256).Hash.ToLowerInvariant()
"$($file.Length),$hash"
```

Do not reuse a cohort number within a study. Do not rename a study directory or change its `Study_ID`.
