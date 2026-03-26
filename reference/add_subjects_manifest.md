# Add Subjects from a Lagged NIfTI Manifest

Adds subject data to a PLS specification from a "manifest" describing
subject × condition observations and the NIfTI files used for each.

The canonical (and recommended) format is one row per observation
(subject × condition), where `file` points to a 4D NIfTI and dim4
encodes lags (time since onset). Optionally, a subset of lags can be
selected via `file[1:8]` syntax or a separate `volume` column.

A second supported format is "one row per lag": provide multiple rows
with the same (subject, condition) key, plus a `lag` column (0-based) or
`volume` column; each row supplies a single 3D file or a single selected
volume from a 4D file.

## Usage

``` r
add_subjects_manifest(
  spec,
  manifest,
  mask,
  subject_col = NULL,
  condition_col = NULL,
  file_col = NULL,
  group_col = NULL,
  lag_col = NULL,
  volume_col = NULL,
  base_dir = NULL
)
```

## Arguments

- spec:

  A `pls_spec` object

- manifest:

  A data.frame or a file path (.csv/.tsv/.rds)

- mask:

  Brain mask (NeuroVol or file path). Required for NIfTI ingestion.

- subject_col, condition_col, file_col:

  Column names (or NULL to use defaults)

- group_col:

  Optional group column (default looks for "group")

- lag_col:

  Optional lag column (default looks for "lag")

- volume_col:

  Optional volume selector column (default looks for "volume")

- base_dir:

  Optional base directory for relative file paths

## Value

Updated `pls_spec` object
