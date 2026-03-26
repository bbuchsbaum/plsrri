# Add Subjects from a Map Manifest

Adds subject data to a PLS specification from a manifest where each
observation references a single 3D map. This is intended for first-level
GLM estimate maps such as condition betas or contrast statistics.

## Usage

``` r
add_subjects_map_manifest(
  spec,
  manifest,
  mask,
  subject_col = NULL,
  condition_col = NULL,
  file_col = NULL,
  group_col = NULL,
  volume_col = NULL,
  base_dir = NULL
)
```

## Arguments

- spec:

  A `pls_spec` object.

- manifest:

  A data.frame or a file path (.csv/.tsv/.rds).

- mask:

  Brain mask (NeuroVol or file path). Required for image ingestion.

- subject_col, condition_col, file_col:

  Column names (or NULL to use defaults).

- group_col:

  Optional group column (default looks for "group").

- volume_col:

  Optional single-volume selector for 4D inputs.

- base_dir:

  Optional base directory for relative file paths.

## Value

Updated `pls_spec` object.
