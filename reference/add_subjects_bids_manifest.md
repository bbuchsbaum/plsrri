# Add Subjects from BIDS as a Lagged NIfTI Manifest

Builds a manifest (subject × condition rows) from a BIDS dataset using
`bidser`, then routes through
[`add_subjects_manifest()`](https://bbuchsbaum.github.io/plsrri/reference/add_subjects_manifest.md)
so that **NIfTI dim4** is interpreted as **lags** and folded as
voxel×lag features.

This is intended for event-related/fIR-style derivatives where each file
is a 4D NIfTI whose 4th dimension corresponds to lags (time since event
onset).

## Usage

``` r
add_subjects_bids_manifest(
  spec,
  bids_dir,
  groups = NULL,
  group_col = NULL,
  task = NULL,
  space = NULL,
  file_regex = ".*\\.nii(\\.gz)?$",
  condition_keys = "cond",
  volumes = NULL,
  mask_method = "intersection",
  strict = TRUE,
  ...
)
```

## Arguments

- spec:

  A `pls_spec` object.

- bids_dir:

  Path to BIDS dataset root.

- groups:

  NULL/"all" or character vector of group labels found in
  `participants.tsv` (same semantics as
  [`add_subjects()`](https://bbuchsbaum.github.io/plsrri/reference/add_subjects.md)).

- group_col:

  Optional grouping column name in `participants.tsv`. When provided,
  `groups` are interpreted as values from this column.

- task:

  Optional task filter passed to `bidser::search_files()`.

- space:

  Optional space filter passed to `bidser::search_files()` and
  `bidser::mask_files()`.

- file_regex:

  Regex applied to filenames to find candidate NIfTI files. Defaults to
  any NIfTI.

- condition_keys:

  Character vector of BIDS entity keys to use to define conditions.
  Values are extracted from filenames using patterns like
  `_<key>-<value>` and joined with `_`.

- volumes:

  Optional integer vector (1-based) or compact spec string (e.g.,
  `"1:8"`) selecting a subset of dim4 volumes (lags). When provided, the
  manifest `file` column uses `file[volspec]` syntax.

- mask_method:

  How to derive a brain mask if `spec$mask` is NULL: `"intersection"`
  (default), `"union"`, or `"first"`.

- strict:

  Logical. If TRUE (default), error on missing condition keys or
  duplicated (group, subject, condition) rows.

- ...:

  Additional filters forwarded to `bidser::search_files()` and
  `bidser::mask_files()`.

## Value

Updated `pls_spec` object with manifest-based inputs.
