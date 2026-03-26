# Summarize a Pipeline Output Root

Reads and validates a CLI pipeline output directory, returning a
structured summary of the first-level outputs found there.

## Usage

``` r
pipeline_attach_summary(root, remap = NULL)
```

## Arguments

- root:

  Path to the pipeline output root directory (the folder containing
  `firstlevel/`, `pls/`, etc.).

- remap:

  Optional alternative root for resolving relative paths. Use when the
  output directory has been moved from its original location. If `NULL`
  (default), paths are resolved against `root`.

## Value

A list with components:

- root:

  The output root path.

- valid:

  Logical; `TRUE` if the output root is usable.

- errors:

  Character vector of validation errors (empty if valid).

- firstlevel_plan:

  First-level work plan data.frame.

- firstlevel_manifest:

  Consolidated first-level manifest data.frame.

- discovery_manifest:

  Discovery manifest data.frame, or `NULL`.

- summary:

  Named list with `n_subjects`, `n_groups`, `groups`, `types`,
  `statistics`, etc.
