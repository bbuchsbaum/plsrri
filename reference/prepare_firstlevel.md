# Prepare First-Level Artifacts

Prepare First-Level Artifacts

## Usage

``` r
prepare_firstlevel(spec, work_id = NULL, run = TRUE, summarize = TRUE)
```

## Arguments

- spec:

  Path to a pipeline YAML file or a specification list.

- work_id:

  Optional work-unit identifier for array/HPC execution.

- run:

  Logical; if `TRUE` (default), execute first-level estimation after
  planning. If `FALSE`, only validate, discover, and write the work
  plan.

- summarize:

  Logical; if `TRUE` (default), refresh `pipeline_summary.tsv` after
  preparation.

## Value

A `plsrri_firstlevel_prep` object containing the validated
specification, artifact root, discovery manifest, work plan, and
optional first-level output manifest.
