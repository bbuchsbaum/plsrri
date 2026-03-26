# Build a PLS Specification from UI Options

Converts an analysis plan (from
[`pipeline_load_analysis_plan`](https://bbuchsbaum.github.io/plsrri/reference/pipeline_load_analysis_plan.md))
and user-selected PLS options into a configured
[`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md)
ready for
[`run()`](https://bbuchsbaum.github.io/plsrri/reference/run.md).

## Usage

``` r
pipeline_build_pls_spec_from_ui(plan, pls_options)
```

## Arguments

- plan:

  An analysis plan as returned by
  [`pipeline_load_analysis_plan`](https://bbuchsbaum.github.io/plsrri/reference/pipeline_load_analysis_plan.md).

- pls_options:

  A named list of PLS configuration. Recognised elements:

  method

  :   PLS method name or integer (e.g., `"task"`, `"behavior"`, `1L`).

  nperm

  :   Number of permutations (integer).

  nboot

  :   Number of bootstrap samples (integer).

  nsplit

  :   Number of split-half iterations (integer).

  clim

  :   Confidence level (numeric, 0–100).

  meancentering

  :   Mean-centering type (integer or string).

  cormode

  :   Correlation mode (integer or string).

  boot_type

  :   Bootstrap type (`"strat"` or `"nonstrat"`).

  is_struct

  :   Logical; structure PLS flag.

  behavior_data

  :   Optional behaviour matrix (rows = observations).

  behavior_measures

  :   Optional column names for `behavior_data`.

  block_conditions

  :   Optional block conditions for behaviour PLS.

  design_matrix

  :   Optional contrast/design matrix for non-rotated methods.

  design_labels

  :   Optional labels for `design_matrix` columns.

## Value

A configured
[`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md)
object.
