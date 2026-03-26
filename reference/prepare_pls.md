# Prepare a Runnable PLS Specification

Prepare a Runnable PLS Specification

## Usage

``` r
prepare_pls(
  x,
  pls_options = NULL,
  input_type = NULL,
  statistic = NULL,
  remap = NULL
)
```

## Arguments

- x:

  One of:

  - a pipeline YAML file or specification list

  - a `plsrri_firstlevel_prep` object

  - an attached artifact root directory

  - an analysis plan from
    [`pipeline_load_analysis_plan`](https://bbuchsbaum.github.io/plsrri/reference/pipeline_load_analysis_plan.md)

- pls_options:

  Optional named list of PLS overrides. For pipeline-spec inputs these
  override the saved PLS configuration. For artifact-root or
  analysis-plan inputs they define the PLS configuration.

- input_type:

  Optional first-level output type for artifact-root inputs.

- statistic:

  Optional first-level statistic for artifact-root inputs.

- remap:

  Optional alternative root for resolving moved relative paths when `x`
  is an artifact root.

## Value

A configured
[`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md)
object.
