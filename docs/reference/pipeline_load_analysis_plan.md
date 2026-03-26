# Load an Analysis Plan from Attached Outputs

Reads first-level manifests from a pipeline output root, filters by
output type and statistic, and returns a validated analysis plan that
can be passed to
[`pipeline_build_pls_spec_from_ui`](https://bbuchsbaum.github.io/plsrri/reference/pipeline_build_pls_spec_from_ui.md).

## Usage

``` r
pipeline_load_analysis_plan(
  root,
  analysis = "pls",
  input_type = NULL,
  statistic = NULL,
  remap = NULL
)
```

## Arguments

- root:

  Path to the pipeline output root directory.

- analysis:

  Analysis type (currently only `"pls"`).

- input_type:

  First-level output type to use (e.g., `"estimates"`). Auto-detected
  from the most common value if `NULL`.

- statistic:

  First-level statistic to use (e.g., `"estimate"`). Auto-detected from
  the most common value if `NULL`.

- remap:

  Optional alternative root for resolving relative paths.

## Value

A validated analysis plan list suitable for
[`pipeline_build_pls_spec_from_ui`](https://bbuchsbaum.github.io/plsrri/reference/pipeline_build_pls_spec_from_ui.md).
