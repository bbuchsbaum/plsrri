# Render a PLS Report

Render a PLS Report

## Usage

``` r
render_pls_report(
  x,
  output_file = NULL,
  output_format = "html",
  title = "PLS Analysis Report",
  author = Sys.info()[["user"]],
  template = NULL,
  include_brain = TRUE,
  bsr_threshold = 3,
  p_threshold = 0.05,
  open = FALSE,
  ...
)
```

## Arguments

- x:

  A `pls_result` object or a path to pipeline/reportable artifacts.

- output_file:

  Output file path (default: artifact-root `reports/` or a timestamped
  local file)

- output_format:

  Output format: `"html"` (default), `"pdf"`, or `"docx"`

- title:

  Report title

- author:

  Report author

- template:

  Custom Quarto template path (NULL = built-in)

- include_brain:

  Include brain map visualizations when a mask is available

- bsr_threshold:

  BSR threshold for reliability summaries

- p_threshold:

  P-value threshold for significance summaries

- open:

  Open the report after rendering when interactive

- ...:

  Additional arguments passed to `quarto::quarto_render()`

## Value

Path to the rendered report, invisibly.
