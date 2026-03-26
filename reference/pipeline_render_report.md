# Render a Report from `plscli` Artifacts

Render a Report from `plscli` Artifacts

## Usage

``` r
pipeline_render_report(
  spec,
  output_file = NULL,
  output_format = "html",
  title = "PLS Pipeline Report",
  author = Sys.info()[["user"]],
  open = FALSE,
  ...
)
```

## Arguments

- spec:

  Path to a pipeline YAML file or a validated pipeline spec list.

- output_file:

  Optional output file path.

- output_format:

  Output format: `"html"`, `"pdf"`, or `"docx"`.

- title:

  Report title.

- author:

  Report author.

- open:

  Open the rendered report if interactive.

- ...:

  Additional arguments passed to
  [`render_report()`](https://bbuchsbaum.github.io/plsrri/reference/render_report.md).

## Value

Path to rendered report, invisibly.
