# High-Level Analysis API

Public convenience wrappers for the staged non-GUI analysis workflow.
These functions expose a small, stable surface over the pipeline and
builder layers:

- [`prepare_firstlevel()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md):
  validate, discover, plan, and optionally run first-level estimation
  from a pipeline specification.

- [`prepare_pls()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md):
  build a runnable
  [`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md)
  from a saved pipeline, artifact root, or analysis plan.

- [`run_pls()`](https://bbuchsbaum.github.io/plsrri/reference/run_pls.md):
  run a prepared
  [`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md)
  or dispatch from saved pipeline/artifact inputs.

- [`render_pls_report()`](https://bbuchsbaum.github.io/plsrri/reference/render_pls_report.md):
  render a Quarto report from a result or saved artifacts.
