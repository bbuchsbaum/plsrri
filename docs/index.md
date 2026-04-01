# plsrri

`plsrri` is a modern R implementation of the McIntosh Lab PLS package
for neuroimaging analysis. It supports task PLS, behavior PLS, and
multiblock PLS with permutation testing, bootstrap confidence intervals,
and split-half validation.

## Choose a workflow

### Interactive analysis

If you want to explore data, inspect scores and loadings, and attach to
prepared first-level outputs through the UI, start with:

- [`launch_pls_gui()`](https://bbuchsbaum.github.io/plsrri/reference/launch_pls_gui.md)
- [Getting
  Started](https://bbuchsbaum.github.io/plsrri/articles/plsrri.md)

### Scripted and reproducible analysis

If you want a clean non-GUI pipeline from saved first-level artifacts to
PLS results and Quarto reports, start with:

- [Scripted
  Workflows](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md)
- [Pipeline YAML
  Specification](https://bbuchsbaum.github.io/plsrri/articles/pipeline-yaml-spec.md)
- [`prepare_firstlevel()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md)
- [`prepare_pls()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md)
- [`run_pls()`](https://bbuchsbaum.github.io/plsrri/reference/run_pls.md)
- [`render_pls_report()`](https://bbuchsbaum.github.io/plsrri/reference/render_pls_report.md)
- [`plscli_main()`](https://bbuchsbaum.github.io/plsrri/reference/plscli_main.md)

Start by scaffolding a spec rather than writing YAML from scratch:

``` r
write_pipeline_template("study.yml")
```

``` bash
plscli template --out study.yml
```

The minimal scripted path is:

``` r
prep <- prepare_firstlevel("study.yml")
result <- run_pls(prep)
render_pls_report(prep$spec)
```

The matching staged CLI path is:

``` bash
plscli validate --spec study.yml
plscli run --spec study.yml
plscli report --input study.yml --format html
```

That same contract is designed to work across:

- ordinary R scripts
- staged CLI and HPC runs
- Shiny attachment to existing first-level outputs

## Core articles

- [Getting
  Started](https://bbuchsbaum.github.io/plsrri/articles/plsrri.md)
- [Scripted End-to-End
  Workflows](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md)
- [Behavior PLS
  Workflow](https://bbuchsbaum.github.io/plsrri/articles/behavior-pls.md)
- [Multiblock and Seed PLS
  Workflows](https://bbuchsbaum.github.io/plsrri/articles/multiblock-and-seed.md)

## Key reference pages

- [Analysis
  API](https://bbuchsbaum.github.io/plsrri/reference/analysis-api.md)
- [CLI
  entrypoint](https://bbuchsbaum.github.io/plsrri/reference/plscli.md)
- [Reporting](https://bbuchsbaum.github.io/plsrri/reference/pls-reports.md)
- [Pipeline attach
  API](https://bbuchsbaum.github.io/plsrri/reference/pipeline-attach.md)
