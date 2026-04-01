# plsrri

`plsrri` is a modern R implementation of the McIntosh Lab PLS package for neuroimaging analysis. It supports task PLS, behavior PLS, and multiblock PLS with permutation testing, bootstrap confidence intervals, and split-half validation.

## Choose a workflow

### Interactive analysis

If you want to explore data, inspect scores and loadings, and attach to prepared first-level outputs through the UI, start with:

- `launch_pls_gui()`
- [Getting Started](articles/plsrri.html)

### Scripted and reproducible analysis

If you want a clean non-GUI pipeline from saved first-level artifacts to PLS results and Quarto reports, start with:

- [Scripted Workflows](articles/scripted-workflows.html)
- [Pipeline YAML Specification](articles/pipeline-yaml-spec.html)
- `prepare_firstlevel()`
- `prepare_pls()`
- `run_pls()`
- `render_pls_report()`
- `plscli_main()`

Start by scaffolding a spec rather than writing YAML from scratch:

```r
write_pipeline_template("study.yml")
```

```bash
plscli template --out study.yml
```

The minimal scripted path is:

```r
prep <- prepare_firstlevel("study.yml")
result <- run_pls(prep)
render_pls_report(prep$spec)
```

The matching staged CLI path is:

```bash
plscli validate --spec study.yml
plscli run --spec study.yml
plscli report --input study.yml --format html
```

That same contract is designed to work across:

- ordinary R scripts
- staged CLI and HPC runs
- Shiny attachment to existing first-level outputs

## Core articles

- [Getting Started](articles/plsrri.html)
- [Scripted End-to-End Workflows](articles/scripted-workflows.html)
- [Behavior PLS Workflow](articles/behavior-pls.html)
- [Multiblock and Seed PLS Workflows](articles/multiblock-and-seed.html)

## Key reference pages

- [Analysis API](reference/analysis-api.html)
- [CLI entrypoint](reference/plscli.html)
- [Reporting](reference/pls-reports.html)
- [Pipeline attach API](reference/pipeline-attach.html)
