# Scripted End-to-End Workflows

``` r
library(plsrri)
```

You want one analysis contract that works in three places: ordinary R
code, a staged CLI workflow, and later the Shiny app. In `plsrri`, that
contract is a saved artifact directory containing first-level manifests,
a planned PLS manifest, a saved `pls_result`, and optional reports.

This vignette starts from a tiny synthetic artifact root so the core
workflow stays runnable under `R CMD check`. It then shows how the same
workflow extends to
[`prepare_firstlevel()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md)
and `plscli` on a real BIDS dataset.

## What does the saved-artifact workflow look like?

The smallest stable path is:

1.  point
    [`prepare_pls()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md)
    at a saved artifact root or pipeline YAML
2.  run the returned `pls_spec`
3.  render a Quarto report from the same saved artifacts

``` r
pspec <- prepare_pls(out_root, pls_options = list(method = "task", nperm = 0L, nboot = 0L))

stopifnot(
  inherits(pspec, "pls_spec"),
  pspec$num_cond == 2L,
  length(pspec$datamat_lst) == 1L
)

pspec
```

Once you have a prepared spec,
[`run_pls()`](https://bbuchsbaum.github.io/plsrri/reference/run_pls.md)
and
[`render_pls_report()`](https://bbuchsbaum.github.io/plsrri/reference/render_pls_report.md)
are thin, deterministic wrappers.

``` r
result <- run_pls(spec_path)
report_file <- file.path(tmp, "plsrri-scripted-report.html")
rendered <- render_pls_report(spec_path, output_file = report_file, include_brain = FALSE)
#> 
#> 
#> processing file: report.qmd
#> 1/29                    
#> 2/29 [setup]            
#> 3/29                    
#> 4/29 [provenance]       
#> 5/29                    
#> 6/29 [pipeline-summary] 
#> 7/29                    
#> 8/29 [input-inventory]  
#> 9/29                    
#> 10/29 [sv-plot]          
#> 11/29                    
#> 12/29 [sv-table]         
#> 13/29                    
#> 14/29 [sig-status]       
#> 15/29                    
#> 16/29 [lv-tabs]          
#> 17/29                    
#> 18/29 [bootstrap-section]
#> 19/29                    
#> 20/29 [bootstrap-summary]
#> 21/29                    
#> 22/29 [artifact-notes]   
#> 23/29                    
#> 24/29 [unnamed-chunk-1]  
#> 25/29                    
#> 26/29 [unnamed-chunk-2]  
#> 27/29                    
#> 28/29 [unnamed-chunk-3]  
#> 29/29                    
#> output file: report.knit.md
#> 
#> pandoc 
#>   to: html
#>   output-file: plsrri-scripted-report.html
#>   standalone: true
#>   self-contained: true
#>   section-divs: true
#>   html-math-method: mathjax
#>   wrap: none
#>   default-image-extension: png
#>   toc: true
#>   toc-depth: 3
#>   variables: {}
#>   
#> metadata
#>   document-css: false
#>   link-citations: true
#>   date-format: long
#>   lang: en
#>   title: '`r params$title`'
#>   subtitle: Partial Least Squares Analysis
#>   author: '`r params$author`'
#>   date: '`r Sys.Date()`'
#>   theme:
#>     light:
#>       - flatly
#>       - custom.scss
#>     dark:
#>       - darkly
#>       - custom.scss
#>   toc-location: left
#>   toc-title: Contents
#>   smooth-scroll: true
#>   anchor-sections: true
#>   fontsize: 1rem
#>   linestretch: 1.5
#>   mainfont: Inter, system-ui, sans-serif
#>   max-width: 860px
#>   output-file: plsrri-scripted-report.html
#>   
#> Output created: plsrri-scripted-report.html
#> 
#> 

stopifnot(
  inherits(result, "pls_result"),
  all(is.finite(result$s)),
  file.exists(file.path(out_root, "pls", "pls_result.rds")),
  file.exists(rendered),
  file.info(rendered)$size > 0
)

c(
  latent_variables = n_lv(result),
  features = n_features(result)
)
#> latent_variables         features 
#>                2                2
```

The saved result and the rendered report are part of the same artifact
contract, so you can reopen them later without reconstructing the
analysis state.

``` r
summary_file <- file.path(out_root, "pls", "pls_summary.tsv")
saved_summary <- utils::read.delim(summary_file, sep = "\t", stringsAsFactors = FALSE)

stopifnot(
  nrow(saved_summary) >= 1L,
  all(is.finite(saved_summary$singular_value))
)

saved_summary
#>   lv singular_value variance_explained
#> 1  1   4.300000e+00       1.000000e+00
#> 2  2   1.219228e-16       8.039569e-34
```

## When do you call `prepare_firstlevel()`?

Use
[`prepare_firstlevel()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md)
when you want the same R session to own BIDS discovery, first-level
planning, and first-level execution before moving into PLS.

That live path depends on the external first-level stack (`bidser`,
`fmridataset`, `fmridesign`, `fmrireg`), so it is shown here as an
execution pattern rather than as a vignette-evaluated chunk:

``` r
prep <- prepare_firstlevel("study.yml")
result <- run_pls(prep)
render_pls_report(prep$spec)
```

For workstation use, that is often the cleanest end-to-end R workflow.
For HPC use, the same YAML spec can be split into explicit stages.

## How does the same workflow look from the CLI?

The CLI mirrors the same contract and the same stage boundaries.

``` bash
plscli validate --spec study.yml
plscli discover --spec study.yml
plscli firstlevel-plan --spec study.yml
plscli firstlevel-run --spec study.yml --work-id w0007
plscli pls-plan --spec study.yml
plscli pls-run --spec study.yml
plscli report --input study.yml --format html
```

For array jobs, the only thing that changes is how you schedule
`firstlevel-run`.

``` bash
plscli firstlevel-plan --spec study.yml
plscli firstlevel-run --spec study.yml --work-id "${SLURM_ARRAY_TASK_ID}"
plscli pls-plan --spec study.yml
plscli pls-run --spec study.yml
plscli report --input /path/to/artifact-root --format pdf
```

The important design point is that `plscli report` reads the same
artifacts that
[`render_pls_report()`](https://bbuchsbaum.github.io/plsrri/reference/render_pls_report.md)
reads in R, and
[`prepare_pls()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md)
reads the same first-level manifests that the CLI writes.

## What should you treat as the public contract?

For scripted analysis, the stable surfaces are:

- [`prepare_firstlevel()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md)
- [`prepare_pls()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md)
- [`run_pls()`](https://bbuchsbaum.github.io/plsrri/reference/run_pls.md)
- [`render_pls_report()`](https://bbuchsbaum.github.io/plsrri/reference/render_pls_report.md)

The stable on-disk contract is the artifact root containing:

- `firstlevel/work_plan.tsv`
- `firstlevel/work/*/maps.tsv`
- `pls/pls_manifest.tsv`
- `pls/pls_result.rds`
- `pls/pls_summary.tsv`
- `reports/`

That is the contract the Shiny app is also expected to consume when it
attaches to existing first-level outputs.

## Where should you go next?

Use
[`vignette("plsrri")`](https://bbuchsbaum.github.io/plsrri/articles/plsrri.md)
for the main task-PLS workflow,
[`vignette("behavior-pls")`](https://bbuchsbaum.github.io/plsrri/articles/behavior-pls.md)
for behavior PLS, and
[`vignette("multiblock-and-seed")`](https://bbuchsbaum.github.io/plsrri/articles/multiblock-and-seed.md)
for multiblock and seed analyses.

The most relevant help pages after this vignette are:

- [`?prepare_firstlevel`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md)
- [`?prepare_pls`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md)
- [`?run_pls`](https://bbuchsbaum.github.io/plsrri/reference/run_pls.md)
- [`?render_pls_report`](https://bbuchsbaum.github.io/plsrri/reference/render_pls_report.md)
- [`?plscli_main`](https://bbuchsbaum.github.io/plsrri/reference/plscli_main.md)
