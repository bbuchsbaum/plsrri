# plsrri

<!-- badges: start -->
[![R-CMD-check](https://github.com/bbuchsbaum/plsrri/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bbuchsbaum/plsrri/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/bbuchsbaum/plsrri/actions/workflows/pkgdown.yaml/badge.svg)](https://bbuchsbaum.github.io/plsrri/)
<!-- badges: end -->

> **Note:** This package is under active development. The API may change before the first stable release.

A modern R implementation of Partial Least Squares (PLS) for neuroimaging analysis, based on the McIntosh Lab MATLAB toolbox. Supports task PLS, behavior PLS, and multiblock PLS with permutation testing, bootstrap confidence intervals, and split-half validation.

## Installation

```r
# Install from GitHub
remotes::install_github("bbuchsbaum/plsrri")
```

## Quick Start

```r
library(plsrri)

# Task PLS with 2 groups, 3 conditions
result <- pls_spec() |>
  add_subjects(list(group1_data, group2_data), groups = c(20, 18)) |>
  add_conditions(3, labels = c("encoding", "retrieval", "rest")) |>
  configure(method = "task", nperm = 500, nboot = 500) |>
  run()

# Extract results
significance(result)
sal <- salience(result, lv = 1, as_neurovol = TRUE)
bsr_map <- bsr(result, lv = 1, threshold = 2)
```

## Features

- **6 PLS methods**: task, behavior, multiblock (rotated and non-rotated variants)
- **Fluent builder API**: pipe-friendly specification and execution
- **BIDS integration**: automatic discovery of neuroimaging data
- **Resampling inference**: permutation testing, bootstrap CIs, split-half validation
- **Interactive Shiny GUI**: 3-step workflow (Setup, Analyze, Explore)
- **Brain visualization**: volume montage/ortho views and cortical surface rendering
- **Pipeline system**: CLI for batch/HPC, UI for interactive exploration
- **Method-neutral framework**: extensible `MvaMethod` protocol for adding new multivariate approaches (CPCA, CCA, ICA, etc.)

## Documentation

See the [pkgdown site](https://bbuchsbaum.github.io/plsrri/) for full documentation, including:

- [Getting Started](https://bbuchsbaum.github.io/plsrri/articles/plsrri.html)
- [Scripted Workflows](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.html)
- [Behavior PLS](https://bbuchsbaum.github.io/plsrri/articles/behavior-pls.html)
- [Multiblock and Seed PLS](https://bbuchsbaum.github.io/plsrri/articles/multiblock-and-seed.html)

## License

GPL-3
