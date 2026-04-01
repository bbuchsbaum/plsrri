# Project Held-Out Observations into a Fitted Score Space

Project Held-Out Observations into a Fitted Score Space

## Usage

``` r
project_scores(x, newdata, type = "feature", progress = FALSE, ...)
```

## Arguments

- x:

  A fitted `pls_result` or `mva_result`.

- newdata:

  Held-out `pls_spec` data to project.

- type:

  Score space to project into: `"feature"`/`"brain"`,
  `"design"`/`"behavior"`, or `"both"`.

- progress:

  Logical, show progress.

- ...:

  Reserved for method-specific extensions.

## Value

A score matrix, or a named list with `feature` and `design` matrices
when `type = "both"`.

## Examples

``` r
set.seed(42)
datamat <- matrix(rnorm(24 * 10), 24, 10)
spec <- pls_spec()
spec$datamat_lst <- list(datamat)
spec$num_subj_lst <- 12L
spec$num_cond <- 2L
result <- run(spec, progress = FALSE)
proj <- project_scores(result, spec, type = "brain")
```
