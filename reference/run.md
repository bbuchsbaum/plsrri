# Run PLS Analysis from Specification

Validates the PLS specification and executes the analysis. This is the
final step in the builder pattern.

## Usage

``` r
run(spec, progress = TRUE, ...)
```

## Arguments

- spec:

  A `pls_spec` object

- progress:

  Logical, show progress messages

- ...:

  Additional arguments passed to
  [`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)

## Value

A `pls_result` object

## Examples

``` r
set.seed(42)
data1 <- matrix(rnorm(60 * 50), 60, 50)
data2 <- matrix(rnorm(54 * 50), 54, 50)

result <- pls_spec() |>
  add_subjects(list(data1, data2), groups = c(20, 18)) |>
  add_conditions(3) |>
  configure(method = "task", nperm = 10) |>
  run(progress = FALSE)
```
