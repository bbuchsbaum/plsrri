# Compare Nested Task PLS Design Subspaces

Computes the observed covariance energy in the part of a full cell-level
design that remains after QR residualization against a reduced design.

## Usage

``` r
compare_designs(
  result,
  reduced,
  full,
  design = NULL,
  condition_key = NULL,
  statistic = c("trace", "largest_root"),
  weights = c("cell_equal", "subject_count", "row_weights"),
  nperm = 0L
)
```

## Arguments

- result:

  A Task PLS `pls_result` produced with
  `pls_analysis(..., keep_crossblock = TRUE)` or
  `run(..., keep_crossblock = TRUE)`.

- reduced:

  One-sided reduced formula.

- full:

  One-sided full formula.

- design:

  Optional `pls_design` object.

- condition_key:

  Optional condition metadata.

- statistic:

  `"trace"` or `"largest_root"`.

- weights:

  `"cell_equal"`, `"subject_count"`, or `"row_weights"` to use stored
  `result$task_pls$row_weights`.

- nperm:

  Number of permutations. Values greater than zero error until a
  reduced-model-respecting null is implemented.

## Value

A one-row data frame with observed nested-subspace statistic.
