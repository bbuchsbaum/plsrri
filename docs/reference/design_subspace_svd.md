# Term-Specific Design-Subspace SVDs for Task PLS

Fits ASCA-like term-specific PLS decompositions by projecting the Task
PLS cross-block matrix into centered factorial design subspaces, then
computing an SVD within each projected subspace.

## Usage

``` r
design_subspace_svd(
  result,
  design = NULL,
  formula = NULL,
  condition_key = NULL,
  terms = "all",
  ncomp = NULL,
  statistic = c("trace", "largest_root"),
  weights = c("cell_equal", "subject_count", "row_weights"),
  keep_crossblocks = FALSE
)
```

## Arguments

- result:

  A Task PLS `pls_result` produced with
  `pls_analysis(..., keep_crossblock = TRUE)` or
  `run(..., keep_crossblock = TRUE)`.

- design:

  Optional `pls_design` object.

- formula:

  Optional one-sided formula overriding `design$formula`.

- condition_key:

  Optional condition metadata.

- terms:

  `"all"` or a character vector of term labels to decompose.

- ncomp:

  Optional number of components to retain per term.

- statistic:

  `"trace"` for total subspace covariance energy, or `"largest_root"`
  for the dominant singular-root statistic.

- weights:

  `"cell_equal"`, `"subject_count"`, or `"row_weights"` to use stored
  `result$task_pls$row_weights`.

- keep_crossblocks:

  Logical; retain projected cross-block matrices.

## Value

A `pls_design_subspace_svd` object with term-level statistics and
component-level singular values.
