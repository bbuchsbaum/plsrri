# Compare Nested Task PLS Design Subspaces

Compatibility wrapper for
[`compare_design_subspaces()`](https://bbuchsbaum.github.io/plsrri/reference/compare_design_subspaces.md).

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
  nperm = 0L,
  permutation = c("none", "global_task_pls", "reduced_model"),
  spec = NULL,
  permsamp = NULL,
  progress = FALSE
)
```

## Arguments

- result:

  A Task PLS `pls_result`.

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

  Number of permutations.

- permutation:

  `"none"` or `"global_task_pls"`.

- spec:

  Optional `pls_spec` used to recompute permuted cross-block matrices.
  Required for `nperm > 0` when `x` is a `pls_result`.

- permsamp:

  Optional permutation order matrix.

- progress:

  Logical; show permutation progress messages.

## Value

A one-row data frame with observed nested-subspace statistic.
