# Compare Nested Task PLS Design Subspaces

Fits the delta design subspace from a full cell-level design after QR
residualization against a reduced design. With
`permutation = "global_task_pls"`, compares the delta statistic to the
package's ordinary global Task PLS permutation null. This global null is
not a reduced-model-preserving nested test.

## Usage

``` r
compare_design_subspaces(
  x,
  reduced,
  full,
  fit = NULL,
  spec = NULL,
  design = NULL,
  condition_key = NULL,
  statistic = c("trace", "largest_root"),
  weights = c("cell_equal", "subject_count", "row_weights"),
  nperm = 0L,
  permutation = c("none", "global_task_pls", "reduced_model"),
  permsamp = NULL,
  progress = FALSE
)
```

## Arguments

- x:

  A Task PLS `pls_result`, or a `pls_spec` when `fit` is supplied.

- reduced:

  One-sided reduced formula.

- full:

  One-sided full formula.

- fit:

  Optional fitted `pls_result` when `x` is a `pls_spec`.

- spec:

  Optional `pls_spec` used to recompute permuted cross-block matrices.
  Required for `nperm > 0` when `x` is a `pls_result`.

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

- permsamp:

  Optional permutation order matrix.

- progress:

  Logical; show permutation progress messages.

## Value

A one-row data frame with observed nested-subspace statistic.
