# Test Task PLS Design Subspaces

Computes observed covariance energy in term-specific design subspaces
and, when requested, compares those statistics to a global Task PLS
permutation null. The permutation null asks whether term-aligned
covariance is larger than expected under the package's ordinary global
Task PLS row permutation.

## Usage

``` r
test_design_subspaces(
  x,
  fit = NULL,
  spec = NULL,
  design = NULL,
  formula = NULL,
  condition_key = NULL,
  terms = "all",
  statistic = c("trace", "largest_root"),
  weights = c("cell_equal", "subject_count", "row_weights"),
  nperm = 0L,
  permutation = c("none", "global_task_pls"),
  correction = c("none", "maxT"),
  permsamp = NULL,
  progress = FALSE
)
```

## Arguments

- x:

  A Task PLS `pls_result`, or a `pls_spec` when `fit` is supplied.

- fit:

  Optional fitted `pls_result` when `x` is a `pls_spec`.

- spec:

  Optional `pls_spec` used to recompute permuted cross-block matrices.
  Required for `nperm > 0` when `x` is a `pls_result`.

- design:

  Optional `pls_design` object.

- formula:

  Optional one-sided formula overriding `design$formula`.

- condition_key:

  Optional condition metadata.

- terms:

  `"all"` or a character vector of term labels to test.

- statistic:

  `"trace"` for total subspace covariance energy, or `"largest_root"`
  for the dominant singular-root statistic.

- weights:

  `"cell_equal"`, `"subject_count"`, or `"row_weights"` to use stored
  `result$task_pls$row_weights`.

- nperm:

  Number of permutations.

- permutation:

  `"none"` or `"global_task_pls"`.

- correction:

  `"none"` or `"maxT"` adjustment across tested terms.

- permsamp:

  Optional permutation order matrix.

- progress:

  Logical; show permutation progress messages.

## Value

A data frame with term, rank, statistic, p-value, and null labels.
