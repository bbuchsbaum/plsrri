# Observed or Global-Null Design-Subspace Statistics for Task PLS Terms

Compatibility wrapper for
[`test_design_subspaces()`](https://bbuchsbaum.github.io/plsrri/reference/test_design_subspaces.md).
New code should prefer
[`test_design_subspaces()`](https://bbuchsbaum.github.io/plsrri/reference/test_design_subspaces.md)
to make clear that inference is over fitted design subspaces, not
post-hoc LV annotations.

## Usage

``` r
test_design_terms(
  result,
  design = NULL,
  formula = NULL,
  condition_key = NULL,
  terms = "all",
  statistic = c("trace", "largest_root"),
  weights = c("cell_equal", "subject_count", "row_weights"),
  nperm = 0L,
  permutation = c("none", "global_task_pls"),
  correction = c("none", "maxT"),
  spec = NULL,
  permsamp = NULL,
  progress = FALSE
)
```

## Arguments

- result:

  A Task PLS `pls_result`.

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

- spec:

  Optional `pls_spec` used to recompute permuted cross-block matrices.
  Required for `nperm > 0` when `x` is a `pls_result`.

- permsamp:

  Optional permutation order matrix.

- progress:

  Logical; show permutation progress messages.

## Value

A data frame with term, rank, statistic, and p-value columns.
