# Observed Design-Subspace Statistics for Task PLS Terms

Computes observed covariance energy in each factorial design subspace.
The subspaces are built in the centered Task PLS row space, so effects
removed by the Task PLS mean-centering operator have rank zero.

## Usage

``` r
test_design_terms(
  result,
  design = NULL,
  formula = NULL,
  condition_key = NULL,
  statistic = c("trace", "largest_root"),
  weights = c("cell_equal", "subject_count", "row_weights"),
  nperm = 0L,
  correction = c("none", "maxT")
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

- statistic:

  `"trace"` for total subspace covariance energy, or `"largest_root"`
  for the dominant singular-root statistic.

- weights:

  `"cell_equal"`, `"subject_count"`, or `"row_weights"` to use stored
  `result$task_pls$row_weights`.

- nperm:

  Number of permutations. Permutation inference is not yet implemented
  for design-subspace tests; values greater than zero error.

- correction:

  Multiple-testing correction label. Present for API compatibility with
  future permutation support.

## Value

A data frame with term, rank, statistic, and placeholder p-value
columns.
