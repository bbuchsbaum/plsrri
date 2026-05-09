# Decompose a Task PLS Design LV into Factorial Subspaces

Descriptively attributes a design-side latent variable to centered
factorial subspaces. This is not an inferential test.

## Usage

``` r
decompose_design_terms(
  result,
  lv = 1L,
  design = NULL,
  formula = NULL,
  condition_key = NULL,
  weights = c("cell_equal", "subject_count", "row_weights")
)
```

## Arguments

- result:

  A Task PLS `pls_result`.

- lv:

  Latent variable index.

- design:

  Optional `pls_design` object.

- formula:

  Optional one-sided formula overriding `design$formula`.

- condition_key:

  Optional condition metadata.

- weights:

  `"cell_equal"`, `"subject_count"`, or `"row_weights"` to use stored
  `result$task_pls$row_weights`.

## Value

A data frame with LV energy and fraction by factorial term.

## Details

Term fractions are independent projections. They sum to one only when
the centered term subspaces are orthogonal under the requested weights.
