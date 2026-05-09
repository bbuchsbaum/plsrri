# Build Centered Design Projectors

Builds QR-based orthonormal bases and optional projection matrices for
each factorial term after applying the same Task PLS cell-centering
operator used to create the stored cross-block matrix.

## Usage

``` r
design_projectors(
  result,
  design = NULL,
  formula = NULL,
  condition_key = NULL,
  weights = c("cell_equal", "subject_count", "row_weights"),
  include_matrix = FALSE
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

- weights:

  `"cell_equal"`, `"subject_count"`, or `"row_weights"` to use stored
  `result$task_pls$row_weights`.

- include_matrix:

  Logical; include dense projection matrices.

## Value

A named list of projector descriptors.
