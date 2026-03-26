# Create a Multivariate Decomposition Object

The universal output of `MvaMethod$fit()`. Every analysis method must
produce this shape, mapping its native output to these fields.

## Usage

``` r
new_mva_decomposition(
  feature_weights,
  design_weights = NULL,
  importance,
  scores_feature = NULL,
  scores_design = NULL,
  extra = list(),
  method = NULL
)
```

## Arguments

- feature_weights:

  P x K matrix of feature loadings (brain weights)

- design_weights:

  D x K matrix of design/behavior loadings, or NULL

- importance:

  Length-K vector (singular values, eigenvalues, etc.)

- scores_feature:

  N x K matrix of feature-space scores (brain scores)

- scores_design:

  N x K matrix of design-space scores, or NULL

- extra:

  Named list of method-specific extras

- method:

  Character string identifying the method

## Value

An `mva_decomposition` object
