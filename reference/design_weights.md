# Extract Design/Behavior Weights

Returns the design-space loadings from any multivariate analysis result.
For PLS, these are the v matrix (design or behavior loadings).

## Usage

``` r
design_weights(x, k = NULL, ...)
```

## Arguments

- x:

  An `mva_result` or `pls_result` object

- k:

  Component index or vector (NULL = all)

- ...:

  Additional arguments

## Value

Matrix of design weights
