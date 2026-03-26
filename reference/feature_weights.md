# Extract Feature Weights

Returns the feature-space loadings (brain weights) from any multivariate
analysis result. For PLS, these are the saliences (u matrix).

## Usage

``` r
feature_weights(x, k = NULL, as_neurovol = FALSE, ...)
```

## Arguments

- x:

  An `mva_result` or `pls_result` object

- k:

  Component index or vector (NULL = all)

- as_neurovol:

  Convert to NeuroVol if mask available

- ...:

  Additional arguments

## Value

Matrix of feature weights, or NeuroVol
