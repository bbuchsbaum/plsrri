# Extract Component Importance

Returns the importance measure for each component. For PLS, these are
singular values. For CPCA, eigenvalues. For ICA, explained variance.

## Usage

``` r
importance(x, normalize = FALSE, ...)
```

## Arguments

- x:

  An `mva_result` or `pls_result` object

- normalize:

  Return as proportion of total (percentage)

- ...:

  Additional arguments

## Value

Numeric vector of importance values
