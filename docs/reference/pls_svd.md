# Standard PLS SVD

Wrapper for SVD used in PLS analysis. Uses economy SVD by default.

## Usage

``` r
pls_svd(X, handle_missing = TRUE)
```

## Arguments

- X:

  Matrix to decompose

- handle_missing:

  Logical; use misssvd if TRUE and missing values present

## Value

List with u, d, v components
