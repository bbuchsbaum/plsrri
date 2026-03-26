# Extract Loadings

Extracts design or behavior loadings (v) from a PLS result. For behavior
PLS, loadings represent the correlation between behavior measures and
brain scores.

## Usage

``` r
loadings(x, type = "design", lv = NULL)
```

## Arguments

- x:

  A `pls_result` object

- type:

  "design" (default) or "behavior"

- lv:

  Latent variable index or vector of indices (NULL = all)

## Value

Matrix of loadings
