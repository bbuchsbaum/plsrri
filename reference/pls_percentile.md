# Compute Percentile

Computes the Nk-th percentile of X using linear interpolation. Ported
from MATLAB percentile.m

## Usage

``` r
pls_percentile(x, nk, margin = 1L)
```

## Arguments

- x:

  Numeric vector or matrix

- nk:

  Percentile(s) to compute (0-100)

- margin:

  For matrices: 1 = column percentiles (default), 2 = row percentiles

## Value

Percentile value(s)

## Examples

``` r
x <- rnorm(100)
pls_percentile(x, c(2.5, 97.5))
#> [1] -2.014884  1.820040
```
