# Fisher r-to-z Transform

Fisher r-to-z Transform

## Usage

``` r
.fisher_z(r)
```

## Arguments

- r:

  Numeric vector of correlation values.

## Value

Fisher z-transformed values. Values of exactly +/-1 are clamped to +/-
(1 - 1e-7) to avoid infinities.
