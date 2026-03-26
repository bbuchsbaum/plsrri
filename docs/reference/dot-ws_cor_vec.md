# Correlate a vector with each column of a matrix

Fast Pearson correlation of a single vector (seed) with each column
(voxel) of a matrix across their shared observations. Zero-variance
inputs produce 0.

## Usage

``` r
.ws_cor_vec(x, Y)
```

## Arguments

- x:

  Numeric vector (n observations).

- Y:

  Numeric matrix (n x p).

## Value

Numeric vector of length p.
