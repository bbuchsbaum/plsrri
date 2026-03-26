# Procrustes Rotation with Sign Flipping

First tries simple sign flipping for each column, then falls back to
full Procrustes if needed.

## Usage

``` r
procrustes_boot_cpp(X, Y, max_iter = 100L, tol = 1e-08)
```

## Arguments

- X:

  Target matrix

- Y:

  Source matrix

- max_iter:

  Maximum iterations for optimization

- tol:

  Tolerance for convergence

## Value

List with rotated Y and rotation matrix
