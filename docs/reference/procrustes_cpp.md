# Procrustes Rotation

Aligns matrix Y to matrix X using Procrustes rotation. Finds orthogonal
matrix Q that minimizes \|\|X - Y\*Q\|\|\_F

## Usage

``` r
procrustes_cpp(X, Y, scale = FALSE)
```

## Arguments

- X:

  Target matrix

- Y:

  Source matrix to rotate

- scale:

  Allow scaling (default false)

## Value

Rotated Y matrix
