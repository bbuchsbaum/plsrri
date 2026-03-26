# SVD with Missing Value Handling

Performs SVD on a matrix, iteratively imputing missing values (NaN/NA).
Based on the algorithm from misssvd.m (Claus A. Andersson).

## Usage

``` r
misssvd(X, truncate = TRUE, conv_lim = 1e-12, max_iter = 100)
```

## Arguments

- X:

  Matrix to decompose

- truncate:

  Logical; if TRUE, truncate to rank of X (economy SVD)

- conv_lim:

  Convergence limit for missing value iteration

- max_iter:

  Maximum iterations for missing value imputation

## Value

List with components:

- u:

  Left singular vectors

- d:

  Singular values (as vector)

- v:

  Right singular vectors

## Examples

``` r
X <- matrix(rnorm(100), 10, 10)
X[sample(100, 5)] <- NA
result <- misssvd(X)
```
