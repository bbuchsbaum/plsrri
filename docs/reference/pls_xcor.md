# Cross-Correlation of Two Matrices

Computes the cross-correlation (or covariance, cosine angle, or dot
product) between a design matrix and a data matrix. Both matrices must
have the same number of rows.

## Usage

``` r
pls_xcor(design, datamat, cormode = 0L)
```

## Arguments

- design:

  Design matrix (n_obs x n_design)

- datamat:

  Data matrix (n_obs x n_features)

- cormode:

  Correlation mode:

  0

  :   Pearson correlation (default)

  2

  :   Covariance

  4

  :   Cosine angle

  6

  :   Dot product

## Value

Cross-correlation matrix (n_design x n_features)

## Examples

``` r
design <- matrix(rnorm(30), 10, 3)
datamat <- matrix(rnorm(100), 10, 10)
xcor <- pls_xcor(design, datamat, cormode = 0)
```
