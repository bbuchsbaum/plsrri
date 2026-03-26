# Fast Cross-Covariance

Computes covariance between design and data matrices.

## Usage

``` r
xcor_cov_cpp(design, datamat)
```

## Arguments

- design:

  Design matrix (n_obs x n_design)

- datamat:

  Data matrix (n_obs x n_features)

## Value

Cross-covariance matrix (n_design x n_features)
