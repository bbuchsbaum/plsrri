# Fast Cross-Correlation (Cosine Angle)

Computes cosine similarity between design and data matrices.

## Usage

``` r
xcor_cosine_cpp(design, datamat)
```

## Arguments

- design:

  Design matrix (n_obs x n_design)

- datamat:

  Data matrix (n_obs x n_features)

## Value

Cross-correlation matrix (n_design x n_features)
