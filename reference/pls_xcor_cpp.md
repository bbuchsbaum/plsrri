# Fast Cross-Correlation (All Modes)

Computes cross-correlation using specified correlation mode.

## Usage

``` r
pls_xcor_cpp(design, datamat, cormode = 0L)
```

## Arguments

- design:

  Design matrix (n_obs x n_design)

- datamat:

  Data matrix (n_obs x n_features)

- cormode:

  Correlation mode: 0=Pearson, 2=Cov, 4=Cosine, 6=Dot

## Value

Cross-correlation matrix (n_design x n_features)
