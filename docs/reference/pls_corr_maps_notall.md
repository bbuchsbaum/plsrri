# Correlation Maps for Subset of Conditions

Computes correlation maps for a subset of conditions (used in multiblock
PLS).

## Usage

``` r
pls_corr_maps_notall(behavdata, datamat, n_subj, bscan, cormode = 0L)
```

## Arguments

- behavdata:

  Behavior matrix

- datamat:

  Brain data matrix

- n_subj:

  Number of subjects

- bscan:

  Vector of condition indices to use

- cormode:

  Correlation mode

## Value

Correlation matrix for selected conditions
