# Correlation Maps for Behavior PLS

Computes correlation between behavior data and brain data within each
condition, stacked across all conditions.

## Usage

``` r
pls_corr_maps(behavdata, datamat, n_subj, num_cond, cormode = 0L)
```

## Arguments

- behavdata:

  Behavior matrix (n_obs x n_behav)

- datamat:

  Brain data matrix (n_obs x n_voxels)

- n_subj:

  Number of subjects

- num_cond:

  Number of conditions

- cormode:

  Correlation mode (0, 2, 4, or 6)

## Value

Correlation matrix (n_behav \* n_cond x n_voxels)
