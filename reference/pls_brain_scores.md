# Compute Brain Scores

Computes brain scores by projecting the data matrix onto the saliences.
Brain scores indicate how strongly each observation expresses each
latent variable pattern.

## Usage

``` r
pls_brain_scores(datamat, salience)
```

## Arguments

- datamat:

  Data matrix (n_obs x n_voxels)

- salience:

  Salience matrix (n_voxels x n_lv)

## Value

Brain scores matrix (n_obs x n_lv)
