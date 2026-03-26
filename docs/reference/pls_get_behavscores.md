# Get Behavior Scores (MATLAB parity)

Port of MATLAB `rri_get_behavscores.m`. Computes brain scores (`usc`),
behavior/design scores (`vsc`), and LV correlations (`lvcorrs`) for
behavior and multiblock PLS.

## Usage

``` r
pls_get_behavscores(
  stacked_datamat,
  stacked_behavdata,
  brainlv,
  behavlv,
  num_cond,
  num_subj_lst,
  cormode = 0L
)
```

## Arguments

- stacked_datamat:

  Stacked brain data matrix (n_obs x n_voxels)

- stacked_behavdata:

  Stacked behavior data matrix (n_obs x n_behav)

- brainlv:

  Brain latent vectors (n_voxels x n_lv)

- behavlv:

  Behavior/design latent vectors (n_behav*num_cond*num_groups x n_lv)

- num_cond:

  Number of conditions

- num_subj_lst:

  Integer vector with subjects per group

- cormode:

  Correlation mode (0, 2, 4, 6)

## Value

List with `usc`, `vsc`, `lvcorrs`
