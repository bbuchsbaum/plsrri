# Get Behavior Scores

Computes behavior scores for behavior and multiblock PLS. These
represent how strongly each behavior measure relates to each LV.

## Usage

``` r
pls_behav_scores(
  stacked_behavdata,
  lvcorrs,
  num_groups,
  num_subj_lst,
  num_cond,
  n_lv
)
```

## Arguments

- stacked_behavdata:

  Stacked behavior data

- lvcorrs:

  Latent variable correlations

- num_groups:

  Number of groups

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- n_lv:

  Number of latent variables

## Value

Behavior scores matrix
