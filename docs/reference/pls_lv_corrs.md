# Compute LV Correlations

Computes correlations between behavior data and brain scores. These are
the behavior loadings for behavior PLS.

## Usage

``` r
pls_lv_corrs(
  stacked_behavdata,
  brain_scores,
  num_groups,
  num_subj_lst,
  num_cond,
  cormode = 0L
)
```

## Arguments

- stacked_behavdata:

  Behavior data matrix

- brain_scores:

  Brain scores matrix

- num_groups:

  Number of groups

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- cormode:

  Correlation mode

## Value

LV correlations matrix
