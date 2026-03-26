# Get Covariance for Subset of Rows

Helper function to compute covariance/correlation matrices for a subset
of observations. This is mainly used by resampling routines.

## Usage

``` r
pls_get_covcor_subset(
  stacked_datamat,
  stacked_behavdata,
  num_groups,
  num_subj_lst,
  num_cond,
  method,
  row_idx,
  bscan,
  meancentering_type,
  cormode
)
```
