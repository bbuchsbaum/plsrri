# Get Covariance/Correlation for SVD

Main function for preparing the cross-block matrix for SVD
decomposition. Handles all 6 PLS methods with appropriate mean-centering
and correlation modes. Ported from MATLAB rri_get_covcor.m

## Usage

``` r
pls_get_covcor(
  method,
  stacked_datamat,
  stacked_behavdata = NULL,
  num_groups,
  num_subj_lst,
  num_cond,
  bscan = seq_len(num_cond),
  meancentering_type = 0L,
  cormode = 0L,
  datamat_reorder = NULL,
  behavdata_reorder = NULL,
  datamat_reorder_4beh = NULL,
  compute_smeanmat = FALSE
)
```

## Arguments

- method:

  PLS method (1-6)

- stacked_datamat:

  Stacked brain data matrix

- stacked_behavdata:

  Stacked behavior data matrix (methods 3-6)

- num_groups:

  Number of groups

- num_subj_lst:

  Vector of subjects per group

- num_cond:

  Number of conditions

- bscan:

  Conditions for behavior block (multiblock)

- meancentering_type:

  Mean-centering type (0-3)

- cormode:

  Correlation mode (0, 2, 4, 6)

- datamat_reorder:

  Optional reordering indices for datamat

- behavdata_reorder:

  Optional reordering indices for behavdata

- datamat_reorder_4beh:

  Optional reordering for behavior block datamat

- compute_smeanmat:

  Logical, compute `stacked_smeanmat` for brain-score CIs

## Value

List with:

- datamatsvd:

  Cross-block matrix for SVD

- datamatsvd_unnorm:

  Unnormalized version (multiblock)

- datamatcorrs_lst:

  Correlation matrices by group (behavior PLS)

- stacked_smeanmat:

  Matrix for brain score CIs (task PLS with bootstrap)
