# Run Split-Half Validation

Performs split-half cross-validation where the sample is randomly split,
PLS is run on each half, and the correlation between the resulting
weights (u) and loadings (v) is computed.

## Usage

``` r
pls_splithalf_test(
  stacked_datamat,
  stacked_behavdata = NULL,
  stacked_designdata = NULL,
  num_groups,
  num_subj_lst,
  num_cond,
  method,
  num_split,
  num_outer_perm = 0L,
  clim = 95,
  bscan = NULL,
  meancentering_type = 0L,
  cormode = 0L,
  is_struct = FALSE,
  outer_reorder = NULL,
  inner_subject_perms = NULL,
  progress = TRUE
)
```

## Arguments

- stacked_datamat:

  Stacked data matrix

- stacked_behavdata:

  Behavior data matrix (methods 3-6)

- stacked_designdata:

  Design contrast matrix (methods 2, 5, 6)

- num_groups:

  Number of groups

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- method:

  PLS method (1-6)

- num_split:

  Number of split-half iterations

- num_outer_perm:

  Number of outer permutations for significance (includes the unpermuted
  reference sample as the first permutation, MATLAB
  `missnk_rri_perm_order` style).

- clim:

  Confidence level used for optional CI reporting (percent).

- bscan:

  Conditions for behavior block

- meancentering_type:

  Mean-centering type

- cormode:

  Correlation mode

- is_struct:

  Structure PLS flag (do not permute conditions within-subject)

- outer_reorder:

  Optional matrix of outer permutation orders (`nrow(stacked_datamat)` x
  `num_outer_perm`). If provided, overrides the internally generated
  outer permutation orders (MATLAB `permsamp` style).

- inner_subject_perms:

  Optional nested list specifying the within-group subject permutations
  for each outer permutation and split. If provided, it must have shape
  `[[op]][[p]][[g]]` where each entry is a permutation of
  `1:num_subj_lst[g]`. This enables deterministic split-half runs for
  testing.

- progress:

  Show progress

## Value

pls_splithalf_result object
