# Run Permutation Test

Runs permutation test to assess significance of singular values.

## Usage

``` r
pls_permutation_test(
  stacked_datamat,
  stacked_behavdata = NULL,
  stacked_designdata = NULL,
  num_groups,
  num_subj_lst,
  num_cond,
  method,
  num_perm,
  observed_s,
  observed_v = NULL,
  org_s = NULL,
  org_v = NULL,
  bscan = NULL,
  meancentering_type = 0L,
  cormode = 0L,
  is_struct = FALSE,
  parallel_config = NULL,
  permsamp = NULL,
  Tpermsamp = NULL,
  Bpermsamp = NULL,
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

- num_perm:

  Number of permutations

- observed_s:

  Observed singular values

- bscan:

  Conditions for behavior block

- meancentering_type:

  Mean-centering type

- cormode:

  Correlation mode

- is_struct:

  Structure PLS flag

- parallel_config:

  Optional parallel execution config list with `backend` and `workers`

- progress:

  Show progress

## Value

pls_perm_result object
