# Fast Permutation Test for Task PLS

Runs the inner loop of permutation testing for task PLS. Computes
singular values for each permutation and compares to observed.

## Usage

``` r
perm_test_task_cpp(
  stacked_datamat,
  permsamp,
  observed_s,
  num_groups,
  num_subj_lst,
  num_cond,
  meancentering_type,
  keep_distribution = FALSE
)
```

## Arguments

- stacked_datamat:

  Stacked data matrix

- permsamp:

  Permutation sample matrix (total_rows x num_perm)

- observed_s:

  Observed singular values

- num_groups:

  Number of groups

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- meancentering_type:

  Mean-centering type

- keep_distribution:

  When TRUE, also return the (n_lv x num_perm) matrix of permuted
  singular values for diagnostic plots.

## Value

List with elements:

- `sp` (integer vector, length `n_lv`): counts of permuted s \>=
  observed s.

- `perm_singval` (numeric matrix `n_lv x num_perm`, or `NULL`): full
  null distribution when `keep_distribution = TRUE`; `NULL` otherwise.
