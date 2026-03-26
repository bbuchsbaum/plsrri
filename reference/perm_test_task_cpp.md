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
  meancentering_type
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

## Value

Vector of counts (permuted s \>= observed s)
