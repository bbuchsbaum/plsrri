# Fast Bootstrap Test for Task PLS

Runs the inner loop of bootstrap testing for task PLS. Accumulates
bootstrap saliences for SE computation.

## Usage

``` r
boot_test_task_cpp(
  stacked_datamat,
  bootsamp,
  observed_u,
  num_groups,
  num_subj_lst,
  num_cond,
  meancentering_type
)
```

## Arguments

- stacked_datamat:

  Stacked data matrix

- bootsamp:

  Bootstrap sample matrix (total_rows x num_boot)

- observed_u:

  Observed u (saliences)

- num_groups:

  Number of groups

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- meancentering_type:

  Mean-centering type

## Value

List with u_sum and u_sq_sum
