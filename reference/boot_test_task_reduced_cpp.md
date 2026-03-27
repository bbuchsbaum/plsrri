# Fast Reduced-Space Bootstrap Test for Task PLS

Runs the reduced-space inner bootstrap loop for balanced task methods
1/2. Returns accumulated saliences and per-bootstrap task score
distributions.

## Usage

``` r
boot_test_task_reduced_cpp(
  task_scores,
  task_loadings,
  stacked_datamat,
  bootsamp,
  observed_v,
  stacked_designdata,
  num_groups,
  num_subj_lst,
  num_cond,
  method
)
```

## Arguments

- task_scores:

  Reduced row-space scores (`U D`)

- task_loadings:

  Right singular vectors used to lift back to features

- stacked_datamat:

  Original stacked data matrix

- bootsamp:

  Bootstrap sample matrix (total_rows x num_boot)

- observed_v:

  Observed design-side saliences/contrasts

- stacked_designdata:

  Normalized design matrix for method 2, or 0-col matrix for method 1

- num_groups:

  Number of groups

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- method:

  Task method (1 or 2)

## Value

List with `u_sum`, `u_sq`, and `usc_distrib_boot`
