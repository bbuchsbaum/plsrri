# Fast Permutation Test with Pre-computed Cross-block Matrices

More efficient version that pre-computes task means and only shuffles.

## Usage

``` r
perm_test_fast_cpp(task_means_lst, permsamp, observed_s)
```

## Arguments

- task_means_lst:

  List of task mean matrices by group

- permsamp:

  Permutation sample matrix

- observed_s:

  Observed singular values

## Value

Vector of counts
