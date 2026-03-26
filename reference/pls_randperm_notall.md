# Permute Only Selected Conditions (MATLAB rri_randperm_notall)

Creates a permutation order that shuffles only rows corresponding to
conditions in `bscan` (across all groups), leaving other rows unchanged.

## Usage

``` r
pls_randperm_notall(num_subj_lst, num_cond, bscan)
```

## Arguments

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- bscan:

  Conditions to permute

## Value

Integer vector of length `sum(num_subj_lst)*num_cond`
