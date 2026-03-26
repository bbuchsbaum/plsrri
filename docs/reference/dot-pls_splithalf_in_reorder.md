# Build Inner Subject Reorder Vector

Internal helper mirroring the MATLAB logic:
`offset + n*(cond-1) + gperm` for each group and condition.

## Usage

``` r
.pls_splithalf_in_reorder(num_subj_lst, num_cond, subject_perm_by_group)
```

## Arguments

- num_subj_lst:

  Integer vector of subjects per group

- num_cond:

  Integer number of conditions

- subject_perm_by_group:

  List of integer permutations, one per group.

## Value

Integer vector of length `sum(num_subj_lst) * num_cond`.
