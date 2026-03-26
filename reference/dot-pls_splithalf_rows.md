# Compute Split-Half Row Selectors

Internal helper implementing the MATLAB `reshape(...); tmp(:)` logic
used to pick the first and second half of subjects within each group.

## Usage

``` r
.pls_splithalf_rows(num_subj_lst, num_cond)
```

## Arguments

- num_subj_lst:

  Integer vector of subjects per group

- num_cond:

  Integer number of conditions

## Value

List with `num_subj_lst1`, `num_subj_lst2`, `rows1`, `rows2`.
