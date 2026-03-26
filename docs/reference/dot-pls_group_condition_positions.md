# Precompute group/condition row positions in stacked ordering

Returns positions in 1:total_rows for each group and condition. These
are positions into a bootstrap order vector, matching MATLAB's indexing
logic.

## Usage

``` r
.pls_group_condition_positions(num_subj_lst, num_cond)
```
