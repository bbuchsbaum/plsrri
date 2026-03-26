# NK/MissNK Permutation Orders (include unpermuted sample)

MATLAB parity for `missnk_rri_perm_order.m`: first column is the
identity ordering, and sequential order is not treated as invalid for
subsequent permutations.

## Usage

``` r
.pls_perm_order_missnk(num_subj_lst, num_cond, num_perm, not_in_cond = FALSE)
```

## Arguments

- num_subj_lst:

  Integer vector of subjects per group

- num_cond:

  Number of conditions

- num_perm:

  Number of permutations to generate (includes identity as first)

- not_in_cond:

  Logical, if TRUE don't permute conditions within group

## Value

Matrix of permutation orders (total_rows x num_perm)
