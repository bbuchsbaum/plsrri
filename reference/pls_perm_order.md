# Generate Permutation Orders

Generates permutation reordering indices for PLS permutation testing.
The method permutes conditions within subjects, then permutes subjects
across groups.

## Usage

``` r
pls_perm_order(num_subj_lst, num_cond, num_perm, not_in_cond = FALSE)
```

## Arguments

- num_subj_lst:

  Integer vector of subjects per group

- num_cond:

  Number of conditions

- num_perm:

  Number of permutations to generate

- not_in_cond:

  Logical, if TRUE don't permute conditions within group (for structure
  PLS)

## Value

Matrix of permutation orders (total_rows x num_perm)

## Examples

``` r
perm_order <- pls_perm_order(c(10, 12), num_cond = 3, num_perm = 100)
```
