# Generate Bootstrap Orders

Generates bootstrap resampling orders for PLS bootstrap testing.
Resampling is done within groups.

## Usage

``` r
pls_boot_order(
  num_subj_lst,
  num_cond,
  num_boot,
  bscan = NULL,
  incl_seq = FALSE,
  boot_type = "strat"
)
```

## Arguments

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- num_boot:

  Number of bootstrap samples

- bscan:

  Conditions for behavior block

- incl_seq:

  Include sequential order

- boot_type:

  "strat" (stratified) or "nonstrat" (non-stratified)

## Value

Matrix of bootstrap orders (total_rows x num_boot)
