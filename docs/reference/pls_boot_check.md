# Check Bootstrap Feasibility

Checks if exhaustive bootstrap enumeration is possible (for small
samples) and computes the minimum subjects per group.

## Usage

``` r
pls_boot_check(num_subj_lst, num_cond, num_boot, incl_seq = FALSE)
```

## Arguments

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- num_boot:

  Requested bootstrap samples

- incl_seq:

  Include sequential order in samples

## Value

List with min_subj_per_group, is_boot_samples, boot_samples,
new_num_boot
