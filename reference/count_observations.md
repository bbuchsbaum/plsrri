# Count Observations Implied by Group/Condition Structure

Count Observations Implied by Group/Condition Structure

## Usage

``` r
count_observations(num_subj_lst, num_cond)
```

## Arguments

- num_subj_lst:

  Subjects per group. May be a numeric vector for balanced designs or a
  list of per-condition subject-count vectors for ssb designs.

- num_cond:

  Number of conditions.

## Value

Integer count of total observations/rows implied by the design.
