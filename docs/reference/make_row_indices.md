# Create Index Vector for Subject-Condition Structure

Creates row indices for the subject-in-condition-in-group data
structure.

## Usage

``` r
make_row_indices(num_subj_lst, num_cond, group = NULL, condition = NULL)
```

## Arguments

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- group:

  Group index

- condition:

  Condition index (NULL = all)

## Value

Integer vector of row indices
