# Cell Table for a Task PLS Factorial Design

Builds the group-condition cell table whose rows match the row order of
the Task PLS cross-block matrix: all conditions for group 1, then all
conditions for group 2, and so on.

## Usage

``` r
design_cell_table(x, design = NULL, condition_key = NULL)
```

## Arguments

- x:

  A `pls_result`.

- design:

  Optional `pls_design` object.

- condition_key:

  Optional condition metadata. Ignored when supplied by `design`.

## Value

A data frame with one row per Task PLS design cell.
