# Normalize Rows to Unit Length

Normalizes each row of a matrix to have unit L2 norm. Used in multiblock
PLS to balance task and behavior blocks.

## Usage

``` r
normalize_rows(X, margin = 1L)
```

## Arguments

- X:

  Matrix to normalize

- margin:

  1 for rows (default), 2 for columns

## Value

Normalized matrix
