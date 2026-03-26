# Compute P-values from Distribution

Computes p-values by comparing observed values to a null distribution.

## Usage

``` r
pls_pvalue(observed, null_distrib, tail = "two")
```

## Arguments

- observed:

  Observed values (vector)

- null_distrib:

  Null distribution matrix (n_perm x n_elements)

- tail:

  "two" (default), "upper", or "lower"

## Value

Vector of p-values
