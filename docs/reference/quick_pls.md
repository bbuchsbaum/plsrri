# Quick Run for Simple Analyses

Convenience function for running simple task PLS without full builder
API.

## Usage

``` r
quick_pls(datamat_lst, num_subj_lst, num_cond, nperm = 1000, nboot = 500, ...)
```

## Arguments

- datamat_lst:

  List of data matrices

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- nperm:

  Number of permutations

- nboot:

  Number of bootstrap samples

- ...:

  Additional arguments to pls_analysis()

## Value

A `pls_result` object
