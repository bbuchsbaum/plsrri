# Compute Bootstrap Ratio

Computes bootstrap ratio (u / SE) from accumulated sums.

## Usage

``` r
compute_bsr_cpp(u_sum, u_sq_sum, observed_u, num_boot)
```

## Arguments

- u_sum:

  Sum of bootstrap u

- u_sq_sum:

  Sum of squared bootstrap u

- observed_u:

  Observed u

- num_boot:

  Number of bootstrap samples

## Value

Bootstrap ratio matrix
