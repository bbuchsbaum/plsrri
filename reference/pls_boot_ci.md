# Compute Bootstrap Confidence Intervals

Computes confidence intervals from bootstrap distribution.

## Usage

``` r
pls_boot_ci(boot_distrib, clim = 95)
```

## Arguments

- boot_distrib:

  Matrix of bootstrap values (n_boot x n_elements)

- clim:

  Confidence level (0-100)

## Value

List with lower and upper bounds
