# Distribution-Based Confidence Intervals (MATLAB rri_distrib)

Port of MATLAB `rri_distrib.m`. Computes percentile confidence intervals
and bias-corrected adjusted intervals for skewed bootstrap
distributions.

## Usage

``` r
pls_distrib_ci(distrib, ll, ul, num_boot, climNi, orig)
```

## Arguments

- distrib:

  3D array with dimensions (r x c x (num_boot+1)). Slice 1 is the
  original statistic; slices 2..(num_boot+1) are bootstrap samples.

- ll:

  Lower percentile (e.g., 100 - clim)

- ul:

  Upper percentile (e.g., clim)

- num_boot:

  Number of bootstrap samples (not counting the original)

- climNi:

  Two-tailed alpha/2 as a probability (e.g., 0.025 for 95% CI)

- orig:

  Original statistic matrix (r x c)

## Value

List with `ll`, `ul`, `prop`, `ll_adj`, `ul_adj`
