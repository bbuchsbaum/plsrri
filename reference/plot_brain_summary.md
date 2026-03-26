# Plot Brain Summary

Creates a summary showing brain maps for all significant LVs. Uses
`neuroim2::plot_montage` for each significant LV.

## Usage

``` r
plot_brain_summary(
  x,
  p_threshold = 0.05,
  bsr_threshold = 3,
  n_slices = 9,
  lag = NULL,
  ...
)
```

## Arguments

- x:

  A `pls_result` object

- p_threshold:

  P-value threshold for significance (default 0.05)

- bsr_threshold:

  BSR threshold for masking (default 3)

- n_slices:

  Number of slices per LV (default 9)

- lag:

  Optional lag label to plot for voxel×lag feature layouts (e.g., 0).

- ...:

  Additional arguments passed to `neuroim2::plot_montage`

## Value

A list of plots (one per significant LV), or NULL if no significant LVs
