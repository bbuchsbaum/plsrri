# Plot Brain Overlay

Convenience function for overlaying PLS results on an anatomical
background. This is a thin wrapper around `neuroim2::plot_overlay`.

## Usage

``` r
plot_brain_overlay(
  x,
  background,
  lv = 1,
  what = "bsr",
  lag = NULL,
  threshold = 2,
  slices = NULL,
  along = 3L,
  ov_cmap = "inferno",
  ov_alpha = 0.7,
  ncol = 3L,
  title = NULL,
  ...
)
```

## Arguments

- x:

  A `pls_result` object

- background:

  A NeuroVol to use as the anatomical background

- lv:

  Latent variable to plot (default 1)

- what:

  What to plot: "salience" or "bsr"

- lag:

  Optional lag label to plot for voxel×lag feature layouts (e.g., 0).

- threshold:

  Threshold for overlay (e.g., 3 for \|BSR\| \> 3)

- slices:

  Slice indices (NULL = auto)

- along:

  Axis for slicing (3 = axial, 2 = coronal, 1 = sagittal)

- ov_cmap:

  Color map for overlay (default "inferno")

- ov_alpha:

  Overlay transparency (default 0.7)

- ncol:

  Number of columns

- title:

  Plot title

- ...:

  Additional arguments passed to `neuroim2::plot_overlay`

## Value

A grid of ggplot panels
