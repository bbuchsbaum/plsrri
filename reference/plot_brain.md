# Plot Brain Salience or BSR Map

Creates brain slice plots for PLS saliences or bootstrap ratios. Uses
neuroim2's plotting functions for visualization.

## Usage

``` r
plot_brain(
  x,
  lv = 1,
  what = "bsr",
  lag = NULL,
  threshold = NULL,
  view = "montage",
  background = NULL,
  slices = NULL,
  along = 3L,
  cmap = "inferno",
  ncol = 6L,
  title = NULL,
  ...
)
```

## Arguments

- x:

  A `pls_result` object

- lv:

  Latent variable to plot (default 1)

- what:

  What to plot: "salience" or "bsr"

- lag:

  Optional lag label to plot for voxel×lag feature layouts (e.g., 0).

- threshold:

  Threshold for BSR (e.g., 3 for \|BSR\| \> 3)

- view:

  View type: "montage" (default), "ortho", or "overlay"

- background:

  For "overlay" view: a NeuroVol to use as background. If NULL and
  overlay is requested, falls back to montage.

- slices:

  Slice indices (NULL = auto, passed to neuroim2)

- along:

  Axis for slicing (3 = axial, 2 = coronal, 1 = sagittal)

- cmap:

  Color map name (default "inferno" for statistical maps)

- ncol:

  Number of columns for montage layout

- title:

  Plot title (NULL = auto-generate)

- ...:

  Additional arguments passed to neuroim2 plotting functions

## Value

A ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- quick_pls(list(d1, d2), c(20, 18), 3, nboot = 100)
plot_brain(result, lv = 1, what = "bsr", threshold = 3)
plot_brain(result, lv = 1, what = "bsr", view = "ortho")
} # }
```
