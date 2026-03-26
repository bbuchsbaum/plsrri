# Plot Orthogonal Brain Views

Convenience function for orthogonal (3-plane) visualization of PLS
results. This is a thin wrapper around
[`neuroim2::plot_ortho`](https://bbuchsbaum.github.io/neuroim2/reference/plot_ortho.html).

## Usage

``` r
plot_brain_ortho(
  x,
  lv = 1,
  what = "bsr",
  lag = NULL,
  threshold = NULL,
  coord = NULL,
  unit = "index",
  cmap = "inferno",
  crosshair = TRUE,
  annotate = TRUE,
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

  Threshold for masking (e.g., 3 for \|BSR\| \> 3)

- coord:

  Coordinate for crosshairs (NULL = center of volume)

- unit:

  Coordinate unit: "index" (voxels) or "mm"

- cmap:

  Color map (default "inferno")

- crosshair:

  Show crosshairs (default TRUE)

- annotate:

  Show orientation labels (default TRUE)

- ...:

  Additional arguments passed to
  [`neuroim2::plot_ortho`](https://bbuchsbaum.github.io/neuroim2/reference/plot_ortho.html)

## Value

A list of ggplot panels (axial, coronal, sagittal)
