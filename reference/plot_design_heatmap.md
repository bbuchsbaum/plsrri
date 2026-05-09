# Plot Design Scores as a Heatmap

Plot summarized LV design scores after rehydrating flattened condition
labels with a condition key.

## Usage

``` r
plot_design_heatmap(
  x,
  lv = 1L,
  condition_key = NULL,
  row = NULL,
  column = NULL,
  facet = "group",
  show_values = TRUE,
  limits = NULL,
  title = NULL,
  block = NULL
)
```

## Arguments

- x:

  A `pls_result` object.

- lv:

  Latent variable to plot.

- condition_key:

  Optional condition metadata.

- row, column, facet:

  Column names used for the heatmap layout.

- show_values:

  Logical; print rounded scores inside tiles.

- limits:

  Optional fill limits. Defaults to symmetric limits around zero.

- title:

  Optional plot title.

- block:

  Optional score block to keep for multiblock results.

## Value

A ggplot object.
