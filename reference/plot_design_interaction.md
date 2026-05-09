# Plot Design Score Interactions

Plot LV design-score means as an interaction plot. This is useful when a
latent variable combines group, task, or condition effects.

## Usage

``` r
plot_design_interaction(
  x,
  lv = 1L,
  condition_key = NULL,
  x_axis = NULL,
  trace = NULL,
  facet = "group",
  show_se = TRUE,
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

- x_axis, trace, facet:

  Column names used for the interaction layout.

- show_se:

  Logical; include standard-error bars.

- title:

  Optional plot title.

- block:

  Optional score block to keep for multiblock results.

## Value

A ggplot object.
