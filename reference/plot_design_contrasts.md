# Plot Factorial Design Contrast Magnitudes

Plot the effect-coded contrast decomposition returned by
[`as_design_contrasts()`](https://bbuchsbaum.github.io/plsrri/reference/as_design_contrasts.md).
Single-degree-of-freedom effects are signed; larger
multi-degree-of-freedom effects are shown as positive magnitudes.

## Usage

``` r
plot_design_contrasts(
  x,
  lv = 1L,
  condition_key = NULL,
  factors = NULL,
  sort = TRUE,
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

- factors:

  Character vector of factor columns to include.

- sort:

  Logical; order effects by absolute contribution.

- title:

  Optional plot title.

- block:

  Optional score block to keep for multiblock results.

## Value

A ggplot object.
