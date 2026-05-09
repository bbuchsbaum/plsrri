# Factorial Design Contrasts for a Latent Variable

Decompose the summarized LV design scores into an effect-coded factorial
model. The returned magnitudes show how much of the flattened
design-score pattern lies in each main-effect or interaction subspace.

## Usage

``` r
as_design_contrasts(
  x,
  lv = 1L,
  condition_key = NULL,
  factors = NULL,
  block = NULL
)
```

## Arguments

- x:

  A `pls_result` object.

- lv:

  Latent variable to summarize.

- condition_key:

  Optional data frame with one row per condition and a `condition`
  column.

- factors:

  Character vector of factor columns to include. Defaults to `group`
  plus every column supplied by `condition_key` except `condition`.

- block:

  Optional score block to keep for multiblock results.

## Value

A data frame with one row per factorial term.
