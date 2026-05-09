# Tidy Design Scores

Extract design scores from a PLS result and optionally summarize them by
group and condition. A `condition_key` can add design factors, such as
task or difficulty level, back onto the flattened PLS condition labels.

## Usage

``` r
as_design_scores(
  x,
  lv = 1L,
  condition_key = NULL,
  summarize = TRUE,
  block = NULL
)
```

## Arguments

- x:

  A `pls_result` object.

- lv:

  Latent variable to extract.

- condition_key:

  Optional data frame with one row per condition and a `condition`
  column. Additional columns are joined onto the returned table.

- summarize:

  Logical; when `TRUE`, return group-condition means and standard
  errors. When `FALSE`, return observation-level scores.

- block:

  Optional score block to keep for multiblock results. If omitted and a
  task block is present, the task block is used.

## Value

A data frame containing score values and design metadata.
