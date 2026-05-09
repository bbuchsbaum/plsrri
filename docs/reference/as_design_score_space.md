# Two-LV Design Score Space

Extract design-score centroids for two latent variables. This is useful
for seeing how the flattened design pattern separates across more than
one interpretable LV.

## Usage

``` r
as_design_score_space(
  x,
  lv = c(1L, 2L),
  condition_key = NULL,
  formula = NULL,
  block = NULL
)
```

## Arguments

- x:

  A `pls_result` object.

- lv:

  Integer vector of two latent variables.

- condition_key:

  Optional data frame with one row per condition and a `condition`
  column.

- formula:

  Optional one-sided formula controlling aggregation and faceting.
  Variables before `|` identify plotted points; variables after `|`
  identify facets. Variables omitted from the formula are averaged over.
  For example, `~ group + level | task` plots group-by-level centroids
  separately within each task.

- block:

  Optional score block to keep for multiblock results.

## Value

A data frame with one row per requested centroid.
