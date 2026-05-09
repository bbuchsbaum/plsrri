# Plot a Two-LV Design Score Space

Plot group-condition design-score centroids with one latent variable on
each axis. Labels identify the design cells, while colors or shapes can
encode grouping factors.

## Usage

``` r
plot_design_score_space(
  x,
  lv = c(1L, 2L),
  condition_key = NULL,
  formula = NULL,
  label = NULL,
  label_sep = "\n",
  color = NULL,
  shape = NULL,
  facet = NULL,
  show_origin = TRUE,
  show_segments = TRUE,
  show_se = FALSE,
  aspect = c("fixed", "free"),
  padding = 0.2,
  title = NULL,
  block = NULL
)
```

## Arguments

- x:

  A `pls_result` object.

- lv:

  Integer vector of two latent variables.

- condition_key:

  Optional condition metadata.

- formula:

  Optional one-sided formula controlling aggregation and faceting.
  Variables before `|` identify plotted points; variables after `|`
  identify facets. Variables omitted from the formula are averaged over.
  For example, `~ group + condition | task` labels group-condition
  centroids in task facets, while `~ group | task` collapses over
  condition labels inside each task.

- label:

  Character vector of columns to paste into point labels. Defaults to
  `group` and `condition` when a group column is available.

- label_sep:

  Separator used when `label` has multiple columns.

- color:

  Optional column name for point color. Defaults to `group` when
  available.

- shape:

  Optional column name for point shape.

- facet:

  Optional column name or character vector used for faceting. Defaults
  to the variables after `|` in `formula`.

- show_origin:

  Logical; draw zero reference lines.

- show_segments:

  Logical; draw segments from the origin to each centroid.

- show_se:

  Logical; draw standard-error bars in both LV dimensions.

- aspect:

  Aspect handling. `"fixed"` uses the same numeric scale for both axes;
  `"free"` lets the panel fill the available device.

- padding:

  Fractional padding added around the joint x/y range. This keeps labels
  visible when points sit near the edge of the score space.

- title:

  Optional plot title.

- block:

  Optional score block to keep for multiblock results.

## Value

A ggplot object.
