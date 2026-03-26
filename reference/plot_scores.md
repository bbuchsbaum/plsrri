# Plot PLS Scores

Creates scatter plots, bar plots, or trajectory plots of PLS scores.

## Usage

``` r
plot_scores(
  x,
  lv = 1,
  type = "design",
  plot_type = "bar",
  group_by = NULL,
  color_by = NULL,
  facet_by = NULL,
  show_ci = TRUE,
  title = NULL
)
```

## Arguments

- x:

  A `pls_result` object

- lv:

  Latent variable to plot (default 1)

- type:

  Score type: "brain" or "design"

- plot_type:

  Plot style: "scatter", "bar", "violin", "box"

- group_by:

  Grouping variable for colors/facets

- color_by:

  Variable for color mapping

- facet_by:

  Variable for faceting

- show_ci:

  Show confidence intervals if available

- title:

  Plot title

## Value

A ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- quick_pls(list(d1, d2), c(20, 18), 3)
plot_scores(result, lv = 1, type = "design", plot_type = "bar")
} # }
```
