# Plot PLS Loadings

Creates bar plots or dot plots of PLS loadings.

## Usage

``` r
plot_loadings(
  x,
  lv = 1,
  type = "design",
  plot_type = "bar",
  show_ci = TRUE,
  threshold = NULL,
  sort = TRUE,
  title = NULL
)
```

## Arguments

- x:

  A `pls_result` object

- lv:

  Latent variable to plot (default 1)

- type:

  Loading type: "design" or "behavior"

- plot_type:

  Plot style: "bar" or "dot"

- show_ci:

  Show confidence intervals if available

- threshold:

  Minimum \|loading\| to show (for decluttering)

- sort:

  Sort loadings by value

- title:

  Plot title

## Value

A ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
result <- quick_pls(list(d1, d2), c(20, 18), 3)
plot_loadings(result, lv = 1, type = "design")
} # }
```
