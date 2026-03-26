# Plot Singular Values

Creates a bar plot of singular values (variance explained).

## Usage

``` r
plot_singular_values(
  x,
  show_pvalue = TRUE,
  alpha = 0.05,
  cumulative = FALSE,
  title = NULL
)
```

## Arguments

- x:

  A `pls_result` object

- show_pvalue:

  Show p-values from permutation test

- alpha:

  Significance threshold for coloring

- cumulative:

  Show cumulative variance explained

- title:

  Plot title

## Value

A ggplot object
