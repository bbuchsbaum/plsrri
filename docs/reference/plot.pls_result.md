# Plot S3 Method for pls_result

Generic plot method for PLS results.

## Usage

``` r
# S3 method for class 'pls_result'
plot(x, what = "singular_values", ...)
```

## Arguments

- x:

  A `pls_result` object

- what:

  What to plot: "singular_values", "scores", "loadings", "brain"

- ...:

  Additional arguments passed to specific plot functions

## Value

A ggplot object
