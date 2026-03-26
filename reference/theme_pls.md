# PLS Plot Theme

A clean, modern ggplot2 theme suitable for neuroimaging publications.

## Usage

``` r
theme_pls(
  base_size = 12,
  base_family = "",
  base_line_size = 0.5,
  base_rect_size = 0.5
)
```

## Arguments

- base_size:

  Base font size (default 12)

- base_family:

  Base font family (default "")

- base_line_size:

  Base line size (default 0.5)

- base_rect_size:

  Base rect size (default 0.5)

## Value

A ggplot2 theme object

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_pls()
```
