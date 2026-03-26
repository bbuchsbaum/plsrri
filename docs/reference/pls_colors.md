# PLS Color Palettes

Color palettes designed for PLS visualizations.

## Usage

``` r
pls_colors(n = 5, palette = "diverging", reverse = FALSE)
```

## Arguments

- n:

  Number of colors needed

- palette:

  Palette name: "diverging", "sequential", "qualitative"

- reverse:

  Logical, reverse palette order

## Value

Character vector of hex colors

## Examples

``` r
pls_colors(5, "diverging")
#> [1] "#2166AC" "#92C5DE" "#F7F7F7" "#F4A582" "#B2182B"
```
