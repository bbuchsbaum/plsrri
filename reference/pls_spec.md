# Create a PLS Specification Object

Creates an empty PLS specification that can be populated via the builder
API. This is the starting point for defining a PLS analysis.

## Usage

``` r
pls_spec(mask = NULL)
```

## Arguments

- mask:

  Optional brain mask (NeuroVol from neuroim2)

## Value

A `pls_spec` object

## Examples

``` r
spec <- pls_spec()
```
