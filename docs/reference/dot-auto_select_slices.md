# Auto-Select Slices with Content

Selects slices that contain non-NA/non-zero values for optimal
visualization.

## Usage

``` r
.auto_select_slices(vol, n_slices = 9, along = 3L)
```

## Arguments

- vol:

  A NeuroVol

- n_slices:

  Number of slices to select

- along:

  Axis (default 3 = axial)

## Value

Integer vector of slice indices
