# Factorial Design Object for Task PLS

Defines a factorial design over Task PLS group-condition cells. The
design object stores the cell-level formula and condition metadata used
by design-subspace decomposition and observed subspace statistics.

## Usage

``` r
pls_design(
  formula,
  condition_key = NULL,
  subject = NULL,
  between = NULL,
  within = NULL,
  contrasts = "sum"
)
```

## Arguments

- formula:

  One-sided model formula, such as `~ group * task * level`.

- condition_key:

  Optional data frame with a `condition` column and one row per PLS
  condition. Additional columns define condition-level factors.

- subject:

  Optional subject identifier column name for future
  exchangeability-aware resampling.

- between:

  Optional between-subject factor names.

- within:

  Optional within-subject factor names.

- contrasts:

  Contrast coding. Currently `"sum"` is used for unordered factors.

## Value

A `pls_design` object.
