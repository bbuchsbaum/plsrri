# Extract Stability Estimates

Returns bootstrap-based stability for feature weights. For PLS, these
are bootstrap ratios (BSR = salience / SE). Other methods may use
different stability metrics.

## Usage

``` r
stability(x, k = NULL, threshold = NULL, as_neurovol = FALSE, ...)
```

## Arguments

- x:

  An `mva_result` or `pls_result` object

- k:

  Component index or vector (NULL = all)

- threshold:

  Optional threshold for masking

- as_neurovol:

  Convert to NeuroVol if mask available

- ...:

  Additional arguments

## Value

Matrix of stability values, or NeuroVol
