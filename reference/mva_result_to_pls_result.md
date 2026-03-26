# Convert an mva_result back to pls_result

Reconstructs a `pls_result` from an `mva_result` for backward
compatibility with code that accesses PLS-specific fields directly.

## Usage

``` r
mva_result_to_pls_result(mva_res)
```

## Arguments

- mva_res:

  An `mva_result` object

## Value

A `pls_result` object
