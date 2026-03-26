# Convert a pls_result to mva_result

Maps the native PLS result fields (u, s, v, usc, vsc) to the universal
`mva_decomposition` / `mva_result` shape. The original pls_result fields
are preserved in `extra` for backward compatibility.

## Usage

``` r
pls_result_to_mva_result(pls_res, spec = NULL, method_name = NULL)
```

## Arguments

- pls_res:

  A `pls_result` object from
  [`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)

- spec:

  The spec used (optional, for metadata)

- method_name:

  Character, method name for class dispatch

## Value

An `mva_result` object
