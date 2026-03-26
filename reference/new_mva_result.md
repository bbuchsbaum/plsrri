# Create a Multivariate Analysis Result

Wraps an `mva_decomposition` with inference results (permutation,
bootstrap, split-half) and metadata. This is the top-level result object
returned by the method-neutral analysis path.

## Usage

``` r
new_mva_result(
  decomposition,
  perm_result = NULL,
  boot_result = NULL,
  split_result = NULL,
  spec = NULL,
  method = NULL,
  mask = NULL
)
```

## Arguments

- decomposition:

  An `mva_decomposition` object

- perm_result:

  Permutation test result, or NULL

- boot_result:

  Bootstrap result, or NULL

- split_result:

  Split-half result, or NULL

- spec:

  The analysis specification used

- method:

  Character string identifying the method

- mask:

  Brain mask (NeuroVol), or NULL

## Value

An `mva_result` object with dual class for method dispatch
