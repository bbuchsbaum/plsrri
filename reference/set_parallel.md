# Set Parallel Processing Options

Configures parallel processing for permutation and bootstrap tests.

## Usage

``` r
set_parallel(spec, workers = NULL, backend = "future")
```

## Arguments

- spec:

  A `pls_spec` object

- workers:

  Number of parallel workers (NULL = auto-detect)

- backend:

  Parallel backend: "future" (default) or "sequential"

## Value

Updated `pls_spec` object
