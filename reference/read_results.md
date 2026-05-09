# Read a PLS Result From Disk

Symmetric reader for files written by
[`write_results()`](https://bbuchsbaum.github.io/plsrri/reference/write_results.md).
Auto-detects the on-disk format by file content (HDF5 magic bytes vs. R
serialized format), so a path written via either format round-trips
correctly.

For HDF5 inputs, the schema (see `?plsrri-hdf5`) supports lossless
round-trip of all six PLS methods including multiblock layouts,
permutation null distributions (when retained via
`pls_analysis(..., keep_perm_distribution = TRUE)`), bootstrap and
split-half results, NeuroVol masks, and site/diagnostics metadata.

## Usage

``` r
read_results(file)
```

## Arguments

- file:

  Path to a `.rds` or `.h5`/`.hdf5` file.

## Value

A `pls_result` object.

## See also

[`write_results()`](https://bbuchsbaum.github.io/plsrri/reference/write_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- pls_analysis(...)
write_results(fit, "fit.h5", format = "hdf5")
fit2 <- read_results("fit.h5")
identical(fit, fit2)
} # }
```
