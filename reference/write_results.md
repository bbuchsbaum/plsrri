# Write Results to File

Exports PLS results to various file formats.

## Usage

``` r
write_results(
  x,
  file,
  format = "rds",
  what = "all",
  lv = NULL,
  compression = 4L
)
```

## Arguments

- x:

  A `pls_result` object

- file:

  Output file path

- format:

  Export format: `"rds"` (default, native R), `"hdf5"` (portable,
  lossless plsrri-native HDF5; see `?plsrri-hdf5`), `"csv"`, or `"mat"`
  (MATLAB-readable, partial fields only).

- what:

  What to export: "all", "salience", "bsr", "scores". Only honored for
  `"rds"` and `"csv"` formats; `"hdf5"` always writes the full result.

- lv:

  LV indices to export (NULL = all)

- compression:

  Integer 0-9 gzip level for `"hdf5"` (default 4). Higher values produce
  smaller files at write-time cost.

## Value

Path to exported file (invisibly)

## See also

[`read_results()`](https://bbuchsbaum.github.io/plsrri/reference/read_results.md)
