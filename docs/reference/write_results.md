# Write Results to File

Exports PLS results to various file formats.

## Usage

``` r
write_results(x, file, format = "rds", what = "all", lv = NULL)
```

## Arguments

- x:

  A `pls_result` object

- file:

  Output file path

- format:

  Export format: "rds", "csv", "mat"

- what:

  What to export: "all", "salience", "bsr", "scores"

- lv:

  LV indices to export (NULL = all)

## Value

Path to exported file (invisibly)
