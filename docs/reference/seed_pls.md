# Seed PLS (Functional Connectivity)

Convenience function for seed-based PLS analysis, commonly used for
functional connectivity. This is Behavior PLS (method 3) where the
"behavior" data is the time series from a seed region.

The result shows which voxels correlate/covary with the seed region,
potentially differently across conditions.

## Usage

``` r
seed_pls(
  datamat_lst,
  seed_data,
  num_subj_lst,
  num_cond,
  cormode = 0L,
  nperm = 1000,
  nboot = 500,
  ...
)
```

## Arguments

- datamat_lst:

  List of data matrices (one per group). Each matrix has rows = subjects
  x conditions, columns = voxels/features.

- seed_data:

  Matrix of seed region data. Rows must match stacked datamat (subjects
  x conditions across all groups), columns are seed regions/voxels.

- num_subj_lst:

  Integer vector with number of subjects per group.

- num_cond:

  Number of conditions.

- cormode:

  Correlation mode:

  0

  :   Pearson correlation (default)

  2

  :   Covariance

  4

  :   Cosine angle

  6

  :   Dot product

- nperm:

  Number of permutations (default 1000).

- nboot:

  Number of bootstrap samples (default 500).

- ...:

  Additional arguments passed to
  [`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md).

## Value

A `pls_result` object with class `pls_behavior`.

## Details

Seed PLS identifies brain patterns that maximally correlate with
activity in a seed region. Unlike standard functional connectivity, it
can reveal condition-specific connectivity patterns.

To run seed PLS: 1 . Extract time series from your seed region(s) 2.
Organize as a matrix: rows = observations (matching datamat), columns =
seed(s) 3. Pass to this function

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract seed time series (e.g., from PCC)
seed_ts <- extract_roi(bold_data, pcc_mask)

# Run seed PLS
result <- seed_pls(
  datamat_lst = list(brain_data),
  seed_data = seed_ts,
  num_subj_lst = 25,
  num_cond = 3
)

# View connectivity patterns
plot_brain(result, lv = 1, what = "bsr", threshold = 3)
} # }
```
