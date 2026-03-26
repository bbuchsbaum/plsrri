# Compute Seed/ROI Data Matrix from a Label Volume

Given voxelwise data matrices and a mask, compute one column per ROI
(voxel layout) or one column per ROI-by-lag (voxel-by-lag layout) as the
mean within the ROI for each observation (row).

## Usage

``` r
pls_seed_data_from_labels(
  datamat_lst,
  mask,
  labels,
  roi_ids = NULL,
  roi_labels = NULL,
  lags = NULL,
  feature_layout = NULL,
  na_rm = TRUE,
  drop_zero_var = TRUE
)
```

## Arguments

- datamat_lst:

  List of group matrices (rows = observations).

- mask:

  3D brain mask volume/array. Non-zero values define included voxels.

- labels:

  3D label volume/array with integer ROI labels (0 = background).

- roi_ids:

  Optional integer vector of ROI IDs to extract (defaults to all
  non-zero labels).

- roi_labels:

  Optional character labels for ROIs (defaults to `seed_<id>`).

- feature_layout:

  Optional feature layout list (e.g., from `pls_result$feature_layout`).

- na_rm:

  Logical, passed to
  [`rowMeans()`](https://rdrr.io/r/base/colSums.html).

- drop_zero_var:

  Logical, drop seed columns with zero/NA variance (common for baseline
  lag).

## Value

Numeric matrix with nrow = sum(nrow(datamat_lst)).
