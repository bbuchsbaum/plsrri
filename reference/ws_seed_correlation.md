# Compute Within-Subject Seed-Voxel Correlation Maps

For each subject and condition, computes Pearson correlations between a
seed region's trial-level betas and every voxel's trial-level betas.
Returns a stacked matrix suitable for task PLS.

## Usage

``` r
ws_seed_correlation(
  beta_lst,
  seed_lst,
  condition_lst,
  fisher_z = TRUE,
  min_trials = 3L
)
```

## Arguments

- beta_lst:

  A list of matrices (one per subject). Each matrix has rows = trials,
  columns = voxels. All matrices must have the same number of columns
  (voxels). Trial counts may vary across subjects.

- seed_lst:

  A list of numeric vectors or single-column matrices (one per subject).
  Each element contains the seed region's trial-level betas, with length
  equal to the number of trials (rows) for that subject. If a matrix
  with multiple columns is provided, each column is treated as a
  separate seed and correlations are computed independently for each.

- condition_lst:

  A list of factors or integer vectors (one per subject). Each element
  maps trials to conditions (same length as rows in the corresponding
  beta matrix). Condition labels must be consistent across subjects.

- fisher_z:

  Logical; if `TRUE` (default), apply Fisher's r-to-z transform to the
  within-subject correlations before stacking.

- min_trials:

  Integer; minimum number of trials per condition required to compute a
  valid correlation (default 3). Conditions with fewer trials produce
  `NA` values.

## Value

A list with components:

- datamat:

  A matrix with rows = subjects x conditions (stacked in
  condition-within-subject order, i.e., all conditions for subject 1,
  then all conditions for subject 2, etc.) and columns = voxels (x
  n_seeds if multiple seeds). For multiple seeds, columns are ordered as
  all voxels for seed 1, then all voxels for seed 2, etc.

- num_cond:

  Integer; number of conditions.

- cond_labels:

  Character vector of condition labels.

- n_subjects:

  Integer; number of subjects.

- n_seeds:

  Integer; number of seed regions.

- n_voxels:

  Integer; number of voxels per seed.

## Details

The stacking order places conditions within subjects (matching the
convention used by
[`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)):

      row 1:  subject 1, condition 1
      row 2:  subject 1, condition 2
      ...
      row k:  subject 1, condition k
      row k+1: subject 2, condition 1
      ...

## Examples

``` r
# Simulate trial-level data for 10 subjects, 2 conditions, 50 voxels
set.seed(42)
n_subj <- 10; n_trials <- 24; n_vox <- 50; n_cond <- 2
beta_lst <- lapply(seq_len(n_subj), function(i) matrix(rnorm(n_trials * n_vox), n_trials, n_vox))
seed_lst <- lapply(seq_len(n_subj), function(i) rnorm(n_trials))
cond_lst <- lapply(seq_len(n_subj), function(i) rep(1:n_cond, each = n_trials / n_cond))

ws <- ws_seed_correlation(beta_lst, seed_lst, cond_lst)
dim(ws$datamat)  # 20 x 50
#> [1] 20 50
```
