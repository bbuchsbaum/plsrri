# Within-Subject Seed PLS Analysis

Convenience function for within-subject seed-based PLS (ws-seed PLS).
Computes within-subject seed-voxel correlations from trial-level beta
estimates, then runs task PLS on the resulting connectivity maps.

This implements the ws-seed PLS method from Roberts et al. (2016), which
captures *temporal* co-fluctuation between seed and target regions
within each individual, avoiding the Simpson's Paradox that can arise
with standard across-subject seed PLS.

## Usage

``` r
ws_seed_pls(
  beta_lst,
  seed_lst,
  condition_lst,
  num_subj_lst,
  groups = NULL,
  seed_labels = NULL,
  layout = c("seed_condition", "stacked_seed_features"),
  fisher_z = TRUE,
  min_trials = 3L,
  nonrotated = FALSE,
  meancentering_type = 0L,
  nperm = 1000,
  nboot = 500,
  nsplit = 0,
  clim = 95,
  boot_type = "strat",
  is_struct = FALSE,
  progress = TRUE,
  stacked_designdata = NULL,
  ...
)
```

## Arguments

- beta_lst:

  A list of matrices (one per subject). Each matrix has rows = trials,
  columns = voxels. Typically produced by
  [`fmrilss::lss()`](https://bbuchsbaum.github.io/fmrilss/reference/lss.html).

- seed_lst:

  A list of numeric vectors or matrices (one per subject). Each contains
  the seed region's trial-level betas. If a matrix, each column is a
  separate seed.

- condition_lst:

  A list of factors or integer vectors (one per subject) mapping trials
  to experimental conditions.

- num_subj_lst:

  Integer vector with number of subjects per group. For a single group,
  just the total number of subjects. For multiple groups, a vector
  (e.g., `c(12, 14)`). For SSB designs, supply a list of integer vectors
  giving subject counts per condition within group.

- groups:

  Optional subject-level group assignments for trial inputs. Required
  when `num_subj_lst` uses SSB/list form because subject-group
  membership cannot be recovered from per-condition counts alone.

- fisher_z:

  Logical; apply Fisher r-to-z transform (default `TRUE`).

- min_trials:

  Minimum trials per condition for valid correlation (default 3).

- nonrotated:

  Logical; if `TRUE`, use non-rotated task PLS (method 2). Default
  `FALSE` uses standard rotated task PLS (method 1).

- meancentering_type:

  Mean-centering for task PLS:

  0

  :   Within-group centering (default)

  1

  :   Grand condition mean removal

  2

  :   Grand mean removal

  3

  :   All main effects removal

- nperm:

  Number of permutations (default 1000).

- nboot:

  Number of bootstrap samples (default 500).

- ...:

  Additional arguments passed to
  [`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md).

## Value

A `pls_result` object (class `pls_task` or `pls_task_nonrot`). The brain
saliences (singular vectors) represent voxel patterns of within-subject
connectivity with the seed. Bootstrap ratios index reliability of each
voxel's connectivity.

## Details

The analysis proceeds in two stages:

1.  **Within-subject correlation**: For each subject and condition,
    Pearson correlations are computed between the seed's trial-level
    betas and every voxel's trial-level betas. This produces one
    connectivity map per subject per condition.

2.  **Task PLS**: The connectivity maps are stacked and submitted to
    task PLS, which identifies latent variables capturing
    condition-dependent connectivity patterns that are reliable across
    subjects.

Trial-level betas are typically estimated using Least Squares Separate
(LSS) via
[`fmrilss::lss()`](https://bbuchsbaum.github.io/fmrilss/reference/lss.html),
which provides stable per-trial activation estimates even with rapid
event-related designs.

## References

Roberts, R. P., Hach, S., Tippett, L. J., & Addis, D. R. (2016). The
Simpson's paradox and fMRI: Similarities and differences between
functional connectivity measures derived from within-subject and
across-subject correlations. *NeuroImage*.
[doi:10.1016/j.neuroimage.2016.04.028](https://doi.org/10.1016/j.neuroimage.2016.04.028)

## See also

[`seed_pls`](https://bbuchsbaum.github.io/plsrri/reference/seed_pls.md)
for standard across-subject seed PLS,
[`ws_seed_correlation`](https://bbuchsbaum.github.io/plsrri/reference/ws_seed_correlation.md)
for the correlation computation step,
[`pls_analysis`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)
for the underlying PLS engine.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Estimate trial-level betas with fmrilss
betas <- fmrilss::lss(Y, X, Nuisance = nuisance_mat)

# 2. Extract seed betas (e.g., mean of voxels in PCC ROI)
seed_betas <- rowMeans(betas[, pcc_voxel_indices])

# 3. Run ws-seed PLS
result <- ws_seed_pls(
  beta_lst = list(betas_subj1, betas_subj2, ...),
  seed_lst = list(seed_subj1, seed_subj2, ...),
  condition_lst = list(cond_subj1, cond_subj2, ...),
  num_subj_lst = 16
)

# 4. Inspect results
significance(result)
plot_scores(result, lv = 1)
} # }
```
