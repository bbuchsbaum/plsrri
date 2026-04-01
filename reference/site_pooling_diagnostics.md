# Site-Pooling Diagnostics for Multisite PLS

Optional post-fit diagnostics for pooled multisite analyses. When
observation- or subject-level site labels are available, these summaries
quantify how stable a pooled solution is across sites.

## Usage

``` r
site_pooling_diagnostics(
  x,
  site = NULL,
  spec = NULL,
  progress = FALSE,
  infer = c("none", "score", "full"),
  nperm = 199L,
  nboot = 499L,
  subspace_k = NULL,
  conf = 0.95
)
```

## Arguments

- x:

  A fitted `pls_result`.

- site:

  Optional site labels. May have length equal to the number of
  observations or the number of subjects. If omitted, `x$site` is used.

- spec:

  Optional `pls_spec` used to fit `x`. This is required for
  site-specific and leave-one-site-out reruns when `x` does not already
  carry enough input context.

- progress:

  Logical; emit informational messages during reruns.

- infer:

  One of `"none"`, `"score"`, or `"full"`. `"score"` adds subject-block
  bootstrap intervals and a subject-block permutation heterogeneity test
  for sitewise LV score correlations. `"full"` additionally compares
  site-specific PLS subspaces against the pooled fit using cumulative
  RV-style concordance summaries and site-label permutation p-values.

- nperm:

  Number of permutations used by inferential site diagnostics.

- nboot:

  Number of subject-block bootstrap resamples used for sitewise
  score-correlation intervals.

- subspace_k:

  Maximum number of leading LVs to compare in subspace concordance
  summaries. Defaults to the number of pooled LVs with permutation
  p-values \\\le 0.05\\, or 1 when permutation results are not
  available.

- conf:

  Confidence level for sitewise bootstrap intervals.

## Value

A named list containing pooled observation metadata, sitewise score
summaries, sitewise score correlations, site-specific salience
similarity, and leave-one-site-out rerun summaries. When
`infer != "none"`, the result also contains inferential
score-heterogeneity summaries; when `infer == "full"`, it also contains
site-subspace concordance tests.
