# Behavior PLS Workflow

``` r
library(plsrri)
```

Behavior PLS finds latent variables that maximally capture the
covariance between brain features and continuous behavioral measures —
reaction time, accuracy, symptom scores, or any per-subject-condition
variable. Where Task PLS asks “which brain patterns separate
conditions?”, Behavior PLS asks “which brain patterns track individual
differences in behavior?”

## What data shape does Behavior PLS expect?

You need two matrices with the same stacked row structure (subjects
within conditions, then groups):

- **Brain matrix** — rows = observations, columns = voxels/features
- **Behavior matrix** — rows = observations, columns = behavioral
  measures

This example has 18 subjects, 2 conditions, 120 voxels, and two
behavioral measures. The synthetic data has a single latent factor
driving both RT and accuracy in opposite directions, plus a
condition-specific voxel block:

``` r
head(round(behav_data, 2))
#>         rt   acc
#> [1,] -0.87  0.57
#> [2,] -0.67 -0.18
#> [3,]  1.15 -1.76
#> [4,] -0.43  0.02
#> [5,] -0.45 -0.38
#> [6,]  1.34 -2.04
```

## How do you run a behavior analysis?

[`behav_pls()`](https://bbuchsbaum.github.io/plsrri/reference/behav_pls.md)
is the convenience wrapper. It runs Behavior PLS (method 3) with the
behavior matrix as the second block:

``` r
behav_result <- behav_pls(
  datamat_lst = list(brain_data),
  behav_data = behav_data,
  num_subj_lst = n_subj,
  num_cond = n_cond,
  nperm = 200,
  nboot = 200,
  progress = FALSE
)
```

Check which latent variables are significant and how much variance each
explains:

``` r
cbind(
  pvalue = round(significance(behav_result), 3),
  variance = round(singular_values(behav_result, normalize = TRUE), 1)
)
#>     pvalue variance
#> LV1      0     96.9
#> LV2      1      2.9
#> LV3      1      0.2
#> LV4      1      0.0
```

LV1 captures the dominant brain-behavior relationship. The scree plot
makes the structure clear:

``` r
plot_singular_values(behav_result)
```

![Singular value scree for the behavior PLS example. LV1
dominates.](behavior-pls_files/figure-html/plot-behavior-scree-1.png)

Singular value scree for the behavior PLS example. LV1 dominates.

## Which behaviors define the first latent variable?

In Behavior PLS, `loadings(..., type = "behavior")` returns the
correlation-style weights linking each behavior-by-condition cell to the
latent variable. Each row is one condition-measure combination.

``` r
round(loadings(behav_result, type = "behavior", lv = 1), 2)
#>       [,1]
#> [1,] -0.98
#> [2,]  0.98
#> [3,] -0.96
#> [4,]  0.96
```

RT and accuracy load with opposite signs, consistent with the planted
relationship — subjects who are faster also tend to be more accurate on
the latent dimension captured by LV1.

``` r
plot_loadings(behav_result, lv = 1, type = "behavior", plot_type = "dot")
```

![Behavior loadings for LV1. RT and accuracy load in opposite
directions, reflecting the shared latent
factor.](behavior-pls_files/figure-html/plot-behavior-loadings-1.png)

Behavior loadings for LV1. RT and accuracy load in opposite directions,
reflecting the shared latent factor.

## How strongly do subjects express that pattern?

Brain scores show how each observation projects onto the latent
variable. In a real analysis, you would check whether the pattern varies
across conditions or groups:

``` r
plot_scores(behav_result, lv = 1, type = "brain", plot_type = "violin")
```

![Brain scores for LV1. The spread reflects individual differences in
the brain-behavior
relationship.](behavior-pls_files/figure-html/plot-behavior-brain-scores-1.png)

Brain scores for LV1. The spread reflects individual differences in the
brain-behavior relationship.

## Are the behavior correlations stable?

For behavior methods, `confidence(..., what = "correlation")` returns
bootstrap confidence intervals on the brain-behavior correlations.
Intervals that exclude zero indicate a reliable relationship:

``` r
corr_ci <- confidence(behav_result, what = "correlation", lv = 1)
round(cbind(lower = corr_ci$lower[, 1], upper = corr_ci$upper[, 1]), 2)
#>      lower upper
#> [1,] -0.95 -0.86
#> [2,]  0.90  0.97
#> [3,] -0.96 -0.84
#> [4,]  0.93  0.99
```

Both measures show intervals that are clearly away from zero, confirming
the stability of the brain-behavior association under resampling.

## Which voxels contribute reliably?

Bootstrap ratios work the same way as in Task PLS — they indicate which
voxels participate reliably in the latent pattern:

``` r
behav_bsr <- bsr(behav_result, lv = 1)
reliable <- sum(abs(behav_bsr) > 2)
```

74 of 120 voxels have $|BSR| > 2$.

![Bootstrap ratio profile. Voxels 1-35 and 36-70 show the planted latent
factor; voxels 71-90 show the condition-specific
signal.](behavior-pls_files/figure-html/plot-behavior-bsr-1.png)

Bootstrap ratio profile. Voxels 1-35 and 36-70 show the planted latent
factor; voxels 71-90 show the condition-specific signal.

The two voxel blocks (1-35 positive, 36-70 negative) mirror the planted
latent structure, while the condition block (71-90) contributes more
weakly.

## Where to go next

| Goal                           | Resource                                                                                                 |
|:-------------------------------|:---------------------------------------------------------------------------------------------------------|
| Task PLS workflow              | [`vignette("plsrri")`](https://bbuchsbaum.github.io/plsrri/articles/plsrri.md)                           |
| Multiblock and seed PLS        | [`vignette("multiblock-and-seed")`](https://bbuchsbaum.github.io/plsrri/articles/multiblock-and-seed.md) |
| Behavior wrapper               | [`?behav_pls`](https://bbuchsbaum.github.io/plsrri/reference/behav_pls.md)                               |
| Full engine with all options   | [`?pls_analysis`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)                         |
| Loadings interpretation        | [`?plot_loadings`](https://bbuchsbaum.github.io/plsrri/reference/plot_loadings.md)                       |
| Bootstrap confidence intervals | [`?confidence`](https://bbuchsbaum.github.io/plsrri/reference/confidence.md)                             |
