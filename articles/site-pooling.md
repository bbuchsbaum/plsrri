# Multisite Behavioral PLS Workflow

``` r
library(plsrri)
```

If you pool subjects from multiple sites, a single behavioral PLS fit is
often the right starting point, but only if you can also ask whether the
latent pattern is stable across sites. This workflow shows a basic
multisite strategy:

1.  standardize brain and behavior within site
2.  fit one pooled behavioral PLS model
3.  inspect site-specific stability diagnostics after the fit

## What does a pooled multisite dataset look like?

For a pooled analysis you still need the standard behavior-PLS row
structure: subjects within conditions. The only addition is a site label
for each subject or observation.

``` r
site_summary
#>     site n_subjects n_observations       rt_mean      acc_mean
#> 1 site_a         12             24 -5.898060e-17 -3.903128e-17
#> 2 site_b         12             24  3.670928e-17 -8.673617e-18
#> 3 site_c         12             24 -9.292516e-18  8.673617e-19
```

This example has three sites, 36 pooled subjects, and 2 conditions. The
hidden setup adds site-specific mean and scale shifts before
standardization, so the pooled fit has to recover the shared latent
brain-behavior relation rather than scanner/site offsets.

## How do you fit one pooled model and keep site information?

Use the usual builder path for behavioral PLS, then attach site labels
before running the fit.

``` r
site_spec <- pls_spec() |>
  add_subjects(list(brain_z), groups = n_subj) |>
  add_conditions(condition_levels) |>
  add_behavior(behav_z) |>
  add_site_labels(site_subj) |>
  configure(method = "behavior", nperm = 120, nboot = 120)

site_result <- run(site_spec, progress = FALSE)
site_diag <- site_result$site_diagnostics

stopifnot(
  inherits(site_result, "pls_result"),
  is.list(site_diag),
  setequal(site_diag$sites, site_levels)
)
```

Check the pooled latent variables first, just as you would in an
ordinary behavior PLS run:

``` r
pooled_summary <- cbind(
  pvalue = round(significance(site_result), 3),
  variance = round(singular_values(site_result, normalize = TRUE), 1)
)

stopifnot(
  pooled_summary[1, "pvalue"] <= 0.1,
  pooled_summary[1, "variance"] > pooled_summary[2, "variance"]
)

pooled_summary
#>     pvalue variance
#> LV1  0.000     98.9
#> LV2  1.000      1.0
#> LV3  0.942      0.1
#> LV4  1.000      0.0
```

LV1 is the pooled brain-behavior pattern. The important multisite
question is whether that pooled pattern looks similar across sites.

## What diagnostics do you get when site labels are present?

[`site_pooling_diagnostics()`](https://bbuchsbaum.github.io/plsrri/reference/site_pooling_diagnostics.md)
summarizes four things:

- sitewise score distributions
- sitewise brain-behavior score correlations
- site-specific refits compared with the pooled saliences
- leave-one-site-out reruns

Because `site` was provided in the builder, those diagnostics are
already attached to the result:

``` r
site_corr_lv1 <- subset(site_diag$site_score_correlations, lv == 1)

stopifnot(
  nrow(site_corr_lv1) == length(site_levels),
  all(is.finite(site_corr_lv1$correlation)),
  length(unique(sign(site_corr_lv1$correlation))) == 1L,
  all(abs(site_corr_lv1$correlation) > 0.35)
)

site_corr_lv1
#>     site lv n_obs correlation
#> 1 site_a  1    24   0.9610821
#> 2 site_b  1    24   0.9730672
#> 3 site_c  1    24   0.9562201
```

Every site shows the same direction of brain-behavior association on
LV1. That is the first thing you want to see in a pooled multisite
analysis.

``` r
ggplot2::ggplot(site_corr_lv1, ggplot2::aes(x = site, y = correlation, fill = site)) +
  ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  ggplot2::labs(x = "Site", y = "LV1 score correlation") +
  theme_pls()
```

![Sitewise LV1 brain-behavior score correlations. All three sites show
the same direction of
association.](site-pooling_files/figure-html/plot-site-correlations-1.png)

Sitewise LV1 brain-behavior score correlations. All three sites show the
same direction of association.

## Do the site-specific saliences resemble the pooled solution?

The site-specific reruns compare each site’s own fit to the pooled fit
using cosine similarity in both feature space and design space.

``` r
site_sim_lv1 <- subset(site_diag$site_fit_similarity, lv == 1)

stopifnot(
  nrow(site_sim_lv1) == length(site_levels),
  all(site_sim_lv1$feature_cosine > 0.6),
  all(site_sim_lv1$design_cosine > 0.6)
)

site_sim_lv1
#>     site lv feature_cosine design_cosine
#> 1 site_a  1      0.9914166     0.9999807
#> 5 site_b  1      0.9892361     0.9996963
#> 9 site_c  1      0.9889637     0.9998657
```

Those similarities are not a replacement for interpretation, but they
are a useful quick check that the pooled LV is not being driven by one
site alone.

## What happens if you leave one site out?

Leave-one-site-out reruns are the strongest built-in stress test in the
current workflow. They refit the model on two sites, then project the
held-out site into that score space.

``` r
loos_lv1 <- subset(site_diag$leave_one_site_out, lv == 1)

stopifnot(
  nrow(loos_lv1) == length(site_levels),
  all(is.finite(loos_lv1$heldout_score_correlation)),
  all(loos_lv1$feature_cosine > 0.5),
  all(loos_lv1$design_cosine > 0.5),
  length(unique(sign(loos_lv1$heldout_score_correlation))) == 1L
)

loos_lv1
#>   held_out_site lv feature_cosine design_cosine heldout_score_correlation
#> 1        site_a  1      0.9978663     0.9999887                 0.9601310
#> 5        site_b  1      0.9976623     0.9999439                 0.9723149
#> 9        site_c  1      0.9970719     0.9999691                 0.9551316
```

If the held-out score correlations flip sign or collapse toward zero,
the pooled latent pattern is not really shared. In this synthetic
example the held-out site still lines up with the training fit, which is
what you want in a stable pooled analysis.

## When should you stop pooling and rethink the model?

Pooling is still a model choice. The diagnostics here are meant to tell
you when the pooled result is doing real shared work and when it is
papering over site heterogeneity.

Rethink the pooled model if you see any of these:

- sitewise LV correlations with different signs
- low site-specific salience similarity to the pooled LV
- leave-one-site-out fits that fail only when one particular site is
  held out
- score distributions that are dominated by one site rather than by the
  latent pattern

At that point you may want stronger harmonization, explicit site
residualization, or separate site-specific analyses.

## Where should you go next?

| Goal                                     | Resource                                                                                                 |
|:-----------------------------------------|:---------------------------------------------------------------------------------------------------------|
| Basic behavior PLS without site pooling  | [`vignette("behavior-pls")`](https://bbuchsbaum.github.io/plsrri/articles/behavior-pls.md)               |
| Scripted CLI and artifact workflow       | [`vignette("scripted-workflows")`](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md)   |
| YAML contract for multisite pipelines    | [`vignette("pipeline-yaml-spec")`](https://bbuchsbaum.github.io/plsrri/articles/pipeline-yaml-spec.md)   |
| Add site labels in the builder           | [`?add_site_labels`](https://bbuchsbaum.github.io/plsrri/reference/add_site_labels.md)                   |
| Compute diagnostics from a fitted result | [`?site_pooling_diagnostics`](https://bbuchsbaum.github.io/plsrri/reference/site_pooling_diagnostics.md) |
| Full behavior wrapper                    | [`?behav_pls`](https://bbuchsbaum.github.io/plsrri/reference/behav_pls.md)                               |
