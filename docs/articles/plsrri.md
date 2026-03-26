# Getting Started with plsrri

``` r
library(plsrri)
```

You have subject-by-condition brain data and want to know two things:
which latent patterns separate your experimental conditions, and how
stable are those patterns under resampling? `plsrri` gives you both a
direct entry point for standard analyses and a builder workflow when you
need more control.

## How does a PLS analysis work?

The core workflow has three steps:

1.  **Specify** — describe your data, conditions, groups, and analysis
    parameters in a `pls_spec` object
2.  **Run** — execute the analysis to get a `pls_result` containing
    singular values, saliences, scores, and optional inference
    (permutation, bootstrap)
3.  **Extract** — pull out what you need with accessor functions:
    [`significance()`](https://bbuchsbaum.github.io/plsrri/reference/significance.md),
    [`salience()`](https://bbuchsbaum.github.io/plsrri/reference/salience.md),
    [`bsr()`](https://bbuchsbaum.github.io/plsrri/reference/bsr.md),
    [`scores()`](https://bbuchsbaum.github.io/plsrri/reference/scores.md),
    [`loadings()`](https://bbuchsbaum.github.io/plsrri/reference/loadings.md),
    [`confidence()`](https://bbuchsbaum.github.io/plsrri/reference/confidence.md)

| Object | What it holds | How you get it |
|:---|:---|:---|
| `pls_spec` | Data matrices, method, resampling config | [`pls_spec()`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md) + builder verbs |
| `pls_result` | SVD decomposition, scores, inference | [`run()`](https://bbuchsbaum.github.io/plsrri/reference/run.md) or [`quick_pls()`](https://bbuchsbaum.github.io/plsrri/reference/quick_pls.md) |

The fastest path is
[`quick_pls()`](https://bbuchsbaum.github.io/plsrri/reference/quick_pls.md),
which wraps all three steps into one call. The builder API
(`pls_spec() |> add_subjects() |> configure() |> run()`) gives you
labels, metadata, and full control over every parameter.

## What does a minimal analysis look like?

This vignette uses a small synthetic dataset: two groups (young, older),
three conditions, and 200 voxels with planted signal in two blocks.

Each group matrix has 15 subjects $`\times`$ 3 conditions = 45 rows and
200 voxels:

``` r
rbind(
  young = dim(group_young),
  older = dim(group_older)
)
#>       [,1] [,2]
#> young   45  200
#> older   45  200
```

[`quick_pls()`](https://bbuchsbaum.github.io/plsrri/reference/quick_pls.md)
is the shortest path. It runs mean-centering Task PLS with permutation
and bootstrap inference in one call:

``` r
result <- quick_pls(
  datamat_lst = list(group_young, group_older),
  num_subj_lst = c(n_subj, n_subj),
  num_cond = n_cond,
  nperm = 200,
  nboot = 200,
  progress = FALSE
)
```

The result object carries everything: 6 latent variables, each with a
p-value from the permutation test and voxel-level bootstrap ratios.

## Which latent variable matters?

Start with
[`significance()`](https://bbuchsbaum.github.io/plsrri/reference/significance.md)
to see which latent variables survive the permutation test, and
[`singular_values()`](https://bbuchsbaum.github.io/plsrri/reference/singular_values.md)
to see how variance is distributed:

``` r
cbind(
  pvalue = round(significance(result), 3),
  variance = round(singular_values(result, normalize = TRUE), 1)
)
#>     pvalue variance
#> LV1  0.600     29.5
#> LV2  0.415     27.0
#> LV3  0.215     24.6
#> LV4  0.875     18.8
#> LV5  0.010      0.0
#> LV6  0.000      0.0
```

LV1 captures 29.5% of the variance. The scree plot makes this dominance
visual:

``` r
plot_singular_values(result)
```

![Variance explained by each latent variable. LV1 dominates, consistent
with the planted
signal.](plsrri_files/figure-html/plot-task-singular-values-1.png)

Variance explained by each latent variable. LV1 dominates, consistent
with the planted signal.

## How do conditions load onto LV1?

Design scores show how each group-by-condition cell projects onto the
latent variable. Large positive and negative bars indicate conditions
that drive the pattern in opposite directions.

``` r
plot_scores(result, lv = 1, type = "design", plot_type = "bar")
```

![Design scores for LV1. Encoding (condition 1) loads positively;
retrieval (condition 2) loads in the opposite
direction.](plsrri_files/figure-html/plot-task-design-scores-1.png)

Design scores for LV1. Encoding (condition 1) loads positively;
retrieval (condition 2) loads in the opposite direction.

The separation between conditions confirms that LV1 captures a genuine
experimental effect, not noise.

## How consistent are subjects?

Brain scores show whether individual subjects express the same latent
pattern. Tight distributions mean the effect is reliable; wide spread
suggests individual differences.

``` r
plot_scores(result, lv = 1, type = "brain", plot_type = "violin")
```

![Brain scores for LV1. Each point is one subject-condition
observation.](plsrri_files/figure-html/plot-task-brain-scores-1.png)

Brain scores for LV1. Each point is one subject-condition observation.

## Which voxels contribute reliably?

Bootstrap ratios (BSRs) are saliences divided by their bootstrap
standard errors — they work like z-scores. Voxels with $`|BSR| > 2`$ or
$`3`$ are typically considered reliable.

``` r
lv1_bsr <- bsr(result, lv = 1)
reliable <- sum(abs(lv1_bsr) > 2)
```

21 of 200 voxels have $`|BSR| > 2`$. A quick profile shows where the
reliable voxels cluster:

![Bootstrap ratio profile across voxels. The horizontal lines mark the
\|BSR\| = 2 threshold. Voxels 1-80 and 81-140 show the planted signal
blocks.](plsrri_files/figure-html/plot-task-bsr-1.png)

Bootstrap ratio profile across voxels. The horizontal lines mark the
\|BSR\| = 2 threshold. Voxels 1-80 and 81-140 show the planted signal
blocks.

You can also threshold directly to get a cleaned vector:

``` r
lv1_bsr_clean <- bsr(result, lv = 1, threshold = 2)
sum(lv1_bsr_clean != 0)
#> [1] 21
```

## When should you use the builder workflow?

Use the builder API when you want condition labels, group names, or
non-default method settings to travel with the analysis. The pipe chain
makes the full specification explicit and readable:

``` r
builder_result <- pls_spec() |>
  add_subjects(
    list(group_young, group_older),
    groups = c(n_subj, n_subj)
  ) |>
  add_conditions(n_cond, labels = c("encoding", "retrieval", "rest")) |>
  add_group_labels(c("young", "older")) |>
  configure(method = "task", nperm = 200, nboot = 200) |>
  run(progress = FALSE)
```

The numerical result is identical to
[`quick_pls()`](https://bbuchsbaum.github.io/plsrri/reference/quick_pls.md),
but now plots carry meaningful labels:

``` r
plot_scores(builder_result, lv = 1, type = "design", plot_type = "bar")
```

![Design scores with condition and group labels from the builder
workflow.](plsrri_files/figure-html/plot-builder-scores-1.png)

Design scores with condition and group labels from the builder workflow.

The builder API also supports behavior data
([`add_behavior()`](https://bbuchsbaum.github.io/plsrri/reference/add_behavior.md)),
design contrasts
([`add_design()`](https://bbuchsbaum.github.io/plsrri/reference/add_design.md)),
and non-rotated methods — see
[`vignette("behavior-pls")`](https://bbuchsbaum.github.io/plsrri/articles/behavior-pls.md)
for the behavior workflow.

## What about the interactive GUI?

For exploratory work, `plsrri` ships a Shiny application that wraps the
full setup-analyze-explore workflow in a point-and-click interface:

``` r
launch_pls_gui()
```

The GUI supports loading data from BIDS directories, attaching to CLI
pipeline outputs, and interactive brain visualization with volume and
surface views.

## Where to go next

If you want to move from interactive examples to reproducible scripted
analysis, go next to
[`vignette("scripted-workflows")`](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md),
which shows the public non-GUI contract built around saved first-level
artifacts, staged CLI runs, and Quarto reporting.

| Goal | Resource |
|:---|:---|
| Scripted R API, CLI, and report workflow | [`vignette("scripted-workflows")`](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md) |
| Behavior PLS with continuous measures | [`vignette("behavior-pls")`](https://bbuchsbaum.github.io/plsrri/articles/behavior-pls.md) |
| Multiblock and seed connectivity PLS | [`vignette("multiblock-and-seed")`](https://bbuchsbaum.github.io/plsrri/articles/multiblock-and-seed.md) |
| Full engine parameters | [`?pls_analysis`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md) |
| Builder verbs | [`?pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md), [`?configure`](https://bbuchsbaum.github.io/plsrri/reference/configure.md), [`?run`](https://bbuchsbaum.github.io/plsrri/reference/run.md) |
| Plotting functions | [`?plot_scores`](https://bbuchsbaum.github.io/plsrri/reference/plot_scores.md), [`?plot_loadings`](https://bbuchsbaum.github.io/plsrri/reference/plot_loadings.md), [`?plot_singular_values`](https://bbuchsbaum.github.io/plsrri/reference/plot_singular_values.md) |
| Bootstrap and permutation details | [`?bsr`](https://bbuchsbaum.github.io/plsrri/reference/bsr.md), [`?significance`](https://bbuchsbaum.github.io/plsrri/reference/significance.md), [`?confidence`](https://bbuchsbaum.github.io/plsrri/reference/confidence.md) |
