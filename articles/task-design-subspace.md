# Design-Subspace Summaries for Task PLS

``` r

library(plsrri)
```

Task PLS gives you latent variables over a flattened set of
group-by-condition cells. When the conditions come from a factorial
design, the next question is often more specific: does the covariance
pattern live mostly in a task effect, a group-by-task interaction, or a
higher-order term?

The design-subspace helpers answer that question on the design side of
Task PLS. They do not run voxelwise ANOVAs. They build factorial
subspaces over the same centered cell rows used by Task PLS and
summarize how much cross-block covariance lies in each subspace.

## What are the data?

This example uses synthetic data with two groups, two tasks, and two
levels. The planted signal includes a group-by-task effect, a level
effect, and a smaller three-way interaction.

The condition key maps each PLS condition label back to the factors that
created it.

``` r

condition_key
#>    condition  task level
#> 1  recog_low recog   low
#> 2 recog_high recog  high
#> 3  nback_low nback   low
#> 4 nback_high nback  high
```

## How do we keep the Task PLS cross-block matrix?

Design-subspace summaries need the cell-level cross-block matrix used by
Task PLS before the singular value decomposition. Request it at fit time
with `keep_crossblock = TRUE`.

``` r

fit <- pls_spec() |>
  add_subjects(list(control, sdam), groups = c(n_subjects, n_subjects)) |>
  add_conditions(nrow(condition_key), labels = condition_key$condition) |>
  add_group_labels(c("control", "sdam")) |>
  configure(method = "task", meancentering = "grand_mean") |>
  run(progress = FALSE, keep_crossblock = TRUE)
```

The ordinary Task PLS result is still a latent-variable model. The new
field is only a compact summary object needed for design-subspace
calculations; it does not store the original subject-level imaging data.

## How do we define the factorial design?

Use
[`pls_design()`](https://bbuchsbaum.github.io/plsrri/reference/pls_design.md)
to describe the design over group-condition cells. The rows come from
the PLS result; the condition-level factors come from the condition key.

``` r

design <- pls_design(
  ~ group * task * level,
  condition_key = condition_key,
  between = "group",
  within = c("task", "level")
)
```

You can inspect the cell table to confirm the row order. These are the
rows of the Task PLS cross-block matrix: all conditions for group 1,
then all conditions for group 2.

``` r

design_cell_table(fit, design = design)[
  , c("row_index", "group", "condition", "task", "level")
]
#>     row_index   group  condition  task level
#> 1           1 control  recog_low recog   low
#> 2           2 control recog_high recog  high
#> 3           3 control  nback_low nback   low
#> 4           4 control nback_high nback  high
#> 1.1         5    sdam  recog_low recog   low
#> 2.1         6    sdam recog_high recog  high
#> 3.1         7    sdam  nback_low nback   low
#> 4.1         8    sdam nback_high nback  high
```

## Which factorial terms does an LV resemble?

Start descriptively.
[`decompose_design_terms()`](https://bbuchsbaum.github.io/plsrri/reference/decompose_design_terms.md)
projects one design-side LV onto the centered factorial subspaces and
reports how much LV energy aligns with each term. This helps interpret
an already selected LV; it is not a p-value.

``` r

lv_terms <- decompose_design_terms(fit, lv = 1, design = design)
lv_terms$fraction <- round(lv_terms$fraction, 3)
lv_terms[order(lv_terms$fraction, decreasing = TRUE),
         c("term", "rank", "fraction")]
#>               term rank fraction
#> 5      group:level    1    0.855
#> 1            group    1    0.110
#> 2             task    1    0.022
#> 6       task:level    1    0.010
#> 3            level    1    0.001
#> 4       group:task    1    0.000
#> 7 group:task:level    1    0.000
```

For this simulated example, LV1 should align strongly with the planted
factorial effects. The exact ordering is data-dependent because the
example still includes noise.

A plot of the same LV-level decomposition is useful when you want a
quick interpretive summary for a selected latent variable.

``` r

plot_design_contrasts(fit, lv = 1, condition_key = condition_key)
```

![Effect-coded decomposition of the LV1 design
scores.](task-design-subspace_files/figure-html/plot-lv-terms-1.png)

Effect-coded decomposition of the LV1 design scores.

## How much covariance is in each design subspace?

[`test_design_terms()`](https://bbuchsbaum.github.io/plsrri/reference/test_design_terms.md)
works on the stored Task PLS cross-block matrix, not just on one LV. The
default statistic is `"trace"`, the total squared singular value energy
in each centered term subspace.

``` r

term_tests <- test_design_terms(
  fit,
  design = design,
  statistic = "trace"
)
term_tests$share <- round(term_tests$statistic / sum(term_tests$statistic), 3)
term_tests$statistic <- round(term_tests$statistic, 2)
term_tests[, c("term", "rank", "statistic", "share", "p_value")]
#>               term rank statistic share p_value
#> 1            group    1      2.88 0.135      NA
#> 2             task    1      2.78 0.130      NA
#> 3            level    1      2.73 0.128      NA
#> 4       group:task    1      2.23 0.104      NA
#> 5      group:level    1      4.75 0.223      NA
#> 6       task:level    1      2.74 0.128      NA
#> 7 group:task:level    1      3.23 0.151      NA
```

The `p_value` column is intentionally `NA` here. The current
implementation computes observed design-subspace summaries; permutation
p-values for nested factorial questions require a
reduced-model-respecting null and are not silently approximated.

If you want the most PLS-like single-root statistic instead of omnibus
energy, use `statistic = "largest_root"`.

``` r

largest_root <- test_design_terms(
  fit,
  design = design,
  statistic = "largest_root"
)
largest_root$statistic <- round(largest_root$statistic, 2)
largest_root[, c("term", "rank", "statistic")]
#>               term rank statistic
#> 1            group    1      2.88
#> 2             task    1      2.78
#> 3            level    1      2.73
#> 4       group:task    1      2.23
#> 5      group:level    1      4.75
#> 6       task:level    1      2.74
#> 7 group:task:level    1      3.23
```

## How do we ask a nested design question?

Use
[`compare_designs()`](https://bbuchsbaum.github.io/plsrri/reference/compare_designs.md)
when your question is “what is added by the full model beyond the
reduced model?” The function residualizes the full design against the
reduced design in the same centered Task PLS row space.

``` r

task_beyond_group <- compare_designs(
  fit,
  design = design,
  reduced = ~ group,
  full = ~ group * task
)

interactions_beyond_main_effects <- compare_designs(
  fit,
  design = design,
  reduced = ~ group + task + level,
  full = ~ group * task * level
)

nested_tests <- rbind(task_beyond_group, interactions_beyond_main_effects)
nested_tests$statistic <- round(nested_tests$statistic, 2)
nested_tests[, c("reduced", "full", "added_terms", "rank", "statistic", "p_value")]
#>                 reduced                  full
#> 1                ~group         ~group * task
#> 2 ~group + task + level ~group * task * level
#>                                                added_terms rank statistic
#> 1                                        task + group:task    3      7.89
#> 2 group:task + group:level + task:level + group:task:level    7     21.26
#>   p_value
#> 1      NA
#> 2      NA
```

The first comparison asks how much covariance is carried by task plus
the group-by-task interaction after accounting for group. The second
asks how much covariance is carried by all interactions after accounting
for the three main effects.

## How should the summaries be interpreted?

Keep three distinctions clear:

1.  [`plot_scores()`](https://bbuchsbaum.github.io/plsrri/reference/plot_scores.md)
    and
    [`plot_design_contrasts()`](https://bbuchsbaum.github.io/plsrri/reference/plot_design_contrasts.md)
    help you interpret selected LVs.
2.  [`decompose_design_terms()`](https://bbuchsbaum.github.io/plsrri/reference/decompose_design_terms.md)
    descriptively attributes one LV’s design vector to factorial terms.
3.  [`test_design_terms()`](https://bbuchsbaum.github.io/plsrri/reference/test_design_terms.md)
    and
    [`compare_designs()`](https://bbuchsbaum.github.io/plsrri/reference/compare_designs.md)
    summarize covariance energy in centered design subspaces of the Task
    PLS cross-block matrix.

These are design-subspace summaries, not classical ANOVA tables and not
voxelwise tests. They preserve the multivariate PLS object while making
the factorial structure of the design explicit.

## Where to go next

Use
[`vignette("sdam-firstlevel-task-pls")`](https://bbuchsbaum.github.io/plsrri/articles/sdam-firstlevel-task-pls.md)
for a first-level image-map example with design-score heatmaps and
contrast plots. Use
[`?pls_design`](https://bbuchsbaum.github.io/plsrri/reference/pls_design.md),
[`?decompose_design_terms`](https://bbuchsbaum.github.io/plsrri/reference/decompose_design_terms.md),
[`?test_design_terms`](https://bbuchsbaum.github.io/plsrri/reference/test_design_terms.md),
and
[`?compare_designs`](https://bbuchsbaum.github.io/plsrri/reference/compare_designs.md)
for the function-level reference.
