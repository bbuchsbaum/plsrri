# Group by Covariate Designs

``` r
library(plsrri)
```

You have one brain map per subject, two between-subject groups, and a
trait-level continuous covariate. In formula notation, the motivating
model is often written as:

``` text
brain map ~ group * trait
```

In `plsrri`, this is not fit as a voxelwise formula model. The PLS
version asks for multivariate brain patterns associated with the trait,
and for whether that trait-brain pattern is shared by the groups or
differs between them.

## What is the PLS translation?

With one map per subject, use a single condition:

- the two groups are the two entries of `datamat_lst`
- `num_cond = 1`
- the trait is a one-column behavior matrix
- the behavior matrix rows are stacked in the same order as the brain
  data

Behavior PLS computes a trait-brain association separately within each
group. With two groups, one condition, and one trait, the behavior side
has two rows:

``` text
group 1 : trait-brain association
group 2 : trait-brain association
```

Contrasts over these two rows let you ask for the common trait
association and the group difference in that association.

## What data shape do we need?

Here is a compact simulated example. `Y_g1` and `Y_g2` are
subject-by-feature matrices. In a real analysis, the columns would be
voxels, parcels, surface vertices, or another common feature space.

``` r
set.seed(42)

n1 <- 28
n2 <- 20
n_features <- 120

trait_g1 <- as.numeric(scale(rnorm(n1)))
trait_g2 <- as.numeric(scale(rnorm(n2)))

Y_g1 <- matrix(rnorm(n1 * n_features, sd = 0.7), nrow = n1)
Y_g2 <- matrix(rnorm(n2 * n_features, sd = 0.7), nrow = n2)
```

The trait matrix is stacked as group 1 followed by group 2:

``` r
head(trait, 4)
#>           trait
#> [1,]  0.9998323
#> [2,] -0.4992076
#> [3,]  0.2193336
#> [4,]  0.4282251
tail(trait, 4)
#>           trait
#> [45,] -1.177344
#> [46,]  0.656984
#> [47,] -0.610182
#> [48,]  1.686925
```

## How do we test the trait effect and the interaction?

Use non-rotated Behavior PLS when the hypothesis is explicit. The design
matrix has one row per group-specific trait association. The first
column asks for a shared trait-brain association; the second asks
whether the association differs by group.

``` r
trait_design <- cbind(
  common_trait = c(1, 1),
  group_by_trait = c(-1, 1)
)

trait_design
#>      common_trait group_by_trait
#> [1,]            1             -1
#> [2,]            1              1
```

``` r
trait_result <- pls_spec() |>
  add_subjects(list(Y_g1, Y_g2), groups = c(n1, n2)) |>
  add_conditions(1) |>
  add_behavior(trait) |>
  add_design(trait_design) |>
  configure(
    method = "behavior_nonrotated",
    cormode = "pearson",
    nperm = 199,
    nboot = 199
  ) |>
  run(progress = FALSE)
```

The two requested contrasts become two latent variables:

``` r
cbind(
  pvalue = round(significance(trait_result), 3),
  variance = round(singular_values(trait_result, normalize = TRUE), 1)
)
#>     pvalue variance
#> LV1      0     76.2
#> LV2      0     23.8
```

The design loadings show the hypothesis weights used for each latent
variable:

``` r
loadings(trait_result, type = "design")
#>      contrast_1 contrast_2
#> [1,]  0.7071068 -0.7071068
#> [2,]  0.7071068  0.7071068
```

Interpret the rows as:

- `common_trait`: the trait-brain association pooled across groups
- `group_by_trait`: the difference in trait-brain association between
  groups

The sign of `group_by_trait` follows the contrast coding. With
`c(-1, 1)`, positive brain saliences are more strongly associated with
the trait in group 2 than in group 1.

## Where is the group main effect?

The trait model above targets the covariate part of `group * trait`. It
does not estimate the mean group difference. For the main group effect,
use non-rotated Task PLS with one condition and a group contrast:

``` r
group_result <- pls_spec() |>
  add_subjects(list(Y_g1, Y_g2), groups = c(n1, n2)) |>
  add_conditions(1) |>
  add_design(cbind(group_2_minus_1 = c(-1, 1))) |>
  configure(
    method = "task_nonrotated",
    nperm = 199,
    nboot = 199
  ) |>
  run(progress = FALSE)
```

``` r
cbind(
  pvalue = round(significance(group_result), 3),
  variance = round(singular_values(group_result, normalize = TRUE), 1)
)
#>     pvalue variance
#> LV1  0.015      100
```

This is usually the clearest workflow: fit the group mean effect and the
trait interaction question as separate, targeted analyses.

## Can this be one multiblock analysis?

Yes. Non-rotated Multiblock PLS combines the task block and behavior
block. For this design, the multiblock design rows are:

``` text
group 1 mean
group 2 mean
group 1 trait association
group 2 trait association
```

That gives a single analysis with columns for the group main effect,
common trait effect, and group-by-trait effect.

``` r
multiblock_design <- cbind(
  group_2_minus_1 = c(-1, 1, 0, 0),
  common_trait = c(0, 0, 1, 1),
  group_by_trait = c(0, 0, -1, 1)
)

multiblock_design
#>      group_2_minus_1 common_trait group_by_trait
#> [1,]              -1            0              0
#> [2,]               1            0              0
#> [3,]               0            1             -1
#> [4,]               0            1              1
```

``` r
multiblock_result <- pls_spec() |>
  add_subjects(list(Y_g1, Y_g2), groups = c(n1, n2)) |>
  add_conditions(1) |>
  add_behavior(trait) |>
  add_design(multiblock_design) |>
  configure(
    method = "multiblock_nonrotated",
    cormode = "pearson",
    nperm = 199,
    nboot = 199
  ) |>
  run(progress = FALSE)
```

``` r
cbind(
  pvalue = round(significance(multiblock_result), 3),
  variance = round(singular_values(multiblock_result, normalize = TRUE), 1)
)
#>     pvalue variance
#> LV1  0.261     35.8
#> LV2  0.869     24.4
#> LV3  0.131     39.8
```

Use the multiblock version when you want the mean group difference and
the trait associations represented in one decomposition. Use the
separate non-rotated analyses when you want the most direct test of each
term.

## What should you report?

For the trait analysis, report:

- the row order and contrast coding
- whether the covariate was centered or scaled before analysis
- the permutation p-values for the requested LVs
- the bootstrap ratios or confidence intervals for stable brain features
- the sign convention for the interaction contrast

For unbalanced groups, pass the actual group sizes in
`groups = c(n1, n2)`. The bootstrap and permutation routines use those
group sizes when resampling.

## Where to go next

| Goal                                  | Resource                                                                                                 |
|:--------------------------------------|:---------------------------------------------------------------------------------------------------------|
| Behavior PLS with continuous measures | [`vignette("behavior-pls")`](https://bbuchsbaum.github.io/plsrri/articles/behavior-pls.md)               |
| Group and condition effects           | [`vignette("plsrri")`](https://bbuchsbaum.github.io/plsrri/articles/plsrri.md)                           |
| Multiblock PLS                        | [`vignette("multiblock-and-seed")`](https://bbuchsbaum.github.io/plsrri/articles/multiblock-and-seed.md) |
| Predictive validation from PLS scores | [`vignette("predictive-measures")`](https://bbuchsbaum.github.io/plsrri/articles/predictive-measures.md) |
| Full engine options                   | [`?pls_analysis`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)                         |
