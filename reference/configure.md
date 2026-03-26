# Configure PLS Analysis

Sets analysis parameters for the PLS specification.

## Usage

``` r
configure(
  spec,
  method = NULL,
  nperm = NULL,
  nboot = NULL,
  nsplit = NULL,
  clim = NULL,
  meancentering = NULL,
  cormode = NULL,
  boot_type = NULL,
  is_struct = NULL
)
```

## Arguments

- spec:

  A `pls_spec` object

- method:

  PLS method (character or integer):

  - "task" or 1: Mean-Centering Task PLS

  - "task_nonrotated" or 2: Non-Rotated Task PLS

  - "behavior" or 3: Regular Behavior PLS

  - "multiblock" or 4: Regular Multiblock PLS

  - "behavior_nonrotated" or 5: Non-Rotated Behavior PLS

  - "multiblock_nonrotated" or 6: Non-Rotated Multiblock PLS

  - "ws_seed": Alias for task PLS on trial-level within-subject seed
    correlation maps from
    [`add_trial_data()`](https://bbuchsbaum.github.io/plsrri/reference/add_trial_data.md)

  - "ws_seed_nonrotated": Alias for non-rotated task PLS on trial-level
    within-subject seed correlation maps

- nperm:

  Number of permutations (0 = no permutation test)

- nboot:

  Number of bootstrap samples (0 = no bootstrap)

- nsplit:

  Number of split-half iterations (0 = no split-half)

- clim:

  Confidence level for bootstrap (0-100, default 95)

- meancentering:

  Mean-centering type (0-3):

  - 0: Within-group centering (default)

  - 1: Grand condition mean centering

  - 2: Grand mean centering

  - 3: Remove all main effects

- cormode:

  Correlation mode for behavior PLS:

  - "pearson" or 0: Pearson correlation (default)

  - "covariance" or 2: Covariance

  - "cosine" or 4: Cosine angle

  - "dot" or 6: Dot product

- boot_type:

  Bootstrap type: "strat" (default) or "nonstrat"

- is_struct:

  Logical, structure PLS (don't permute conditions)

## Value

Updated `pls_spec` object

## Examples

``` r
set.seed(42)
data1 <- matrix(rnorm(60 * 50), 60, 50)
data2 <- matrix(rnorm(54 * 50), 54, 50)

spec <- pls_spec() |>
  add_subjects(list(data1, data2), groups = c(20, 18)) |>
  add_conditions(3) |>
  configure(method = "task", nperm = 100, nboot = 50)
```
