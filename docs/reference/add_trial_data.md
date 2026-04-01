# Add Trial-Level Data for Within-Subject Seed PLS

Attaches trial-level beta matrices, seed data, and condition mappings to
a PLS specification for use with `method = "ws_seed"`.

## Usage

``` r
add_trial_data(
  spec,
  beta_lst,
  seed_lst,
  condition_lst,
  groups = NULL,
  seed_labels = NULL,
  layout = c("seed_condition", "stacked_seed_features"),
  fisher_z = TRUE,
  min_trials = 3L
)
```

## Arguments

- spec:

  A `pls_spec` object.

- beta_lst:

  A list of matrices (one per subject), each with rows = trials and
  columns = voxels.

- seed_lst:

  A list of numeric vectors or matrices (one per subject) with the seed
  region's trial-level betas.

- condition_lst:

  A list of integer vectors or factors (one per subject) mapping trials
  to conditions.

- fisher_z:

  Logical; apply Fisher r-to-z transform (default `TRUE`).

- min_trials:

  Minimum trials per condition for valid correlation (default 3).

## Value

Updated `pls_spec` object.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- pls_spec() |>
  add_trial_data(beta_lst, seed_lst, cond_lst) |>
  configure(method = "ws_seed", nperm = 500, nboot = 500) |>
  run()
} # }
```
