# Run Bootstrap Test

Runs bootstrap test to compute bootstrap ratios and confidence
intervals.

## Usage

``` r
pls_bootstrap_test(
  stacked_datamat,
  stacked_behavdata = NULL,
  stacked_designdata = NULL,
  num_groups,
  num_subj_lst,
  num_cond,
  method,
  num_boot,
  observed_u,
  observed_v,
  observed_s,
  observed_lvcorrs = NULL,
  bscan = NULL,
  meancentering_type = 0L,
  cormode = 0L,
  boot_type = "strat",
  nonrotated_boot = FALSE,
  bootsamp = NULL,
  bootsamp_4beh = NULL,
  clim = 95,
  progress = TRUE
)
```

## Arguments

- stacked_datamat:

  Stacked data matrix

- stacked_behavdata:

  Behavior data matrix (methods 3-6)

- stacked_designdata:

  Design contrast matrix (methods 2, 5, 6)

- num_groups:

  Number of groups

- num_subj_lst:

  Subjects per group

- num_cond:

  Number of conditions

- method:

  PLS method (1-6)

- num_boot:

  Number of bootstrap samples

- observed_u:

  Observed u (saliences)

- observed_v:

  Observed v (loadings)

- bscan:

  Conditions for behavior block

- meancentering_type:

  Mean-centering type

- cormode:

  Correlation mode

- boot_type:

  Bootstrap type

- clim:

  Confidence level

- progress:

  Show progress

## Value

pls_boot_result object
