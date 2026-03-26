# Create a Bootstrap Result Object

Create a Bootstrap Result Object

## Usage

``` r
new_pls_boot_result(
  num_boot,
  boot_type = "strat",
  compare_u = NULL,
  u_se = NULL,
  clim = 95,
  bootsamp = NULL,
  bootsamp_4beh = NULL,
  distrib = NULL,
  prop = NULL,
  Tdistrib = NULL,
  Tprop = NULL,
  usc2 = NULL,
  orig_usc = NULL,
  ulusc = NULL,
  llusc = NULL,
  ulusc_adj = NULL,
  llusc_adj = NULL,
  orig_corr = NULL,
  ulcorr = NULL,
  llcorr = NULL,
  ulcorr_adj = NULL,
  llcorr_adj = NULL,
  nonrotated_boot = FALSE,
  num_LowVariability_behav_boots = NULL,
  badbeh = NULL,
  countnewtotal = NULL
)
```

## Arguments

- num_boot:

  Number of bootstrap samples

- boot_type:

  "strat" or "nonstrat"

- compare_u:

  Bootstrap ratios (u / u_se)

- u_se:

  Standard error of saliences

- clim:

  Confidence level (0-100)

- bootsamp:

  Bootstrap sample matrix

- bootsamp_4beh:

  Bootstrap samples for behavior (if different)

- distrib:

  Distribution of bootstrap statistics

- prop:

  Proportions/probabilities

- Tdistrib:

  Task-block distribution (multiblock)

- Tprop:

  Task-block proportions (multiblock)

- usc2:

  Brain scores from mean-centered data (task PLS)

- orig_usc:

  Original brain scores (task PLS)

- ulusc:

  Upper confidence bound for usc

- llusc:

  Lower confidence bound for usc

- orig_corr:

  Original correlations (behavior PLS)

- ulcorr:

  Upper confidence bound for correlations

- llcorr:

  Lower confidence bound for correlations

- nonrotated_boot:

  Whether using non-rotated bootstrap

- num_LowVariability_behav_boots:

  Count of low variability samples

- badbeh:

  Bad behavior indices

- countnewtotal:

  Count of resampled bad behavior

## Value

A `pls_boot_result` object
