# Create a PLS Result Object

Create a PLS Result Object

## Usage

``` r
new_pls_result(
  method,
  u,
  s,
  v,
  usc = NULL,
  vsc = NULL,
  datamatcorrs_lst = NULL,
  lvcorrs = NULL,
  perm_result = NULL,
  boot_result = NULL,
  splithalf_result = NULL,
  num_subj_lst = NULL,
  num_cond = NULL,
  bscan = NULL,
  stacked_designdata = NULL,
  stacked_behavdata = NULL,
  other_input = NULL,
  TBv = NULL,
  TBusc = NULL,
  TBvsc = NULL,
  is_struct = FALSE,
  mask = NULL
)
```

## Arguments

- method:

  Integer, PLS method (1-6)

- u:

  Salience (brain loadings) matrix

- s:

  Singular values

- v:

  Design/behavior loadings matrix

- usc:

  Brain scores

- vsc:

  Design/behavior scores

- datamatcorrs_lst:

  Correlation matrices by group (behavior PLS)

- lvcorrs:

  Latent variable correlations (behavior PLS)

- perm_result:

  Permutation result object

- boot_result:

  Bootstrap result object

- splithalf_result:

  Split-half result object

- num_subj_lst:

  Number of subjects per group

- num_cond:

  Number of conditions

- bscan:

  Conditions used for behavior block (multiblock)

- stacked_designdata:

  Design contrast matrix (non-rotated methods)

- stacked_behavdata:

  Behavior data matrix

- other_input:

  List of other inputs (meancentering_type, cormode)

- TBv:

  Task/Behavior v stored separately (multiblock)

- TBusc:

  Task/Behavior brain scores (multiblock)

- TBvsc:

  Task/Behavior design scores (multiblock)

- is_struct:

  Structure PLS flag

- mask:

  Brain mask (optional)

## Value

A `pls_result` object
