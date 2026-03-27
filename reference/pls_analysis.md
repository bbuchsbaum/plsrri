# Run PLS Analysis

Performs PLS (Partial Least Squares) analysis on neuroimaging or other
multivariate data. Supports task PLS, behavior PLS, and multiblock PLS
with both rotated and non-rotated variants.

## Usage

``` r
pls_analysis(
  datamat_lst,
  num_subj_lst,
  num_cond,
  method = 1L,
  num_perm = 0L,
  num_boot = 0L,
  num_split = 0L,
  clim = 95,
  stacked_behavdata = NULL,
  stacked_designdata = NULL,
  bscan = NULL,
  meancentering_type = 0L,
  cormode = 0L,
  boot_type = "strat",
  is_struct = FALSE,
  permsamp = NULL,
  Tpermsamp = NULL,
  Bpermsamp = NULL,
  bootsamp = NULL,
  bootsamp_4beh = NULL,
  parallel_config = NULL,
  progress = TRUE
)
```

## Arguments

- datamat_lst:

  List of data matrices. Each element is a matrix where rows are
  observations (subjects x conditions) and columns are features
  (voxels). Each list element represents one group.

- num_subj_lst:

  Integer vector with number of subjects per group.

- num_cond:

  Integer, number of conditions.

- method:

  Integer 1-6 specifying PLS method:

  1

  :   Mean-Centering Task PLS (default)

  2

  :   Non-Rotated Task PLS

  3

  :   Regular Behavior PLS

  4

  :   Regular Multiblock PLS

  5

  :   Non-Rotated Behavior PLS

  6

  :   Non-Rotated Multiblock PLS

- num_perm:

  Number of permutations (0 = no permutation test).

- num_boot:

  Number of bootstrap samples (0 = no bootstrap).

- num_split:

  Number of split-half validations (0 = no split-half).

- clim:

  Confidence level for bootstrap (0-100, default 95).

- stacked_behavdata:

  Behavior data matrix (required for methods 3-6). Rows match stacked
  datamat, columns are behavior measures.

- stacked_designdata:

  Design contrast matrix (required for methods 2, 5, 6).

- bscan:

  Integer vector of conditions for behavior block in multiblock PLS
  (methods 4, 6). Default is all conditions.

- meancentering_type:

  Mean-centering type (0-3):

  0

  :   Remove within-group mean (default)

  1

  :   Remove grand condition mean

  2

  :   Remove grand mean

  3

  :   Remove all main effects

- cormode:

  Correlation mode for behavior PLS (0, 2, 4, or 6):

  0

  :   Pearson correlation (default)

  2

  :   Covariance

  4

  :   Cosine angle

  6

  :   Dot product

- boot_type:

  Bootstrap type: "strat" (stratified, default) or "nonstrat".

- is_struct:

  Logical, TRUE for structure PLS (don't permute conditions).

- permsamp:

  Optional permutation reordering matrix (`total_rows x num_perm`). For
  methods 3-6 this is treated as behavior-block permutation order.

- Tpermsamp:

  Optional task-block permutation matrix (`total_rows x num_perm`) for
  multiblock methods (4, 6).

- Bpermsamp:

  Optional behavior-block permutation matrix (`total_rows x num_perm`)
  for multiblock/behavior methods.

- bootsamp:

  Optional bootstrap reordering matrix (`total_rows x num_boot`).

- bootsamp_4beh:

  Optional behavior bootstrap matrix (`total_rows x num_boot`) for
  behavior/multiblock methods.

- parallel_config:

  Optional parallel execution config list with `backend` (`"future"` or
  `"sequential"`) and `workers`. This affects permutation testing and
  task-only bootstrap execution.

- progress:

  Logical, show progress messages.

## Value

A `pls_result` object containing:

- method:

  Integer, the PLS method used

- u:

  Salience matrix (voxels x LVs)

- s:

  Singular values

- v:

  Design/behavior loadings

- usc:

  Brain scores

- vsc:

  Design/behavior scores

- perm_result:

  Permutation test results (if num_perm \> 0)

- boot_result:

  Bootstrap results (if num_boot \> 0)

- splithalf_result:

  Split-half results (if num_split \> 0)

## Examples

``` r
# Simple task PLS with 2 groups, 3 conditions
set.seed(42)
datamat1 <- matrix(rnorm(20 * 3 * 100), 60, 100)  # 20 subj x 3 cond
datamat2 <- matrix(rnorm(15 * 3 * 100), 45, 100)  # 15 subj x 3 cond

result <- pls_analysis(
  datamat_lst = list(datamat1, datamat2),
  num_subj_lst = c(20, 15),
  num_cond = 3,
  method = 1
)
#> ℹ Stacking data matrices...
#> ℹ Computing covariance/correlation matrix...
#> ℹ Computing latent variables...
#> ℹ Computing scores...
#> ✔ PLS analysis complete
```
