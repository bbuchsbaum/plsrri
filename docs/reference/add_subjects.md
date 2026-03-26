# Add Subjects to PLS Specification

Adds subject data to a PLS specification. Can load from BIDS directories
(if bidser is available) or accept pre-loaded data matrices.

## Usage

``` r
add_subjects(
  spec,
  data,
  groups = NULL,
  task = NULL,
  space = "MNI152NLin2009cAsym",
  mask_method = "intersection",
  ...
)
```

## Arguments

- spec:

  A `pls_spec` object

- data:

  Either:

  - A character path to a BIDS directory

  - A list of data matrices (one per group)

  - A single data matrix (one group)

- groups:

  Character vector of group labels (for BIDS) or integer vector of
  subjects per group (for matrices)

- task:

  Task name (for BIDS)

- space:

  Space name for fMRIPrep derivatives (default "MNI152NLin2009cAsym")

- mask_method:

  How to handle brain mask:

  - "intersection": use intersection of all subject masks

  - "union": use union of all subject masks

  - "first": use first subject's mask

  - "provided": use mask from pls_spec

- ...:

  Additional arguments passed to bidser functions

## Value

Updated `pls_spec` object

## Examples

``` r
# From pre-loaded matrices
set.seed(42)
group1_data <- matrix(rnorm(60 * 50), 60, 50)
group2_data <- matrix(rnorm(54 * 50), 54, 50)

spec <- pls_spec() |>
  add_subjects(list(group1_data, group2_data), groups = c(20, 18))

# From BIDS (requires bidser)
if (FALSE) { # \dontrun{
spec <- pls_spec() |>
  add_subjects("/path/to/bids", groups = c("control", "patient"), task = "rest")
} # }
```
