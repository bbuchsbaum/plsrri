# Add Behavior Data to PLS Specification

Adds behavioral measures for behavior PLS or multiblock PLS. The
behavior data should have one row per observation (subject x condition).

## Usage

``` r
add_behavior(spec, data, measures = NULL, block_conditions = NULL)
```

## Arguments

- spec:

  A `pls_spec` object

- data:

  Behavior data as:

  - A matrix (observations x measures)

  - A data frame with measure columns

- measures:

  Optional character vector of measure names

- block_conditions:

  For multiblock PLS: which conditions to use in the behavior block
  (default all)

## Value

Updated `pls_spec` object

## Examples

``` r
# Create brain data: 30 subjects x 3 conditions = 90 rows, 50 features
set.seed(42)
brain_data <- matrix(rnorm(90 * 50), 90, 50)

# Behavior data: 90 rows, 4 measures
behav_data <- matrix(rnorm(90 * 4), 90, 4)
colnames(behav_data) <- c("accuracy", "rt", "confidence", "effort")

spec <- pls_spec() |>
  add_subjects(list(brain_data), groups = 30) |>
  add_conditions(3) |>
  add_behavior(behav_data)
```
