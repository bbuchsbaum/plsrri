# Add Conditions to PLS Specification

Specifies the condition structure for the PLS analysis. Conditions
define how the data matrix rows are organized.

## Usage

``` r
add_conditions(spec, conditions, labels = NULL)
```

## Arguments

- spec:

  A `pls_spec` object

- conditions:

  Either:

  - An integer specifying the number of conditions

  - A character vector of condition labels

  - A data frame with condition information (for BIDS events)

- labels:

  Optional character vector of condition labels (if conditions is an
  integer)

## Value

Updated `pls_spec` object

## Examples

``` r
set.seed(42)
data1 <- matrix(rnorm(60 * 50), 60, 50)  # 20 subjects x 3 conditions
data2 <- matrix(rnorm(54 * 50), 54, 50)  # 18 subjects x 3 conditions

spec <- pls_spec() |>
  add_subjects(list(data1, data2), groups = c(20, 18)) |>
  add_conditions(3, labels = c("baseline", "task1", "task2"))
```
