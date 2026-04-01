# Add Design Contrasts to PLS Specification

Adds design contrasts for non-rotated PLS methods. Design contrasts
specify hypotheses about condition/group differences.

## Usage

``` r
add_design(spec, contrasts, labels = NULL)
```

## Arguments

- spec:

  A `pls_spec` object

- contrasts:

  Contrast matrix where:

  - For method 2 (non-rotated task): rows = groups x conditions

  - For method 5 (non-rotated behavior): rows = groups x conditions x
    measures

  - For method 6 (non-rotated multiblock): rows = task rows + behavior
    rows

  - Columns = number of contrasts/LVs to extract

- labels:

  Optional character vector of contrast labels

## Value

Updated `pls_spec` object

## Examples

``` r
# Create brain data for 2 groups
set.seed(42)
d1 <- matrix(rnorm(60 * 50), 60, 50)  # 20 subjects x 3 conditions
d2 <- matrix(rnorm(54 * 50), 54, 50)  # 18 subjects x 3 conditions

# 2 groups x 3 conditions = 6 rows, testing 2 contrasts
contrasts <- matrix(c(
  1, 0,   # g1c1
  0, 1,   # g1c2
 -1,-1,   # g1c3
  1, 0,   # g2c1
  0, 1,   # g2c2
 -1,-1    # g2c3
), ncol = 2, byrow = TRUE)

spec <- pls_spec() |>
  add_subjects(list(d1, d2), groups = c(20, 18)) |>
  add_conditions(3) |>
  add_design(contrasts, labels = c("task_vs_baseline", "task1_vs_task2"))
#> i Contrasts are not orthogonal. LV effects may overlap.
```
