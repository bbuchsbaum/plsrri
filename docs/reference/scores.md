# Extract Scores

Extracts brain scores (usc) or design/behavior scores (vsc) from a PLS
result.

## Usage

``` r
scores(x, type = "brain", lv = NULL)
```

## Arguments

- x:

  A `pls_result` object

- type:

  "brain" (default), "design", or "behavior"

- lv:

  Latent variable index or vector of indices (NULL = all)

## Value

Matrix of scores

## Examples

``` r
set.seed(42)
data1 <- matrix(rnorm(60 * 50), 60, 50)
data2 <- matrix(rnorm(54 * 50), 54, 50)
result <- quick_pls(list(data1, data2), c(20, 18), 3, progress = FALSE)
brain_scores <- scores(result, type = "brain")
design_scores <- scores(result, type = "design")
```
