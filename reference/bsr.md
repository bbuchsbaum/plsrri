# Extract Bootstrap Ratios

Extracts the bootstrap ratio (BSR) from a PLS result. BSR = salience /
standard error, similar to a z-score.

## Usage

``` r
bsr(x, lv = NULL, threshold = NULL, as_neurovol = FALSE)
```

## Arguments

- x:

  A `pls_result` object

- lv:

  Latent variable index or vector of indices (NULL = all)

- threshold:

  Optional threshold for masking (e.g., \|BSR\| \> 3)

- as_neurovol:

  Logical, convert to SparseNeuroVol if mask available

## Value

Matrix of bootstrap ratios, or SparseNeuroVol

## Examples

``` r
set.seed(42)
data1 <- matrix(rnorm(60 * 50), 60, 50)
data2 <- matrix(rnorm(54 * 50), 54, 50)
result <- quick_pls(list(data1, data2), c(20, 18), 3, nboot = 20, progress = FALSE)
bsr_map <- bsr(result, lv = 1, threshold = 2)
```
