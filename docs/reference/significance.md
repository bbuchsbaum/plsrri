# Get Significance Values (P-values)

Extracts permutation-based p-values for each latent variable.

## Usage

``` r
significance(x, lv = NULL, threshold = NULL)
```

## Arguments

- x:

  A `pls_result` object

- lv:

  Latent variable index or vector of indices (NULL = all)

- threshold:

  P-value threshold for significance (default 0.05)

## Value

Named vector of p-values, or data frame with significance info

## Examples

``` r
set.seed(42)
data1 <- matrix(rnorm(60 * 50), 60, 50)
data2 <- matrix(rnorm(54 * 50), 54, 50)
result <- quick_pls(list(data1, data2), c(20, 18), 3, nperm = 20, progress = FALSE)
pvals <- significance(result)
sig_lvs <- significance(result, threshold = 0.05)
```
