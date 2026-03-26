# Extract Saliences (Brain Loadings)

Extracts the salience matrix (u) from a PLS result. Optionally converts
to NeuroVol if mask is available.

## Usage

``` r
salience(x, lv = NULL, as_neurovol = FALSE)
```

## Arguments

- x:

  A `pls_result` object

- lv:

  Latent variable index or vector of indices (NULL = all)

- as_neurovol:

  Logical, convert to SparseNeuroVol if mask available

## Value

Matrix of saliences, or SparseNeuroVol if as_neurovol=TRUE

## Examples

``` r
set.seed(42)
data1 <- matrix(rnorm(60 * 50), 60, 50)
data2 <- matrix(rnorm(54 * 50), 54, 50)
result <- quick_pls(list(data1, data2), c(20, 18), 3, progress = FALSE)
sal <- salience(result, lv = 1)
```
