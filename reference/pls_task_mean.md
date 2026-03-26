# Compute Task Means

Computes the mean for each condition from a stacked data matrix where
data is organized as subjects within conditions (n_subj \* n_cond rows).

## Usage

``` r
pls_task_mean(datamat, n_subj)
```

## Arguments

- datamat:

  Data matrix (n_subj\*n_cond x n_features)

- n_subj:

  Number of subjects

## Value

Matrix of condition means (n_cond x n_features)

## Examples

``` r
# 10 subjects, 3 conditions, 50 features
datamat <- matrix(rnorm(10 * 3 * 50), 30, 50)
means <- pls_task_mean(datamat, n_subj = 10)
```
