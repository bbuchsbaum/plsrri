# Compute Task Means with Unequal Group Sizes

Computes condition means when subjects per condition vary
(single-subject or unbalanced designs). The n_subj_cond parameter
specifies subjects in each condition.

## Usage

``` r
pls_task_mean_ssb(datamat, n_subj_cond)
```

## Arguments

- datamat:

  Data matrix (sum(n_subj_cond) x n_features)

- n_subj_cond:

  Vector of subjects per condition

## Value

Matrix of condition means (n_cond x n_features)
