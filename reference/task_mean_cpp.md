# Fast Task Mean Computation

Computes task means from stacked data matrix.

## Usage

``` r
task_mean_cpp(datamat, n_subj, n_cond)
```

## Arguments

- datamat:

  Data matrix (n_subj\*n_cond x n_features)

- n_subj:

  Number of subjects

- n_cond:

  Number of conditions

## Value

Task mean matrix (n_cond x n_features)
