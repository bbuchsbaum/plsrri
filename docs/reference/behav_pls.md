# Behavioral Correlates PLS

Convenience function for behavior PLS analysis, identifying brain
patterns that correlate with behavioral measures (reaction time,
accuracy, etc.).

## Usage

``` r
behav_pls(
  datamat_lst,
  behav_data,
  num_subj_lst,
  num_cond,
  cormode = 0L,
  nperm = 1000,
  nboot = 500,
  site = NULL,
  ...
)
```

## Arguments

- datamat_lst:

  List of data matrices (one per group).

- behav_data:

  Matrix of behavioral measures. Rows must match stacked datamat,
  columns are different behavioral measures.

- num_subj_lst:

  Integer vector with number of subjects per group.

- num_cond:

  Number of conditions.

- cormode:

  Correlation mode (0=Pearson, 2=covariance, 4=cosine, 6=dot).

- nperm:

  Number of permutations (default 1000).

- nboot:

  Number of bootstrap samples (default 500).

- site:

  Optional subject-level site labels used for multisite diagnostics
  after fitting.

- ...:

  Additional arguments passed to
  [`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md).

## Value

A `pls_result` object with class `pls_behavior`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Behavioral measures: RT and accuracy
behav <- cbind(rt = rt_data, accuracy = acc_data)

result <- behav_pls(
  datamat_lst = list(brain_data),
  behav_data = behav,
  num_subj_lst = 30,
  num_cond = 4
)
} # }
```
