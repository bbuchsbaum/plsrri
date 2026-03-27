# Benchmark Exact Fast Paths

Runs reproducible synthetic benchmarks comparing baseline and exact fast
paths for task PLS permutation and bootstrap workloads.

## Usage

``` r
benchmark_fast_paths(
  operations = c("bootstrap", "permutation"),
  method = 1L,
  num_subj_lst = c(16L, 16L),
  num_cond = 2L,
  n_features = 5000L,
  num_boot = 48L,
  num_perm = 48L,
  reps = 3L,
  workers = 2L,
  seed = 1L
)
```

## Arguments

- operations:

  Character vector containing `"bootstrap"` and/or `"permutation"`.

- method:

  Task method to benchmark: `1L` (rotated task PLS) or `2L` (non-rotated
  task PLS).

- num_subj_lst:

  Balanced subject counts by group.

- num_cond:

  Number of conditions.

- n_features:

  Number of features/voxels.

- num_boot:

  Number of bootstrap samples for bootstrap benchmarks.

- num_perm:

  Number of permutation samples for permutation benchmarks.

- reps:

  Number of repeated timing runs per scenario.

- workers:

  Number of workers for the parallel fast-path scenario.

- seed:

  Random seed for synthetic data generation.

## Value

A data frame with timing results by operation and scenario.
