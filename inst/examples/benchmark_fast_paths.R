library(plsrri)

# Compact reproducible benchmark for the exact task-PLS fast paths.
res <- benchmark_fast_paths(
  operations = c("bootstrap", "permutation"),
  method = 1L,
  num_subj_lst = c(12L, 12L),
  num_cond = 2L,
  n_features = 10000L,
  num_boot = 32L,
  num_perm = 32L,
  reps = 3L,
  workers = 2L,
  seed = 1L
)

print(res)
print(
  aggregate(
    elapsed_sec ~ operation + scenario,
    data = res,
    FUN = median
  )
)
