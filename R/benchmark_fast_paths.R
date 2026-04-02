#' Benchmark Exact Fast Paths
#'
#' @description
#' Runs reproducible synthetic benchmarks comparing baseline and exact fast
#' paths for task PLS permutation and bootstrap workloads.
#'
#' @param operations Character vector containing `"bootstrap"` and/or
#'   `"permutation"`.
#' @param method Task method to benchmark: `1L` (rotated task PLS) or `2L`
#'   (non-rotated task PLS).
#' @param num_subj_lst Balanced subject counts by group.
#' @param num_cond Number of conditions.
#' @param n_features Number of features/voxels.
#' @param num_boot Number of bootstrap samples for bootstrap benchmarks.
#' @param num_perm Number of permutation samples for permutation benchmarks.
#' @param reps Number of repeated timing runs per scenario.
#' @param workers Number of workers for the parallel fast-path scenario.
#' @param seed Random seed for synthetic data generation.
#'
#' @return A data frame with timing results by operation and scenario.
#' @export
benchmark_fast_paths <- function(operations = c("bootstrap", "permutation"),
                                 method = 1L,
                                 num_subj_lst = c(16L, 16L),
                                 num_cond = 2L,
                                 n_features = 5000L,
                                 num_boot = 48L,
                                 num_perm = 48L,
                                 reps = 3L,
                                 workers = 2L,
                                 seed = 1L) {
  operations <- match.arg(operations, c("bootstrap", "permutation"), several.ok = TRUE)
  method <- as.integer(method)
  reps <- as.integer(reps)
  workers <- as.integer(workers)

  if (!method %in% c(1L, 2L)) {
    stop("benchmark_fast_paths currently supports method 1 or 2")
  }
  if (!is.numeric(num_subj_lst) || length(num_subj_lst) < 1L) {
    stop("num_subj_lst must be a numeric vector of balanced group sizes")
  }

  num_subj_lst <- as.integer(num_subj_lst)
  num_groups <- length(num_subj_lst)

  set.seed(seed)
  datamat_lst <- lapply(num_subj_lst, function(n) {
    matrix(stats::rnorm(as.integer(n) * as.integer(num_cond) * as.integer(n_features)),
           nrow = as.integer(n) * as.integer(num_cond),
           ncol = as.integer(n_features))
  })
  stacked_datamat <- do.call(rbind, datamat_lst)

  stacked_designdata <- NULL
  if (method == 2L) {
    stacked_designdata <- diag(num_groups * as.integer(num_cond))
  }

  observed <- pls_analysis(
    datamat_lst = datamat_lst,
    num_subj_lst = num_subj_lst,
    num_cond = as.integer(num_cond),
    method = method,
    stacked_designdata = stacked_designdata,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  bootsamp <- if ("bootstrap" %in% operations) {
    pls_boot_order(
      num_subj_lst = num_subj_lst,
      num_cond = as.integer(num_cond),
      num_boot = as.integer(num_boot),
      incl_seq = (method == 1L),
      boot_type = "strat"
    )
  } else {
    NULL
  }

  permsamp <- if ("permutation" %in% operations) {
    pls_perm_order(
      num_subj_lst = num_subj_lst,
      num_cond = as.integer(num_cond),
      num_perm = as.integer(num_perm),
      not_in_cond = FALSE
    )
  } else {
    NULL
  }

  run_with_fast_option <- function(enabled, operation, expr) {
    old <- getOption("plsrri.fast_paths")
    if (isTRUE(enabled)) {
      fast_opt <- if (operation == "permutation") {
        c("xcor", "bootstrap", "permutation")
      } else {
        c("xcor", "bootstrap")
      }
    } else {
      fast_opt <- FALSE
    }
    options(plsrri.fast_paths = fast_opt)
    on.exit(options(plsrri.fast_paths = old), add = TRUE)
    force(expr)
  }

  bench_once <- function(operation, scenario, parallel_config = NULL) {
    elapsed <- system.time({
      if (operation == "bootstrap") {
        run_with_fast_option(
          scenario != "baseline",
          operation,
          pls_bootstrap_test(
            stacked_datamat = stacked_datamat,
            stacked_designdata = if (!is.null(stacked_designdata)) normalize_rows(stacked_designdata, margin = 2L) else NULL,
            num_groups = num_groups,
            num_subj_lst = num_subj_lst,
            num_cond = as.integer(num_cond),
            method = method,
            num_boot = ncol(bootsamp),
            observed_u = observed$u,
            observed_v = observed$v,
            observed_s = observed$s,
            bootsamp = bootsamp,
            clim = 95,
            parallel_config = parallel_config,
            progress = FALSE
          )
        )
      } else {
        run_with_fast_option(
          scenario != "baseline",
          operation,
          pls_permutation_test(
            stacked_datamat = stacked_datamat,
            stacked_designdata = if (!is.null(stacked_designdata)) normalize_rows(stacked_designdata, margin = 2L) else NULL,
            num_groups = num_groups,
            num_subj_lst = num_subj_lst,
            num_cond = as.integer(num_cond),
            method = method,
            num_perm = ncol(permsamp),
            observed_s = observed$s,
            observed_v = observed$v,
            meancentering_type = 0L,
            permsamp = permsamp,
            parallel_config = parallel_config,
            progress = FALSE
          )
        )
      }
    })[["elapsed"]]

    data.frame(
      operation = operation,
      method = method,
      scenario = scenario,
      workers = if (is.null(parallel_config) || is.null(parallel_config$workers)) {
        1L
      } else {
        as.integer(parallel_config$workers)
      },
      elapsed_sec = as.numeric(elapsed),
      num_groups = num_groups,
      num_cond = as.integer(num_cond),
      n_features = as.integer(n_features),
      num_boot = as.integer(num_boot),
      num_perm = as.integer(num_perm),
      stringsAsFactors = FALSE
    )
  }

  scenarios <- list(
    baseline = list(parallel_config = list(backend = "sequential", workers = 1L)),
    exact_fast = list(parallel_config = list(backend = "sequential", workers = 1L))
  )
  if (workers > 1L && requireNamespace("future", quietly = TRUE)) {
    scenarios$exact_fast_parallel <- list(
      parallel_config = list(backend = "future", workers = workers)
    )
  }

  out <- vector("list", 0L)
  idx <- 0L
  for (operation in operations) {
    for (scenario_name in names(scenarios)) {
      for (rep_idx in seq_len(reps)) {
        idx <- idx + 1L
        out[[idx]] <- cbind(
          bench_once(operation, scenario_name, scenarios[[scenario_name]]$parallel_config),
          rep = rep_idx
        )
      }
    }
  }

  do.call(rbind, out)
}
