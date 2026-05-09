#' Optional multifer Adapter Bridge
#'
#' @description
#' Internal glue for routing selected plsrri fits through multifer's
#' adapter-owned inference engine. The bridge is deliberately optional:
#' the McIntosh path remains the default and the package can load without
#' multifer installed.
#'
#' @name multifer-adapter
#' @keywords internal
NULL

.plsrri_register_multifer_adapters <- function() {
  if (!requireNamespace("multifer", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  multifer::register_infer_adapter(
    "plsrri_task",
    adapter_plsrri_task(),
    overwrite = TRUE
  )

  invisible(TRUE)
}

adapter_plsrri_task <- function() {
  if (!requireNamespace("multifer", quietly = TRUE)) {
    stop("Package 'multifer' is required to create plsrri adapters.", call. = FALSE)
  }

  multifer::infer_adapter(
    adapter_id = "plsrri_task",
    adapter_version = "0.1.0",
    shape_kinds = "cross",
    capabilities = multifer::capability_matrix(
      list(
        geometry = "cross",
        relation = "covariance",
        targets = c(
          "component_significance",
          "variable_stability",
          "score_stability",
          "subspace_stability"
        )
      )
    ),
    roots = function(x, ...) x$s^2,
    domains = function(x, data = NULL, ...) c("X", "Y"),
    loadings = function(x, domain = c("X", "Y"), ...) {
      domain <- match.arg(domain)
      if (identical(domain, "X")) x$u else x$v
    },
    refit = function(x, new_data, ...) {
      .plsrri_multifer_fit_task(new_data)
    },
    bootstrap_action = function(x, data, design, replicate = NULL, ...) {
      .plsrri_multifer_bootstrap_action(x, data, replicate = replicate)
    },
    project_scores = function(x, data, domain = c("X", "Y"), ...) {
      .plsrri_multifer_project_scores(x, data, domain = domain)
    },
    null_action = function(x, data, ...) {
      .plsrri_multifer_null_action(x, data)
    },
    component_stat = function(x, data, k, split = NULL, ...) {
      fit <- .plsrri_multifer_fit_task(data)
      idx <- max(1L, as.integer(k)[1])
      if (idx > length(fit$s)) {
        return(0)
      }
      fit$s[[idx]]^2
    },
    residualize = function(x, k, data, ...) {
      .plsrri_multifer_residualize_task(x, k = k, data = data)
    },
    validity_level = "conditional",
    declared_assumptions = c(
      "conditions_within_subjects_exchangeable",
      "subjects_across_groups_exchangeable",
      "adapter_owned_cross_component_execution"
    ),
    checked_assumptions = list(
      list(
        name = "plsrri_task_payload",
        check = .plsrri_multifer_check_task_payload,
        detail = "data must be a valid method-1 task PLS payload"
      )
    ),
    component_execution = "adapter"
  )
}

.run_pls_multifer <- function(spec, progress = TRUE, ...) {
  if (!requireNamespace("multifer", quietly = TRUE)) {
    stop(
      "inference = 'multifer' requires package 'multifer'. ",
      "Install multifer or use inference = 'mcintosh'.",
      call. = FALSE
    )
  }
  if (!.plsrri_multifer_has_cross_adapter_execution()) {
    stop(
      "inference = 'multifer' requires a multifer build that supports ",
      "component_execution = 'adapter' for geometry = 'cross'. ",
      "Update multifer or use inference = 'mcintosh'.",
      call. = FALSE
    )
  }
  .plsrri_register_multifer_adapters()

  if (!identical(as.integer(spec$method), 1L)) {
    stop(
      "multifer inference is currently wired for method 1 task PLS only.",
      call. = FALSE
    )
  }
  if (isTRUE(spec$num_split > 0L)) {
    stop(
      "multifer inference does not currently produce plsrri split-half results.",
      call. = FALSE
    )
  }

  base_fit <- pls_analysis(
    datamat_lst = spec$datamat_lst,
    num_subj_lst = spec$num_subj_lst,
    num_cond = spec$num_cond,
    method = spec$method,
    num_perm = 0L,
    num_boot = 0L,
    num_split = 0L,
    clim = spec$clim,
    stacked_behavdata = spec$stacked_behavdata,
    stacked_designdata = spec$stacked_designdata,
    bscan = spec$bscan,
    meancentering_type = spec$meancentering_type,
    cormode = spec$cormode,
    boot_type = spec$boot_type,
    is_struct = spec$is_struct,
    parallel_config = spec$.parallel,
    progress = progress,
    ...
  )

  targets <- .plsrri_multifer_targets(spec)
  if (!length(targets)) {
    base_fit$inference_engine <- "multifer"
    return(base_fit)
  }

  payload <- .plsrri_multifer_payload_from_spec(spec)
  bundle <- multifer::infer(
    adapter = "plsrri_task",
    data = payload,
    geometry = "cross",
    relation = "covariance",
    targets = targets,
    B = max(1L, as.integer(spec$num_perm)),
    R = max(0L, as.integer(spec$num_boot)),
    model = base_fit,
    parallel = "sequential",
    return_bundle = TRUE
  )

  infer_result <- bundle$result
  artifacts <- bundle$artifacts

  if (spec$num_perm > 0L) {
    base_fit$perm_result <- .plsrri_multifer_perm_result(
      infer_result = infer_result,
      n_lv = length(base_fit$s),
      num_perm = as.integer(spec$num_perm)
    )
  }

  if (spec$num_boot > 0L) {
    base_fit$boot_result <- .plsrri_multifer_boot_result(
      artifact = artifacts$bootstrap,
      observed_fit = base_fit,
      clim = spec$clim,
      boot_type = spec$boot_type
    )
  }

  base_fit$multifer_result <- infer_result
  base_fit$multifer_artifacts <- artifacts
  base_fit$inference_engine <- "multifer"
  base_fit$multifer_adapter <- "plsrri_task"
  base_fit
}

.plsrri_multifer_targets <- function(spec) {
  targets <- character(0)
  if (isTRUE(spec$num_perm > 0L)) {
    targets <- c(targets, "component_significance")
  }
  if (isTRUE(spec$num_boot > 0L)) {
    targets <- c(
      targets,
      "variable_stability",
      "score_stability",
      "subspace_stability"
    )
  }
  unique(targets)
}

.plsrri_multifer_payload_from_spec <- function(spec) {
  .plsrri_multifer_sync_cross_payload(list(
    datamat_lst = spec$datamat_lst,
    num_subj_lst = spec$num_subj_lst,
    num_cond = spec$num_cond,
    method = as.integer(spec$method),
    stacked_behavdata = spec$stacked_behavdata,
    stacked_designdata = spec$stacked_designdata,
    bscan = spec$bscan,
    meancentering_type = as.integer(spec$meancentering_type),
    cormode = as.integer(spec$cormode),
    boot_type = spec$boot_type,
    is_struct = isTRUE(spec$is_struct),
    clim = spec$clim
  ))
}

.plsrri_multifer_fit_task <- function(data) {
  .plsrri_multifer_assert_task_payload(data)

  pls_analysis(
    datamat_lst = data$datamat_lst,
    num_subj_lst = data$num_subj_lst,
    num_cond = data$num_cond,
    method = data$method,
    num_perm = 0L,
    num_boot = 0L,
    num_split = 0L,
    clim = data$clim %||% 95,
    stacked_behavdata = data$stacked_behavdata,
    stacked_designdata = data$stacked_designdata,
    bscan = data$bscan,
    meancentering_type = data$meancentering_type %||% 0L,
    cormode = data$cormode %||% 0L,
    boot_type = data$boot_type %||% "strat",
    is_struct = isTRUE(data$is_struct),
    progress = FALSE
  )
}

.plsrri_multifer_null_action <- function(x, data) {
  order <- pls_perm_order(
    num_subj_lst = data$num_subj_lst,
    num_cond = data$num_cond,
    num_perm = 1L,
    not_in_cond = isTRUE(data$is_struct)
  )[, 1L]

  out <- .plsrri_multifer_reorder_payload(data, order)
  out$permsamp <- matrix(order, ncol = 1L)
  out
}

.plsrri_multifer_bootstrap_action <- function(x, data, replicate = NULL) {
  order <- pls_boot_order(
    num_subj_lst = data$num_subj_lst,
    num_cond = data$num_cond,
    num_boot = 1L,
    bscan = seq_len(data$num_cond),
    incl_seq = FALSE,
    boot_type = data$boot_type %||% "strat"
  )[, 1L]

  boot_data <- .plsrri_multifer_reorder_payload(data, order)
  list(
    fit = .plsrri_multifer_fit_task(boot_data),
    resample_indices = order,
    info = list(kind = "pls_boot_order", replicate = replicate)
  )
}

.plsrri_multifer_residualize_task <- function(x, k, data) {
  k <- max(1L, as.integer(k)[1])
  k <- min(k, ncol(x$u))
  u <- x$u[, seq_len(k), drop = FALSE]
  projector <- tcrossprod(u)

  data$datamat_lst <- lapply(data$datamat_lst, function(datamat) {
    datamat - datamat %*% projector
  })
  .plsrri_multifer_sync_cross_payload(data)
}

.plsrri_multifer_project_scores <- function(x, data, domain = c("X", "Y")) {
  domain <- match.arg(domain)
  if (identical(domain, "X")) {
    return(stack_datamats(data$datamat_lst) %*% x$u)
  }

  .plsrri_task_design_scores_from_v(
    v = x$v,
    num_subj_lst = data$num_subj_lst,
    num_cond = data$num_cond
  )
}

.plsrri_task_design_scores_from_v <- function(v, num_subj_lst, num_cond) {
  k <- as.integer(num_cond)
  num_groups <- length(num_subj_lst)
  num_col <- ncol(v)
  total_rows <- count_observations(num_subj_lst, k)
  vsc <- matrix(0, nrow = total_rows, ncol = num_col)

  is_ssb <- is.list(num_subj_lst)
  offset <- 0L
  for (g in seq_len(num_groups)) {
    n_vec <- if (is_ssb) as.integer(num_subj_lst[[g]]) else rep(as.integer(num_subj_lst[g]), k)
    v_group <- v[((g - 1L) * k + 1L):(g * k), , drop = FALSE]

    step <- 0L
    for (cond in seq_len(k)) {
      n <- n_vec[cond]
      rows <- offset + step + seq_len(n)
      vsc[rows, ] <- matrix(v_group[cond, ], nrow = n, ncol = num_col, byrow = TRUE)
      step <- step + n
    }
    offset <- offset + sum(n_vec)
  }

  vsc
}

.plsrri_multifer_reorder_payload <- function(data, order) {
  stacked <- stack_datamats(data$datamat_lst)
  if (length(order) != nrow(stacked)) {
    stop("Resampling order length does not match stacked data rows.", call. = FALSE)
  }

  reordered <- stacked[as.integer(order), , drop = FALSE]
  sizes <- .plsrri_group_row_counts(data$num_subj_lst, data$num_cond)
  data$datamat_lst <- .plsrri_split_stacked_datamat(reordered, sizes)
  .plsrri_multifer_sync_cross_payload(data)
}

.plsrri_multifer_sync_cross_payload <- function(data) {
  data$X <- stack_datamats(data$datamat_lst)
  if (is.null(data$Y) || !is.matrix(data$Y) || nrow(data$Y) != nrow(data$X)) {
    data$Y <- .plsrri_multifer_task_design_matrix(
      data$num_subj_lst,
      data$num_cond
    )
  }
  data$relation <- "covariance"
  data
}

.plsrri_multifer_task_design_matrix <- function(num_subj_lst, num_cond) {
  k <- as.integer(num_cond)
  num_groups <- length(num_subj_lst)
  n_rows <- count_observations(num_subj_lst, k)
  out <- matrix(0, nrow = n_rows, ncol = num_groups * k)
  colnames(out) <- unlist(lapply(seq_len(num_groups), function(g) {
    paste0("group", g, ":condition", seq_len(k))
  }), use.names = FALSE)

  is_ssb <- is.list(num_subj_lst)
  offset <- 0L
  for (g in seq_len(num_groups)) {
    n_vec <- if (is_ssb) {
      as.integer(num_subj_lst[[g]])
    } else {
      rep(as.integer(num_subj_lst[g]), k)
    }

    for (cond in seq_len(k)) {
      n <- n_vec[[cond]]
      if (n > 0L) {
        rows <- offset + seq_len(n)
        col <- (g - 1L) * k + cond
        out[rows, col] <- 1
      }
      offset <- offset + n
    }
  }

  out
}

.plsrri_group_row_counts <- function(num_subj_lst, num_cond) {
  if (is.list(num_subj_lst)) {
    return(vapply(num_subj_lst, function(x) sum(as.integer(x)), integer(1)))
  }
  as.integer(num_subj_lst) * as.integer(num_cond)
}

.plsrri_split_stacked_datamat <- function(stacked, sizes) {
  out <- vector("list", length(sizes))
  offset <- 0L
  for (i in seq_along(sizes)) {
    rows <- offset + seq_len(sizes[i])
    out[[i]] <- stacked[rows, , drop = FALSE]
    offset <- offset + sizes[i]
  }
  out
}

.plsrri_multifer_perm_result <- function(infer_result, n_lv, num_perm) {
  sprob <- rep(1, n_lv)
  sp <- rep(as.integer(num_perm), n_lv)

  tests <- infer_result$component_tests
  units <- infer_result$units
  members <- attr(units, "members")

  if (nrow(tests) > 0L) {
    for (i in seq_len(nrow(tests))) {
      unit_pos <- match(tests$unit_id[[i]], units$unit_id)
      if (is.na(unit_pos)) next
      member_idx <- as.integer(members[[unit_pos]])
      member_idx <- member_idx[member_idx >= 1L & member_idx <= n_lv]
      if (!length(member_idx)) next

      p <- as.numeric(tests$p_value[[i]])
      stopped_at <- as.integer(tests$stopped_at[[i]])
      if (!is.finite(p)) p <- 1
      if (!is.finite(stopped_at) || stopped_at < 1L) stopped_at <- as.integer(num_perm)
      r <- round(p * (stopped_at + 1L) - 1L)
      r <- max(0L, min(as.integer(num_perm), as.integer(r)))

      sprob[member_idx] <- p
      sp[member_idx] <- r
    }
  }

  new_pls_perm_result(
    num_perm = as.integer(num_perm),
    sp = as.integer(sp),
    sprob = sprob
  )
}

.plsrri_multifer_boot_result <- function(artifact, observed_fit, clim, boot_type) {
  if (is.null(artifact) || !inherits(artifact, "multifer_bootstrap_artifact")) {
    return(NULL)
  }

  n_features <- nrow(observed_fit$u)
  n_lv <- ncol(observed_fit$u)
  R <- as.integer(artifact$R)
  scaled <- array(NA_real_, dim = c(n_features, n_lv, R))
  bootsamp <- matrix(NA_integer_, nrow = nrow(observed_fit$usc), ncol = R)

  for (b in seq_len(R)) {
    rep <- artifact$reps[[b]]
    load_x <- rep$aligned_loadings$X
    fit_b <- rep$fit
    k <- min(ncol(load_x), length(fit_b$s), n_lv)
    if (k > 0L) {
      scaled[, seq_len(k), b] <- sweep(
        load_x[, seq_len(k), drop = FALSE],
        2L,
        fit_b$s[seq_len(k)],
        `*`
      )
    }
    if (!is.null(rep$resample_indices) && length(rep$resample_indices) == nrow(bootsamp)) {
      bootsamp[, b] <- as.integer(rep$resample_indices)
    }
  }

  u_se <- apply(scaled, c(1L, 2L), stats::sd, na.rm = TRUE)
  bad <- !is.finite(u_se) | u_se <= 0
  u_se[bad] <- 1

  observed_scaled <- sweep(observed_fit$u, 2L, observed_fit$s, `*`)
  compare_u <- observed_scaled / u_se
  compare_u[bad] <- 0
  compare_u[!is.finite(compare_u)] <- 0

  new_pls_boot_result(
    num_boot = R,
    boot_type = boot_type %||% "strat",
    compare_u = compare_u,
    u_se = u_se,
    clim = clim,
    bootsamp = bootsamp
  )
}

.plsrri_multifer_check_task_payload <- function(data, ...) {
  tryCatch(
    {
      .plsrri_multifer_assert_task_payload(data)
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
}

.plsrri_multifer_assert_task_payload <- function(data) {
  if (!is.list(data)) {
    stop("data must be a list payload", call. = FALSE)
  }
  if (is.null(data$datamat_lst) || !is.list(data$datamat_lst) ||
      !length(data$datamat_lst) ||
      !all(vapply(data$datamat_lst, is.matrix, logical(1)))) {
    stop("data$datamat_lst must be a non-empty list of matrices", call. = FALSE)
  }
  if (length(unique(vapply(data$datamat_lst, ncol, integer(1)))) != 1L) {
    stop("all datamat matrices must have the same number of columns", call. = FALSE)
  }
  if (is.null(data$num_subj_lst)) {
    stop("data$num_subj_lst is required", call. = FALSE)
  }
  if (is.null(data$num_cond) || !is.numeric(data$num_cond) || data$num_cond < 1L) {
    stop("data$num_cond must be a positive integer", call. = FALSE)
  }
  if (!identical(as.integer(data$method), 1L)) {
    stop("plsrri_task currently supports method 1 only", call. = FALSE)
  }
  expected <- .plsrri_group_row_counts(data$num_subj_lst, data$num_cond)
  observed <- vapply(data$datamat_lst, nrow, integer(1))
  if (!identical(as.integer(expected), as.integer(observed))) {
    stop("datamat row counts do not match num_subj_lst x num_cond", call. = FALSE)
  }
  if (!is.matrix(data$X) || !is.numeric(data$X)) {
    stop("data$X must be a numeric matrix", call. = FALSE)
  }
  if (!is.matrix(data$Y) || !is.numeric(data$Y)) {
    stop("data$Y must be a numeric matrix", call. = FALSE)
  }
  if (nrow(data$X) != nrow(data$Y)) {
    stop("data$X and data$Y must have the same number of rows", call. = FALSE)
  }
  if (nrow(data$X) != sum(observed)) {
    stop("data$X row count must match the stacked datamat rows", call. = FALSE)
  }
  invisible(TRUE)
}

.plsrri_multifer_has_cross_adapter_execution <- local({
  cached <- NULL

  function() {
    if (!is.null(cached)) {
      return(cached)
    }
    if (!requireNamespace("multifer", quietly = TRUE)) {
      cached <<- FALSE
      return(cached)
    }

    seen <- new.env(parent = emptyenv())
    seen$component_stat <- 0L
    seen$null_action <- 0L
    seen$residualize <- 0L
    adapter_id <- paste0(
      "plsrri_cross_probe_",
      Sys.getpid(),
      "_",
      sample.int(1000000L, 1L)
    )

    adapter <- tryCatch(
      multifer::infer_adapter(
        adapter_id = adapter_id,
        shape_kinds = "cross",
        capabilities = multifer::capability_matrix(
          list(
            geometry = "cross",
            relation = "covariance",
            targets = "component_significance"
          )
        ),
        roots = function(x, ...) c(1, 0.5),
        loadings = function(x, domain = c("X", "Y"), ...) {
          matrix(1, nrow = 2L, ncol = 2L)
        },
        scores = function(x, domain = c("X", "Y"), ...) {
          matrix(1, nrow = 3L, ncol = 2L)
        },
        refit = function(x, new_data, ...) {
          list()
        },
        component_stat = function(x, data, k, ...) {
          seen$component_stat <- seen$component_stat + 1L
          if (isTRUE(data$.plsrri_probe_null)) 0 else 1
        },
        null_action = function(x, data, ...) {
          seen$null_action <- seen$null_action + 1L
          data$.plsrri_probe_null <- TRUE
          data
        },
        residualize = function(x, k, data, ...) {
          seen$residualize <- seen$residualize + 1L
          data
        },
        validity_level = "conditional",
        component_execution = "adapter"
      ),
      error = function(e) e
    )
    if (inherits(adapter, "error")) {
      cached <<- FALSE
      return(cached)
    }

    registered <- FALSE
    result <- tryCatch(
      {
        multifer::register_infer_adapter(adapter_id, adapter, overwrite = TRUE)
        registered <- TRUE
        multifer::infer(
          adapter = adapter_id,
          data = list(
            X = matrix(rnorm(6), nrow = 3L),
            Y = matrix(rnorm(6), nrow = 3L),
            relation = "covariance"
          ),
          geometry = "cross",
          relation = "covariance",
          targets = "component_significance",
          B = 1L,
          R = 0L,
          alpha = 0.999,
          model = list(),
          parallel = "sequential"
        )
      },
      error = function(e) e
    )

    if (isTRUE(registered)) {
      try(multifer::unregister_infer_adapter(adapter_id), silent = TRUE)
    }

    cached <<- !inherits(result, "error") &&
      seen$component_stat > 0L &&
      seen$null_action > 0L &&
      seen$residualize > 0L
    cached
  }
})
