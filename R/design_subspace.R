#' Factorial Design Object for Task PLS
#'
#' @description
#' Defines a factorial design over Task PLS group-condition cells. The design
#' object stores the cell-level formula and condition metadata used by
#' design-subspace decomposition and observed subspace statistics.
#'
#' @param formula One-sided model formula, such as `~ group * task * level`.
#' @param condition_key Optional data frame with a `condition` column and one
#'   row per PLS condition. Additional columns define condition-level factors.
#' @param subject Optional subject identifier column name for future
#'   exchangeability-aware resampling.
#' @param between Optional between-subject factor names.
#' @param within Optional within-subject factor names.
#' @param contrasts Contrast coding. Currently `"sum"` is used for unordered
#'   factors.
#'
#' @return A `pls_design` object.
#' @export
pls_design <- function(formula,
                       condition_key = NULL,
                       subject = NULL,
                       between = NULL,
                       within = NULL,
                       contrasts = "sum") {
  formula <- .pls_one_sided_formula(formula)
  condition_key <- .validate_condition_key(condition_key, conditions = NULL)

  structure(
    list(
      formula = formula,
      condition_key = condition_key,
      subject = subject,
      between = between,
      within = within,
      contrasts = contrasts
    ),
    class = "pls_design"
  )
}

#' Cell Table for a Task PLS Factorial Design
#'
#' @description
#' Builds the group-condition cell table whose rows match the row order of the
#' Task PLS cross-block matrix: all conditions for group 1, then all conditions
#' for group 2, and so on.
#'
#' @param x A `pls_result`.
#' @param design Optional `pls_design` object.
#' @param condition_key Optional condition metadata. Ignored when supplied by
#'   `design`.
#'
#' @return A data frame with one row per Task PLS design cell.
#' @export
design_cell_table <- function(x, design = NULL, condition_key = NULL) {
  if (!inherits(x, "pls_result")) {
    stop("x must be a pls_result.", call. = FALSE)
  }

  if (!is.null(design) && !inherits(design, "pls_design")) {
    stop("design must be a pls_design object.", call. = FALSE)
  }

  num_subj_lst <- x$num_subj_lst
  num_cond <- as.integer(x$num_cond)
  num_groups <- length(num_subj_lst)
  groups <- as.character(x$groups %||% paste0("Group", seq_len(num_groups)))
  conditions <- as.character(x$conditions %||% paste0("Cond", seq_len(num_cond)))

  if (length(groups) != num_groups) {
    stop("result group labels do not match num_subj_lst.", call. = FALSE)
  }
  if (length(conditions) != num_cond) {
    stop("result condition labels do not match num_cond.", call. = FALSE)
  }

  condition_key <- if (!is.null(design)) design$condition_key else condition_key
  condition_key <- .validate_condition_key(condition_key, conditions = conditions)

  cell_n <- .task_pls_cell_counts(num_subj_lst, num_cond)
  out <- data.frame(
    row_index = seq_len(num_groups * num_cond),
    group = rep(groups, each = num_cond),
    condition = rep(conditions, times = num_groups),
    cell_n = cell_n,
    stringsAsFactors = FALSE
  )

  if (!is.null(condition_key)) {
    ordered_key <- condition_key[match(out$condition, as.character(condition_key$condition)), , drop = FALSE]
    ordered_key$condition <- NULL
    out <- cbind(out, ordered_key)
  }

  out$group <- factor(out$group, levels = groups)
  out$condition <- factor(out$condition, levels = conditions)
  out <- .factorize_design_columns(out)
  .assert_task_pls_cell_info(x, out)
  out
}

#' Build Centered Design Projectors
#'
#' @description
#' Builds QR-based orthonormal bases and optional projection matrices for each
#' factorial term after applying the same Task PLS cell-centering operator used
#' to create the stored cross-block matrix.
#'
#' @param result A Task PLS `pls_result`.
#' @param design Optional `pls_design` object.
#' @param formula Optional one-sided formula overriding `design$formula`.
#' @param condition_key Optional condition metadata.
#' @param weights `"cell_equal"`, `"subject_count"`, or `"row_weights"` to
#'   use stored `result$task_pls$row_weights`.
#' @param include_matrix Logical; include dense projection matrices.
#'
#' @return A named list of projector descriptors.
#' @export
design_projectors <- function(result,
                              design = NULL,
                              formula = NULL,
                              condition_key = NULL,
                              weights = c("cell_equal", "subject_count", "row_weights"),
                              include_matrix = FALSE) {
  weights <- match.arg(weights)
  ctx <- .design_subspace_context(
    result = result,
    design = design,
    formula = formula,
    condition_key = condition_key,
    weights = weights,
    require_crossblock = FALSE
  )

  out <- vector("list", length(ctx$terms))
  names(out) <- ctx$terms

  for (term_id in seq_along(ctx$terms)) {
    columns <- which(ctx$assign == term_id)
    basis <- .weighted_basis(ctx$centered_model[, columns, drop = FALSE], ctx$weights)
    item <- list(
      term = ctx$terms[[term_id]],
      rank = basis$rank,
      basis = basis$q,
      weights = ctx$weights
    )
    if (isTRUE(include_matrix)) {
      item$projector <- basis$q %*% t(basis$q)
    }
    out[[term_id]] <- item
  }

  out
}

#' Term-Specific Design-Subspace SVDs for Task PLS
#'
#' @description
#' Fits ASCA-like term-specific PLS decompositions by projecting the Task PLS
#' cross-block matrix into centered factorial design subspaces, then computing
#' an SVD within each projected subspace.
#'
#' @param result A Task PLS `pls_result` produced with
#'   `pls_analysis(..., keep_crossblock = TRUE)` or
#'   `run(..., keep_crossblock = TRUE)`.
#' @param design Optional `pls_design` object.
#' @param formula Optional one-sided formula overriding `design$formula`.
#' @param condition_key Optional condition metadata.
#' @param terms `"all"` or a character vector of term labels to decompose.
#' @param ncomp Optional number of components to retain per term.
#' @param statistic `"trace"` for total subspace covariance energy, or
#'   `"largest_root"` for the dominant singular-root statistic.
#' @param weights `"cell_equal"`, `"subject_count"`, or `"row_weights"` to
#'   use stored `result$task_pls$row_weights`.
#' @param keep_crossblocks Logical; retain projected cross-block matrices.
#'
#' @return A `pls_design_subspace_svd` object with term-level statistics and
#'   component-level singular values.
#' @export
design_subspace_svd <- function(result,
                                design = NULL,
                                formula = NULL,
                                condition_key = NULL,
                                terms = "all",
                                ncomp = NULL,
                                statistic = c("trace", "largest_root"),
                                weights = c("cell_equal", "subject_count", "row_weights"),
                                keep_crossblocks = FALSE) {
  statistic <- match.arg(statistic)
  weights <- match.arg(weights)

  ctx <- .design_subspace_context(
    result = result,
    design = design,
    formula = formula,
    condition_key = condition_key,
    weights = weights,
    require_crossblock = TRUE
  )

  term_ids <- .select_design_terms(terms, ctx$terms)
  term_fits <- vector("list", length(term_ids))
  names(term_fits) <- ctx$terms[term_ids]

  for (i in seq_along(term_ids)) {
    term_id <- term_ids[[i]]
    columns <- which(ctx$assign == term_id)
    basis <- .weighted_basis(ctx$centered_model[, columns, drop = FALSE], ctx$weights)
    term_fits[[i]] <- .design_subspace_svd_one(
      crossblock = ctx$crossblock,
      basis = basis$q,
      term = ctx$terms[[term_id]],
      rank = basis$rank,
      weights = ctx$weights,
      ncomp = ncomp,
      keep_crossblock = keep_crossblocks
    )
  }

  stats <- do.call(rbind, lapply(term_fits, function(x) {
    data.frame(
      term = x$term,
      rank = x$rank,
      ncomp = length(x$singular_values),
      trace = x$trace,
      largest_root = x$largest_root,
      statistic = if (identical(statistic, "trace")) x$trace else x$largest_root,
      statistic_type = statistic,
      stringsAsFactors = FALSE
    )
  }))

  components <- do.call(rbind, lapply(term_fits, function(x) x$components))
  rownames(stats) <- NULL
  rownames(components) <- NULL

  structure(
    list(
      terms = names(term_fits),
      term_fits = term_fits,
      statistics = stats,
      components = components,
      design = design,
      formula = ctx$formula,
      cell_table = ctx$cell_table,
      weights = ctx$weights,
      statistic_type = statistic,
      centering_operator = ctx$centering_operator
    ),
    class = "pls_design_subspace_svd"
  )
}

#' @export
print.pls_design_subspace_svd <- function(x, ...) {
  cat("Task PLS design-subspace SVD\n")
  cat("Terms:", paste(x$terms, collapse = ", "), "\n\n")
  print(x$statistics[, c("term", "rank", "trace", "largest_root")], row.names = FALSE)
  invisible(x)
}

#' Test Task PLS Design Subspaces
#'
#' @description
#' Computes observed covariance energy in term-specific design subspaces and,
#' when requested, compares those statistics to a global Task PLS permutation
#' null. The permutation null asks whether term-aligned covariance is larger
#' than expected under the package's ordinary global Task PLS row permutation.
#'
#' @param x A Task PLS `pls_result`, or a `pls_spec` when `fit` is supplied.
#' @param fit Optional fitted `pls_result` when `x` is a `pls_spec`.
#' @param spec Optional `pls_spec` used to recompute permuted cross-block
#'   matrices. Required for `nperm > 0` when `x` is a `pls_result`.
#' @param design Optional `pls_design` object.
#' @param formula Optional one-sided formula overriding `design$formula`.
#' @param condition_key Optional condition metadata.
#' @param terms `"all"` or a character vector of term labels to test.
#' @param statistic `"trace"` for total subspace covariance energy, or
#'   `"largest_root"` for the dominant singular-root statistic.
#' @param weights `"cell_equal"`, `"subject_count"`, or `"row_weights"` to
#'   use stored `result$task_pls$row_weights`.
#' @param nperm Number of permutations.
#' @param permutation `"none"` or `"global_task_pls"`.
#' @param correction `"none"` or `"maxT"` adjustment across tested terms.
#' @param permsamp Optional permutation order matrix.
#' @param progress Logical; show permutation progress messages.
#'
#' @return A data frame with term, rank, statistic, p-value, and null labels.
#' @export
test_design_subspaces <- function(x,
                                  fit = NULL,
                                  spec = NULL,
                                  design = NULL,
                                  formula = NULL,
                                  condition_key = NULL,
                                  terms = "all",
                                  statistic = c("trace", "largest_root"),
                                  weights = c("cell_equal", "subject_count", "row_weights"),
                                  nperm = 0L,
                                  permutation = c("none", "global_task_pls"),
                                  correction = c("none", "maxT"),
                                  permsamp = NULL,
                                  progress = FALSE) {
  statistic <- match.arg(statistic)
  weights <- match.arg(weights)
  permutation <- match.arg(permutation)
  correction <- match.arg(correction)
  nperm <- as.integer(nperm)
  if (length(nperm) != 1L || is.na(nperm) || nperm < 0L) {
    stop("nperm must be a non-negative integer.", call. = FALSE)
  }
  if (nperm > 0L && identical(permutation, "none")) {
    permutation <- "global_task_pls"
  }

  inputs <- .resolve_design_subspace_inputs(x, fit = fit, spec = spec)
  observed <- design_subspace_svd(
    inputs$result,
    design = design,
    formula = formula,
    condition_key = condition_key,
    terms = terms,
    statistic = statistic,
    weights = weights
  )

  out <- observed$statistics[, c("term", "rank", "statistic", "statistic_type"), drop = FALSE]
  out$p_value <- NA_real_
  out$p_adjusted <- NA_real_
  out$permutation <- permutation
  out$correction <- correction
  out$nperm <- nperm

  if (nperm > 0L) {
    if (is.null(inputs$spec)) {
      stop(
        "Permutation design-subspace inference requires a pls_spec. ",
        "Call test_design_subspaces(spec, fit = fit, ...) or provide spec = spec.",
        call. = FALSE
      )
    }
    perm_stats <- .design_subspace_permutation_stats(
      spec = inputs$spec,
      result = inputs$result,
      observed = observed,
      statistic = statistic,
      nperm = nperm,
      permsamp = permsamp,
      progress = progress
    )
    observed_stats <- out$statistic
    out$p_value <- colMeans(sweep(perm_stats, 2L, observed_stats, `>=`))
    if (identical(correction, "maxT")) {
      max_stat <- apply(perm_stats, 1L, max)
      out$p_adjusted <- vapply(observed_stats, function(x) mean(max_stat >= x), numeric(1))
    } else {
      out$p_adjusted <- out$p_value
    }
  }

  rownames(out) <- NULL
  out
}

#' Observed or Global-Null Design-Subspace Statistics for Task PLS Terms
#'
#' @description
#' Compatibility wrapper for [test_design_subspaces()]. New code should prefer
#' `test_design_subspaces()` to make clear that inference is over fitted
#' design subspaces, not post-hoc LV annotations.
#'
#' @inheritParams test_design_subspaces
#' @param result A Task PLS `pls_result`.
#' @param spec Optional `pls_spec` used to recompute permuted cross-block
#'   matrices. Required for `nperm > 0`.
#'
#' @return A data frame with term, rank, statistic, and p-value columns.
#' @export
test_design_terms <- function(result,
                              design = NULL,
                              formula = NULL,
                              condition_key = NULL,
                              terms = "all",
                              statistic = c("trace", "largest_root"),
                              weights = c("cell_equal", "subject_count", "row_weights"),
                              nperm = 0L,
                              permutation = c("none", "global_task_pls"),
                              correction = c("none", "maxT"),
                              spec = NULL,
                              permsamp = NULL,
                              progress = FALSE) {
  test_design_subspaces(
    result,
    spec = spec,
    design = design,
    formula = formula,
    condition_key = condition_key,
    terms = terms,
    statistic = statistic,
    weights = weights,
    nperm = nperm,
    permutation = permutation,
    correction = correction,
    permsamp = permsamp,
    progress = progress
  )
}

#' Compare Nested Task PLS Design Subspaces
#'
#' @description
#' Fits the delta design subspace from a full cell-level design after QR
#' residualization against a reduced design. With
#' `permutation = "global_task_pls"`, compares the delta statistic to the
#' package's ordinary global Task PLS permutation null. This global null is not
#' a reduced-model-preserving nested test.
#'
#' @param x A Task PLS `pls_result`, or a `pls_spec` when `fit` is supplied.
#' @param fit Optional fitted `pls_result` when `x` is a `pls_spec`.
#' @param spec Optional `pls_spec` used to recompute permuted cross-block
#'   matrices. Required for `nperm > 0` when `x` is a `pls_result`.
#' @param reduced One-sided reduced formula.
#' @param full One-sided full formula.
#' @param design Optional `pls_design` object.
#' @param condition_key Optional condition metadata.
#' @param statistic `"trace"` or `"largest_root"`.
#' @param weights `"cell_equal"`, `"subject_count"`, or `"row_weights"` to
#'   use stored `result$task_pls$row_weights`.
#' @param nperm Number of permutations.
#' @param permutation `"none"` or `"global_task_pls"`.
#' @param permsamp Optional permutation order matrix.
#' @param progress Logical; show permutation progress messages.
#'
#' @return A one-row data frame with observed nested-subspace statistic.
#' @export
compare_design_subspaces <- function(x,
                                     reduced,
                                     full,
                                     fit = NULL,
                                     spec = NULL,
                                     design = NULL,
                                     condition_key = NULL,
                                     statistic = c("trace", "largest_root"),
                                     weights = c("cell_equal", "subject_count", "row_weights"),
                                     nperm = 0L,
                                     permutation = c("none", "global_task_pls", "reduced_model"),
                                     permsamp = NULL,
                                     progress = FALSE) {
  statistic <- match.arg(statistic)
  weights <- match.arg(weights)
  permutation <- match.arg(permutation)
  nperm <- as.integer(nperm)
  if (length(nperm) != 1L || is.na(nperm) || nperm < 0L) {
    stop("nperm must be a non-negative integer.", call. = FALSE)
  }
  if (identical(permutation, "reduced_model")) {
    stop(
      "Reduced-model design-subspace permutation inference is not implemented yet. ",
      "Use permutation = \"global_task_pls\" for a global-null test, or nperm = 0 for observed statistics.",
      call. = FALSE
    )
  }
  if (nperm > 0L && identical(permutation, "none")) {
    permutation <- "global_task_pls"
  }

  inputs <- .resolve_design_subspace_inputs(x, fit = fit, spec = spec)
  reduced <- .pls_one_sided_formula(reduced)
  full <- .pls_one_sided_formula(full)
  ctx <- .design_subspace_context(
    result = inputs$result,
    design = design,
    formula = full,
    condition_key = condition_key,
    weights = weights,
    require_crossblock = TRUE
  )

  x_full <- .design_model_matrix(full, ctx$cell_table, contrasts = ctx$contrasts)
  x_reduced <- .design_model_matrix(reduced, ctx$cell_table, contrasts = ctx$contrasts)
  x_full_c <- ctx$centering_operator %*% x_full
  x_reduced_c <- ctx$centering_operator %*% x_reduced
  .assert_nested_design(full = x_full_c, reduced = x_reduced_c, weights = ctx$weights)
  basis <- .nested_weighted_basis(full = x_full_c, reduced = x_reduced_c, weights = ctx$weights)

  full_terms <- attr(stats::terms(full), "term.labels")
  reduced_terms <- attr(stats::terms(reduced), "term.labels")
  added_terms <- .added_terms(full_terms, reduced_terms)
  if (!length(added_terms)) {
    added_terms <- if (basis$rank == 0L) "none" else "residualized_full"
  }

  term_label <- paste(added_terms, collapse = " + ")
  fit_delta <- .design_subspace_svd_one(
    crossblock = ctx$crossblock,
    basis = basis$q,
    term = term_label,
    rank = basis$rank,
    weights = ctx$weights,
    ncomp = NULL,
    keep_crossblock = FALSE
  )
  observed_stat <- if (identical(statistic, "trace")) fit_delta$trace else fit_delta$largest_root

  p_value <- NA_real_
  if (nperm > 0L) {
    if (is.null(inputs$spec)) {
      stop(
        "Permutation design-subspace inference requires a pls_spec. ",
        "Call compare_design_subspaces(spec, fit = fit, ...) or provide spec = spec.",
        call. = FALSE
      )
    }
    perm_stats <- .design_subspace_permutation_stats(
      spec = inputs$spec,
      result = inputs$result,
      observed = list(term_fits = list(fit_delta)),
      statistic = statistic,
      nperm = nperm,
      permsamp = permsamp,
      progress = progress
    )
    p_value <- mean(perm_stats[, 1L] >= observed_stat)
  }

  data.frame(
    reduced = .formula_text(reduced),
    full = .formula_text(full),
    added_terms = term_label,
    rank = basis$rank,
    statistic = observed_stat,
    p_value = p_value,
    statistic_type = statistic,
    permutation = permutation,
    nperm = nperm,
    stringsAsFactors = FALSE
  )
}

#' Compare Nested Task PLS Design Subspaces
#'
#' @description
#' Compatibility wrapper for [compare_design_subspaces()].
#'
#' @inheritParams compare_design_subspaces
#' @param result A Task PLS `pls_result`.
#' @param spec Optional `pls_spec` used to recompute permuted cross-block
#'   matrices. Required for `nperm > 0`.
#'
#' @return A one-row data frame with observed nested-subspace statistic.
#' @export
compare_designs <- function(result,
                            reduced,
                            full,
                            design = NULL,
                            condition_key = NULL,
                            statistic = c("trace", "largest_root"),
                            weights = c("cell_equal", "subject_count", "row_weights"),
                            nperm = 0L,
                            permutation = c("none", "global_task_pls", "reduced_model"),
                            spec = NULL,
                            permsamp = NULL,
                            progress = FALSE) {
  compare_design_subspaces(
    result,
    reduced = reduced,
    full = full,
    spec = spec,
    design = design,
    condition_key = condition_key,
    statistic = statistic,
    weights = weights,
    nperm = nperm,
    permutation = permutation,
    permsamp = permsamp,
    progress = progress
  )
}

#' Decompose a Task PLS Design LV into Factorial Subspaces
#'
#' @description
#' Descriptively attributes a design-side latent variable to centered
#' factorial subspaces. This is not an inferential test.
#'
#' @param result A Task PLS `pls_result`.
#' @param lv Latent variable index.
#' @param design Optional `pls_design` object.
#' @param formula Optional one-sided formula overriding `design$formula`.
#' @param condition_key Optional condition metadata.
#' @param weights `"cell_equal"`, `"subject_count"`, or `"row_weights"` to
#'   use stored `result$task_pls$row_weights`.
#'
#' @details
#' Term fractions are independent projections. They sum to one only when the
#' centered term subspaces are orthogonal under the requested weights.
#'
#' @return A data frame with LV energy and fraction by factorial term.
#' @export
decompose_design_terms <- function(result,
                                   lv = 1L,
                                   design = NULL,
                                   formula = NULL,
                                   condition_key = NULL,
                                   weights = c("cell_equal", "subject_count", "row_weights")) {
  weights <- match.arg(weights)
  lv <- as.integer(lv)
  if (length(lv) != 1L || lv < 1L || lv > ncol(result$v)) {
    stop("lv must identify one design latent variable.", call. = FALSE)
  }

  ctx <- .design_subspace_context(
    result = result,
    design = design,
    formula = formula,
    condition_key = condition_key,
    weights = weights,
    require_crossblock = FALSE
  )

  y <- as.numeric(result$v[, lv])
  if (length(y) != nrow(ctx$cell_table)) {
    stop("The selected design LV does not match the Task PLS cell table.", call. = FALSE)
  }

  yw <- y * sqrt(ctx$weights)
  total_energy <- sum(yw^2)

  rows <- vector("list", length(ctx$terms))
  for (term_id in seq_along(ctx$terms)) {
    columns <- which(ctx$assign == term_id)
    basis <- .weighted_basis(ctx$centered_model[, columns, drop = FALSE], ctx$weights)
    coefficients <- if (basis$rank > 0L) as.numeric(crossprod(basis$q, yw)) else numeric(0L)
    energy <- sum(coefficients^2)
    rows[[term_id]] <- data.frame(
      lv = lv,
      term = ctx$terms[[term_id]],
      rank = basis$rank,
      signed_projection = if (basis$rank == 1L) coefficients[[1L]] else NA_real_,
      energy = energy,
      fraction = if (total_energy > 0) energy / total_energy else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

.select_design_terms <- function(terms, available_terms) {
  if (identical(terms, "all")) {
    return(seq_along(available_terms))
  }
  if (!is.character(terms) || !length(terms)) {
    stop("terms must be \"all\" or a character vector of design term labels.", call. = FALSE)
  }
  missing <- setdiff(terms, available_terms)
  if (length(missing)) {
    stop("Unknown design term(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  match(terms, available_terms)
}

.design_subspace_svd_one <- function(crossblock,
                                     basis,
                                     term,
                                     rank,
                                     weights,
                                     ncomp = NULL,
                                     keep_crossblock = FALSE) {
  if (!is.matrix(crossblock)) {
    crossblock <- as.matrix(crossblock)
  }
  if (any(!is.finite(crossblock))) {
    stop("Task PLS crossblock contains non-finite values.", call. = FALSE)
  }
  if (nrow(crossblock) != nrow(basis)) {
    stop("Subspace basis and Task PLS crossblock have incompatible row counts.", call. = FALSE)
  }
  if (length(weights) != nrow(crossblock)) {
    stop("Design weights must have one value per design cell.", call. = FALSE)
  }

  n_features <- ncol(crossblock)
  if (ncol(basis) == 0L || rank == 0L) {
    components <- data.frame(
      term = character(0),
      component = integer(0),
      singular_value = numeric(0),
      covariance = numeric(0),
      percent_term_covariance = numeric(0),
      rank = integer(0),
      stringsAsFactors = FALSE
    )
    return(list(
      term = term,
      rank = as.integer(rank),
      basis = basis,
      weights = weights,
      singular_values = numeric(0),
      all_singular_values = numeric(0),
      left_vectors = matrix(0, nrow = nrow(crossblock), ncol = 0L),
      right_vectors = matrix(0, nrow = n_features, ncol = 0L),
      trace = 0,
      largest_root = 0,
      components = components,
      projected_crossblock = if (isTRUE(keep_crossblock)) matrix(0, nrow = nrow(crossblock), ncol = n_features) else NULL
    ))
  }

  if (!is.null(ncomp)) {
    ncomp <- as.integer(ncomp)
    if (length(ncomp) != 1L || is.na(ncomp) || ncomp < 1L) {
      stop("ncomp must be a positive integer or NULL.", call. = FALSE)
    }
  }

  crossblock_w <- crossblock * sqrt(weights)
  coordinates <- crossprod(basis, crossblock_w)
  sv <- svd(coordinates)
  all_d <- sv$d
  trace <- sum(all_d^2)
  largest_root <- if (length(all_d)) all_d[[1L]]^2 else 0

  keep <- if (length(all_d)) seq_along(all_d) else integer(0)
  if (!is.null(ncomp)) {
    keep <- keep[seq_len(min(ncomp, length(keep)))]
  }

  d <- all_d[keep]
  u <- if (length(keep)) sv$u[, keep, drop = FALSE] else matrix(0, nrow = ncol(basis), ncol = 0L)
  v <- if (length(keep)) sv$v[, keep, drop = FALSE] else matrix(0, nrow = n_features, ncol = 0L)
  left <- basis %*% u

  components <- data.frame(
    term = rep(term, length(d)),
    component = seq_along(d),
    singular_value = d,
    covariance = d^2,
    percent_term_covariance = if (trace > 0) d^2 / trace else rep(NA_real_, length(d)),
    rank = rep(as.integer(rank), length(d)),
    stringsAsFactors = FALSE
  )

  list(
    term = term,
    rank = as.integer(rank),
    basis = basis,
    weights = weights,
    singular_values = d,
    all_singular_values = all_d,
    left_vectors = left,
    right_vectors = v,
    trace = trace,
    largest_root = largest_root,
    components = components,
    projected_crossblock = if (isTRUE(keep_crossblock)) basis %*% coordinates else NULL
  )
}

.resolve_design_subspace_inputs <- function(x, fit = NULL, spec = NULL) {
  if (inherits(x, "pls_result")) {
    if (!is.null(fit)) {
      stop("fit is only used when x is a pls_spec.", call. = FALSE)
    }
    .validate_task_pls_result(x)
    return(list(result = x, spec = spec))
  }

  if (inherits(x, "pls_spec")) {
    if (is.null(fit)) {
      stop("When x is a pls_spec, provide fit = a fitted pls_result.", call. = FALSE)
    }
    if (!inherits(fit, "pls_result")) {
      stop("fit must be a pls_result.", call. = FALSE)
    }
    .validate_task_pls_result(fit)
    return(list(result = fit, spec = x))
  }

  stop("x must be a pls_result or a pls_spec.", call. = FALSE)
}

.design_subspace_permutation_stats <- function(spec,
                                               result,
                                               observed,
                                               statistic,
                                               nperm,
                                               permsamp = NULL,
                                               progress = FALSE) {
  spec <- .materialize_pls_spec(spec, derive_seed_behavior = TRUE)
  .validate_spec(spec)
  if (!as.integer(spec$method) %in% c(1L, 2L)) {
    stop("Design-subspace permutations currently support Task PLS methods 1 and 2.", call. = FALSE)
  }
  if (as.integer(spec$method) != as.integer(result$method)) {
    stop("spec and fit use different PLS methods.", call. = FALSE)
  }

  stacked_datamat <- do.call(rbind, spec$datamat_lst)
  total_rows <- nrow(stacked_datamat)
  n_cells <- length(spec$num_subj_lst) * as.integer(spec$num_cond)
  if (nrow(.result_crossblock(result, require = TRUE)) != n_cells) {
    stop("spec and fit have incompatible Task PLS design dimensions.", call. = FALSE)
  }

  permsamp <- .normalize_design_permsamp(
    permsamp = permsamp %||% .stored_task_pls_permsamp(result, nperm, total_rows),
    spec = spec,
    nperm = nperm,
    total_rows = total_rows
  )

  out <- matrix(0, nrow = nperm, ncol = length(observed$term_fits))
  colnames(out) <- vapply(observed$term_fits, function(x) x$term, character(1))
  bscan <- spec$bscan %||% seq_len(spec$num_cond)

  for (p in seq_len(nperm)) {
    covcor <- pls_get_covcor(
      method = as.integer(spec$method),
      stacked_datamat = stacked_datamat,
      stacked_behavdata = spec$stacked_behavdata,
      num_groups = length(spec$num_subj_lst),
      num_subj_lst = spec$num_subj_lst,
      num_cond = as.integer(spec$num_cond),
      bscan = bscan,
      meancentering_type = as.integer(spec$meancentering_type %||% 0L),
      cormode = as.integer(spec$cormode %||% 0L),
      datamat_reorder = permsamp[, p]
    )

    for (j in seq_along(observed$term_fits)) {
      term_fit <- observed$term_fits[[j]]
      out[p, j] <- .design_subspace_stat(
        crossblock = covcor$datamatsvd,
        basis = term_fit$basis,
        weights = term_fit$weights,
        statistic = statistic
      )
    }

    if (isTRUE(progress) && (p == nperm || p %% 25L == 0L)) {
      cli::cli_inform("Computed design-subspace permutation {p}/{nperm}.")
    }
  }

  out
}

.stored_task_pls_permsamp <- function(result, nperm, total_rows) {
  permsamp <- result$perm_result$permsamp %||% NULL
  if (is.null(permsamp) || nrow(permsamp) != total_rows || ncol(permsamp) < nperm) {
    return(NULL)
  }
  permsamp[, seq_len(nperm), drop = FALSE]
}

.normalize_design_permsamp <- function(permsamp, spec, nperm, total_rows) {
  if (is.null(permsamp)) {
    permsamp <- pls_perm_order(
      num_subj_lst = spec$num_subj_lst,
      num_cond = as.integer(spec$num_cond),
      num_perm = nperm,
      not_in_cond = isTRUE(spec$is_struct)
    )
  }
  if (!is.matrix(permsamp) || !is.numeric(permsamp)) {
    stop("permsamp must be a numeric matrix.", call. = FALSE)
  }
  if (nrow(permsamp) != total_rows || ncol(permsamp) < nperm) {
    stop(
      "permsamp must have one row per stacked data row and at least nperm columns.",
      call. = FALSE
    )
  }
  permsamp <- permsamp[, seq_len(nperm), drop = FALSE]
  if (any(!is.finite(permsamp))) {
    stop("permsamp must contain finite row indices.", call. = FALSE)
  }
  storage.mode(permsamp) <- "integer"
  if (any(permsamp < 1L) || any(permsamp > total_rows)) {
    stop("permsamp contains row indices outside the stacked data matrix.", call. = FALSE)
  }
  permsamp
}

.design_subspace_context <- function(result,
                                     design,
                                     formula,
                                     condition_key,
                                     weights,
                                     require_crossblock) {
  .validate_task_pls_result(result)

  if (!is.null(design) && !inherits(design, "pls_design")) {
    stop("design must be a pls_design object.", call. = FALSE)
  }

  formula <- if (!is.null(formula)) {
    .pls_one_sided_formula(formula)
  } else if (!is.null(design)) {
    design$formula
  } else {
    stop("Provide either design or formula.", call. = FALSE)
  }

  cell_table <- design_cell_table(result, design = design, condition_key = condition_key)
  contrasts <- if (!is.null(design)) design$contrasts else "sum"
  model <- .design_model_matrix(formula, cell_table, contrasts = contrasts)
  terms <- attr(stats::terms(formula), "term.labels")
  if (!length(terms)) {
    stop("formula must contain at least one design term.", call. = FALSE)
  }

  centering_operator <- .result_centering_operator(result)
  if (nrow(centering_operator) != nrow(model)) {
    stop("Task PLS centering operator does not match the design model.", call. = FALSE)
  }

  crossblock <- .result_crossblock(result, require = require_crossblock)
  if (!is.null(crossblock) && nrow(crossblock) != nrow(model)) {
    stop("Stored Task PLS crossblock does not match the design cell table.", call. = FALSE)
  }

  list(
    cell_table = cell_table,
    formula = formula,
    terms = terms,
    assign = attr(model, "assign"),
    model = model,
    centered_model = centering_operator %*% model,
    centering_operator = centering_operator,
    crossblock = crossblock,
    weights = .resolve_design_weights(weights, cell_table, result),
    contrasts = contrasts
  )
}

.validate_task_pls_result <- function(result) {
  if (!inherits(result, "pls_result")) {
    stop("result must be a pls_result.", call. = FALSE)
  }
  if (!result$method %in% c(1L, 2L)) {
    stop("Design-subspace tools currently support Task PLS methods 1 and 2.", call. = FALSE)
  }
  invisible(TRUE)
}

.result_crossblock <- function(result, require = TRUE) {
  crossblock <- result$task_pls$crossblock %||% result$design_crossblock %||% NULL
  if (is.null(crossblock) && isTRUE(require)) {
    stop(
      "No Task PLS crossblock is stored in result. ",
      "Fit with pls_analysis(..., keep_crossblock = TRUE) or run(..., keep_crossblock = TRUE).",
      call. = FALSE
    )
  }
  crossblock
}

.result_centering_operator <- function(result) {
  op <- result$task_pls$centering_operator %||% NULL
  if (!is.null(op)) {
    return(op)
  }

  meancentering_type <- as.integer(result$other_input$meancentering_type %||% 0L)
  .task_pls_centering_operator(
    num_subj_lst = result$num_subj_lst,
    num_cond = result$num_cond,
    meancentering_type = meancentering_type,
    method = result$method
  )
}

.design_model_matrix <- function(formula, cell_table, contrasts = "sum") {
  formula <- .pls_one_sided_formula(formula)
  terms <- all.vars(formula)
  missing <- setdiff(terms, names(cell_table))
  if (length(missing)) {
    stop("Design formula references missing cell-table columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  data <- cell_table
  contrast_args <- .design_contrast_args(
    data[, intersect(all.vars(formula), names(data)), drop = FALSE],
    contrasts
  )
  stats::model.matrix(formula, data = data, contrasts.arg = contrast_args)
}

.design_contrast_args <- function(data, contrasts) {
  factor_cols <- names(data)[vapply(data, is.factor, logical(1))]
  if (!length(factor_cols)) {
    return(NULL)
  }

  if (!identical(contrasts, "sum")) {
    stop("Only contrasts = \"sum\" is currently supported.", call. = FALSE)
  }

  stats::setNames(rep(list(stats::contr.sum), length(factor_cols)), factor_cols)
}

.weighted_basis <- function(x, weights, tol = 1e-10) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!ncol(x)) {
    return(list(q = matrix(0, nrow = nrow(x), ncol = 0L), rank = 0L))
  }
  if (any(!is.finite(x))) {
    stop("Design matrix contains non-finite values.", call. = FALSE)
  }

  xw <- x * sqrt(weights)
  qr_x <- qr(xw, tol = tol)
  rank <- as.integer(qr_x$rank)
  if (rank == 0L) {
    return(list(q = matrix(0, nrow = nrow(x), ncol = 0L), rank = 0L))
  }

  list(q = qr.Q(qr_x)[, seq_len(rank), drop = FALSE], rank = rank)
}

.nested_weighted_basis <- function(full, reduced, weights, tol = 1e-10) {
  full_w <- full * sqrt(weights)
  reduced_w <- reduced * sqrt(weights)

  reduced_basis <- .weighted_basis(reduced, weights, tol = tol)
  residual <- full_w
  if (reduced_basis$rank > 0L) {
    residual <- full_w - reduced_basis$q %*% crossprod(reduced_basis$q, full_w)
  }

  if (max(abs(residual), na.rm = TRUE) <= tol) {
    return(list(q = matrix(0, nrow = nrow(full), ncol = 0L), rank = 0L))
  }

  qr_resid <- qr(residual, tol = tol)
  rank <- as.integer(qr_resid$rank)
  if (rank == 0L) {
    return(list(q = matrix(0, nrow = nrow(full), ncol = 0L), rank = 0L))
  }

  list(q = qr.Q(qr_resid)[, seq_len(rank), drop = FALSE], rank = rank)
}

.assert_nested_design <- function(full, reduced, weights, tol = 1e-8) {
  full_basis <- .weighted_basis(full, weights)
  reduced_w <- reduced * sqrt(weights)
  residual <- reduced_w
  if (full_basis$rank > 0L) {
    residual <- reduced_w - full_basis$q %*% crossprod(full_basis$q, reduced_w)
  }
  if (max(abs(residual), na.rm = TRUE) > tol) {
    stop("reduced design is not nested in full design after Task PLS centering.", call. = FALSE)
  }
  invisible(TRUE)
}

.added_terms <- function(full_terms, reduced_terms) {
  full_canonical <- .canonical_terms(full_terms)
  reduced_canonical <- .canonical_terms(reduced_terms)
  full_terms[!full_canonical %in% reduced_canonical]
}

.canonical_terms <- function(terms) {
  vapply(strsplit(terms, ":", fixed = TRUE), function(parts) {
    paste(sort(parts), collapse = ":")
  }, character(1))
}

.design_subspace_stat <- function(crossblock, basis, weights, statistic) {
  if (any(!is.finite(crossblock))) {
    stop("Task PLS crossblock contains non-finite values.", call. = FALSE)
  }
  if (ncol(basis) == 0L) {
    return(0)
  }

  crossblock_w <- crossblock * sqrt(weights)
  coordinates <- crossprod(basis, crossblock_w)

  if (identical(statistic, "trace")) {
    return(sum(coordinates^2))
  }

  d <- svd(coordinates, nu = 0L, nv = 0L)$d
  if (!length(d)) 0 else d[[1L]]^2
}

.resolve_design_weights <- function(weights, cell_table, result) {
  if (identical(weights, "cell_equal")) {
    return(rep(1, nrow(cell_table)))
  }

  out <- if (identical(weights, "subject_count")) {
    as.numeric(cell_table$cell_n)
  } else {
    result$task_pls$row_weights %||% NULL
  }

  if (is.null(out)) {
    stop("weights = \"row_weights\" requires result$task_pls$row_weights.", call. = FALSE)
  }
  out <- as.numeric(out)
  if (length(out) != nrow(cell_table)) {
    stop("Design weights must have one value per design cell.", call. = FALSE)
  }
  if (any(!is.finite(out)) || any(out <= 0)) {
    stop("Design weights must be positive finite values.", call. = FALSE)
  }
  out
}

.task_pls_cell_counts <- function(num_subj_lst, num_cond) {
  num_cond <- as.integer(num_cond)
  if (is.list(num_subj_lst)) {
    unlist(lapply(num_subj_lst, function(x) {
      x <- as.integer(x)
      if (length(x) != num_cond) {
        stop("Each num_subj_lst entry must have length num_cond.", call. = FALSE)
      }
      x
    }), use.names = FALSE)
  } else {
    rep(as.integer(num_subj_lst), each = num_cond)
  }
}

.task_pls_centering_operator <- function(num_subj_lst,
                                         num_cond,
                                         meancentering_type = 0L,
                                         method = 1L) {
  counts <- .task_pls_cell_counts(num_subj_lst, num_cond)
  num_cond <- as.integer(num_cond)
  num_groups <- length(num_subj_lst)
  n_cells <- num_groups * num_cond

  if (as.integer(method) == 2L) {
    return(diag(n_cells))
  }

  meancentering_type <- as.integer(meancentering_type)
  if (!meancentering_type %in% 0:3) {
    stop("meancentering_type must be 0, 1, 2, or 3.", call. = FALSE)
  }

  m <- diag(n_cells)

  if (meancentering_type == 0L) {
    for (g in seq_len(num_groups)) {
      idx <- ((g - 1L) * num_cond + 1L):(g * num_cond)
      w <- counts[idx] / sum(counts[idx])
      m[idx, idx] <- m[idx, idx] - outer(rep(1, length(idx)), w)
    }
  } else if (meancentering_type == 1L) {
    for (cond in seq_len(num_cond)) {
      idx <- seq(from = cond, by = num_cond, length.out = num_groups)
      m[idx, idx] <- m[idx, idx] - matrix(1 / num_groups, nrow = num_groups, ncol = num_groups)
    }
  } else if (meancentering_type == 2L) {
    w <- counts / sum(counts)
    m <- m - outer(rep(1, n_cells), w)
  } else if (meancentering_type == 3L) {
    for (row in seq_len(n_cells)) {
      group_id <- ((row - 1L) %/% num_cond) + 1L
      cond_id <- ((row - 1L) %% num_cond) + 1L
      group_idx <- ((group_id - 1L) * num_cond + 1L):(group_id * num_cond)
      cond_idx <- seq(from = cond_id, by = num_cond, length.out = num_groups)
      m[row, group_idx] <- m[row, group_idx] - 1 / num_cond
      m[row, cond_idx] <- m[row, cond_idx] - 1 / num_groups
      m[row, ] <- m[row, ] + 1 / n_cells
    }
  }

  m
}

.task_pls_cell_info <- function(num_subj_lst, num_cond) {
  num_cond <- as.integer(num_cond)
  num_groups <- length(num_subj_lst)
  data.frame(
    row_index = seq_len(num_groups * num_cond),
    group_index = rep(seq_len(num_groups), each = num_cond),
    condition_index = rep(seq_len(num_cond), times = num_groups),
    cell_n = .task_pls_cell_counts(num_subj_lst, num_cond),
    stringsAsFactors = FALSE
  )
}

.assert_task_pls_cell_info <- function(result, cell_table) {
  info <- result$task_pls$cell_info %||% NULL
  if (is.null(info)) {
    return(invisible(TRUE))
  }
  if (nrow(info) != nrow(cell_table)) {
    stop("Stored Task PLS cell_info does not match the design cell table.", call. = FALSE)
  }
  if ("row_index" %in% names(info) && !identical(as.integer(info$row_index), as.integer(cell_table$row_index))) {
    stop("Stored Task PLS cell_info row_index does not match the design cell table.", call. = FALSE)
  }
  if ("cell_n" %in% names(info) && !identical(as.integer(info$cell_n), as.integer(cell_table$cell_n))) {
    stop("Stored Task PLS cell_info cell_n does not match the design cell table.", call. = FALSE)
  }
  if (all(c("group_index", "condition_index") %in% names(info))) {
    expected_group <- as.integer(cell_table$group)
    expected_condition <- as.integer(cell_table$condition)
    if (!identical(as.integer(info$group_index), expected_group) ||
        !identical(as.integer(info$condition_index), expected_condition)) {
      stop("Stored Task PLS cell_info ordering does not match group-condition cell order.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.validate_condition_key <- function(condition_key, conditions = NULL) {
  if (is.null(condition_key)) {
    return(NULL)
  }
  if (!is.data.frame(condition_key)) {
    stop("condition_key must be a data frame.", call. = FALSE)
  }
  if (!"condition" %in% names(condition_key)) {
    stop("condition_key must contain a condition column.", call. = FALSE)
  }
  if (anyDuplicated(as.character(condition_key$condition))) {
    stop("condition_key contains duplicated condition labels.", call. = FALSE)
  }
  if (!is.null(conditions)) {
    missing <- setdiff(as.character(conditions), as.character(condition_key$condition))
    if (length(missing)) {
      stop("condition_key is missing condition labels: ", paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  condition_key
}

.factorize_design_columns <- function(x) {
  skip <- c("row_index", "cell_n")
  for (nm in setdiff(names(x), skip)) {
    if (is.factor(x[[nm]])) {
      next
    }
    x[[nm]] <- factor(x[[nm]], levels = unique(x[[nm]]))
  }
  x
}

.pls_one_sided_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("formula must be a formula.", call. = FALSE)
  }
  if (length(formula) == 3L) {
    formula <- stats::as.formula(paste("~", paste(deparse(formula[[3L]]), collapse = " ")), env = environment(formula))
  }
  if (length(formula) != 2L) {
    stop("formula must be one-sided.", call. = FALSE)
  }
  formula
}

.formula_text <- function(formula) {
  paste(deparse(formula), collapse = " ")
}
