#' ASCA-PLS Partial Test Specification
#'
#' @description
#' Defines how formula terms are tested in `asca_pls()`. The current
#' implementation supports observed partial statistics and raw-observation
#' Freedman-Lane residual permutation when the original `pls_spec` is available.
#' For repeated-measures terms, residuals are permuted within derived subject
#' blocks so higher-order interaction tests are not calibrated from the
#' anti-conservative cell-level residual fallback.
#'
#' @param method Testing method. Currently `"freedman_lane"` or `"none"`.
#' @param statistic `"trace"` for omnibus subspace energy or `"largest_root"`
#'   for the dominant root.
#' @param nperm Number of permutations.
#' @param correction `"none"` or `"maxT"` across tested terms.
#' @param exchangeability Optional exchangeability metadata.
#' @param keep_null Logical; retain the permutation null matrix.
#'
#' @return A partial-test specification.
#' @export
partial_test <- function(method = c("freedman_lane", "none"),
                         statistic = c("trace", "largest_root"),
                         nperm = 0L,
                         correction = c("none", "maxT"),
                         exchangeability = NULL,
                         keep_null = FALSE) {
  method <- match.arg(method)
  statistic <- match.arg(statistic)
  correction <- match.arg(correction)
  nperm <- as.integer(nperm)
  if (length(nperm) != 1L || is.na(nperm) || nperm < 0L) {
    stop("nperm must be a non-negative integer.", call. = FALSE)
  }
  structure(
    list(
      method = method,
      statistic = statistic,
      nperm = nperm,
      correction = correction,
      exchangeability = exchangeability,
      keep_null = isTRUE(keep_null)
    ),
    class = "asca_partial_test"
  )
}

#' ASCA-PLS Hierarchical Selection Specification
#'
#' @param method Selection method. Currently `"none"` or `"backward"`.
#' @param heredity Heredity rule. Currently `"strong"`.
#' @param alpha Selection threshold.
#' @param adjust Column used for selection, `"p_adjusted"` or `"p_value"`.
#'
#' @return A selection specification.
#' @export
hierarchical_selection <- function(method = c("none", "backward"),
                                   heredity = c("strong"),
                                   alpha = 0.05,
                                   adjust = c("p_adjusted", "p_value")) {
  method <- match.arg(method)
  heredity <- match.arg(heredity)
  adjust <- match.arg(adjust)
  alpha <- as.numeric(alpha)
  if (length(alpha) != 1L || is.na(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha must be a number in [0, 1].", call. = FALSE)
  }
  structure(
    list(method = method, heredity = heredity, alpha = alpha, adjust = adjust),
    class = "asca_hierarchical_selection"
  )
}

#' Exchangeability Metadata for ASCA-PLS
#'
#' @param unit Observation-unit identifier, usually a subject column.
#' @param within Within-unit repeated-measures factors.
#' @param between Between-unit factors.
#' @param strata Optional permutation strata.
#'
#' @return An exchangeability metadata object.
#' @export
exchangeability <- function(unit = NULL,
                            within = NULL,
                            between = NULL,
                            strata = NULL) {
  structure(
    list(
      unit = .asca_role_names(substitute(unit), unit),
      within = .asca_role_names(substitute(within), within),
      between = .asca_role_names(substitute(between), between),
      strata = .asca_role_names(substitute(strata), strata)
    ),
    class = "asca_exchangeability"
  )
}

#' Formula-Aware ASCA-PLS for Task PLS
#'
#' @description
#' Adds an ASCA-like formula layer to Task PLS. Each formula term carries an
#' estimand, a partial test space, display views, and an explicit bridge from
#' tested partial components into named factorial coordinates.
#'
#' @param x A `pls_spec` or a Task PLS `pls_result`.
#' @param decompose Formula for interpretable factorial terms.
#' @param fit Optional fitted result when `x` is a `pls_spec`.
#' @param condition_key Optional condition metadata with a `condition` column.
#' @param adjust Optional nuisance formula. Stored and used in diagnostics.
#' @param id Subject/unit identifier column name.
#' @param within Within-unit factors.
#' @param between Between-unit factors.
#' @param strata Optional strata variables.
#' @param contrasts Contrast coding. Currently `"sum"`.
#' @param estimand Estimand weighting label.
#' @param test Partial test specification, a string, or `NULL`.
#' @param selection Selection specification, a string, or `NULL`.
#' @param nperm Convenience number of permutations when `test` is a string.
#' @param progress Logical; show progress from fitting if a spec is supplied.
#' @param ... Additional arguments passed to [run()] when fitting a spec.
#'
#' @return An `asca_pls_result`.
#' @export
asca_pls <- function(x,
                     decompose,
                     fit = NULL,
                     condition_key = NULL,
                     adjust = NULL,
                     id = NULL,
                     within = NULL,
                     between = NULL,
                     strata = NULL,
                     contrasts = "sum",
                     estimand = c("equal_cell", "subject_count", "row_weights"),
                     test = partial_test(),
                     selection = hierarchical_selection(),
                     nperm = NULL,
                     progress = FALSE,
                     ...) {
  estimand <- match.arg(estimand)
  decompose <- .pls_one_sided_formula(decompose)
  adjust <- if (is.null(adjust)) NULL else .pls_one_sided_formula(adjust)
  roles <- .asca_design_roles(
    id = .asca_role_names(substitute(id), id),
    within = .asca_role_names(substitute(within), within),
    between = .asca_role_names(substitute(between), between),
    strata = .asca_role_names(substitute(strata), strata)
  )
  test <- .asca_normalize_test(test, nperm = nperm)
  if (is.null(test$exchangeability)) {
    test$exchangeability <- structure(roles, class = "asca_exchangeability")
  }
  selection <- .asca_normalize_selection(selection)
  inputs <- .asca_resolve_inputs(x, fit = fit, progress = progress, ...)

  design <- pls_design(
    formula = decompose,
    condition_key = condition_key,
    subject = roles$id %||% NULL,
    between = roles$between,
    within = roles$within,
    contrasts = contrasts
  )
  weights <- switch(
    estimand,
    equal_cell = "cell_equal",
    subject_count = "subject_count",
    row_weights = "row_weights"
  )
  ctx <- .design_subspace_context(
    result = inputs$result,
    design = design,
    formula = decompose,
    condition_key = condition_key,
    weights = weights,
    require_crossblock = TRUE
  )
  diagnostics <- .asca_formula_diagnostics(ctx, roles = roles, adjust = adjust, estimand = estimand)
  effect_fit <- design_subspace_svd(
    inputs$result,
    design = design,
    statistic = test$statistic,
    weights = weights,
    keep_crossblocks = FALSE
  )
  partial <- .asca_partial_terms(ctx, statistic = test$statistic)
  partial <- .asca_partial_pvalues(
    partial = partial,
    ctx = ctx,
    spec = inputs$spec,
    test = test
  )
  selection_table <- .asca_select_terms(partial$anova, selection)
  selection_table$rank_effect <- diagnostics$rank_effect[match(selection_table$term, diagnostics$term)]
  selected_formula <- .asca_formula_from_terms(selection_table$term[selection_table$status %in% c("kept", "protected")])
  terms <- .asca_build_term_results(
    ctx = ctx,
    diagnostics = diagnostics,
    effect_fit = effect_fit,
    partial = partial,
    selection_table = selection_table,
    test = test
  )

  structure(
    list(
      result = inputs$result,
      spec = inputs$spec,
      design = design,
      decompose = decompose,
      adjust = adjust,
      roles = roles,
      estimand = estimand,
      diagnostics = diagnostics,
      anova = selection_table,
      selected_formula = selected_formula,
      terms = terms,
      test = test,
      selection = selection,
      call = match.call()
    ),
    class = "asca_pls_result"
  )
}

#' Diagnose an ASCA-PLS Formula Against a Fitted Task PLS Result
#'
#' @inheritParams asca_pls
#' @param result A fitted Task PLS result.
#'
#' @return A data frame with term ranks and estimability diagnostics.
#' @export
diagnose_asca_formula <- function(result,
                                  decompose,
                                  condition_key = NULL,
                                  id = NULL,
                                  within = NULL,
                                  between = NULL,
                                  strata = NULL,
                                  contrasts = "sum",
                                  estimand = c("equal_cell", "subject_count", "row_weights"),
                                  adjust = NULL) {
  estimand <- match.arg(estimand)
  decompose <- .pls_one_sided_formula(decompose)
  adjust <- if (is.null(adjust)) NULL else .pls_one_sided_formula(adjust)
  roles <- .asca_design_roles(
    id = .asca_role_names(substitute(id), id),
    within = .asca_role_names(substitute(within), within),
    between = .asca_role_names(substitute(between), between),
    strata = .asca_role_names(substitute(strata), strata)
  )
  design <- pls_design(
    formula = decompose,
    condition_key = condition_key,
    subject = roles$id %||% NULL,
    between = roles$between,
    within = roles$within,
    contrasts = contrasts
  )
  weights <- switch(
    estimand,
    equal_cell = "cell_equal",
    subject_count = "subject_count",
    row_weights = "row_weights"
  )
  ctx <- .design_subspace_context(
    result = result,
    design = design,
    formula = decompose,
    condition_key = condition_key,
    weights = weights,
    require_crossblock = TRUE
  )
  .asca_formula_diagnostics(ctx, roles = roles, adjust = adjust, estimand = estimand)
}

#' @export
anova.asca_pls_result <- function(object, ...) {
  object$anova
}

#' @export
terms.asca_pls_result <- function(x, ...) {
  x$diagnostics
}

#' @export
print.asca_pls_result <- function(x, ...) {
  cat("ASCA-PLS formula result\n")
  cat("Formula:", .formula_text(x$decompose), "\n")
  cat("Selected:", .formula_text(x$selected_formula), "\n\n")
  print(x$anova[, c("term", "order", "rank_effect", "rank_partial", "statistic", "p_value", "p_adjusted", "status")],
        row.names = FALSE)
  invisible(x)
}

#' Extract the Selected ASCA-PLS Formula
#'
#' @param x An `asca_pls_result`.
#' @param ... Unused.
#'
#' @return A one-sided formula.
#' @export
selected_formula <- function(x, ...) {
  UseMethod("selected_formula")
}

#' @export
selected_formula.asca_pls_result <- function(x, ...) {
  x$selected_formula
}

#' Extract ASCA-PLS Term Components
#'
#' @param x An `asca_pls_result`.
#' @param term Term label.
#' @param view `"tested_effect"`, `"effect"`, or `"partial"`.
#' @param component Component index.
#' @param ... Unused.
#'
#' @return A list describing the requested component.
#' @export
asca_components <- function(x,
                            term,
                            view = c("tested_effect", "effect", "partial"),
                            component = 1L,
                            ...) {
  if (!inherits(x, "asca_pls_result")) {
    stop("x must be an asca_pls_result.", call. = FALSE)
  }
  view <- match.arg(view)
  component <- as.integer(component)
  if (length(component) != 1L || is.na(component) || component < 1L) {
    stop("component must be a positive integer.", call. = FALSE)
  }
  item <- x$terms[[term]]
  if (is.null(item)) {
    stop("Unknown ASCA-PLS term: ", term, call. = FALSE)
  }
  view_obj <- item$display[[view]]
  if (is.null(view_obj)) {
    stop("View is not available for term: ", term, call. = FALSE)
  }
  if (component > length(view_obj$singular_values)) {
    stop("component exceeds the available components for term: ", term, call. = FALSE)
  }
  list(
    term = term,
    view = view,
    component = component,
    singular_value = view_obj$singular_values[[component]],
    design_profile = if (!is.null(view_obj$design_profiles)) view_obj$design_profiles[, component, drop = FALSE] else NULL,
    brain_salience = if (!is.null(view_obj$brain_saliences)) view_obj$brain_saliences[, component, drop = FALSE] else NULL,
    cell_table = item$estimand$cell_grid,
    test = item$test,
    bridge = item$bridge
  )
}

#' Extract ASCA-PLS Brain Saliences
#'
#' @inheritParams asca_components
#'
#' @return A matrix of brain saliences.
#' @export
brain_saliences <- function(x, ...) {
  UseMethod("brain_saliences")
}

#' @export
brain_saliences.asca_pls_result <- function(x,
                                            term,
                                            view = c("tested_effect", "effect", "partial"),
                                            component = NULL,
                                            ...) {
  view <- match.arg(view)
  item <- .asca_get_term_view(x, term = term, view = view)
  out <- item$brain_saliences
  .asca_select_component_columns(out, component)
}

#' Extract ASCA-PLS Design Saliences
#'
#' @inheritParams asca_components
#'
#' @return A matrix of design profiles in cell coordinates.
#' @export
design_saliences <- function(x, ...) {
  UseMethod("design_saliences")
}

#' @export
design_saliences.asca_pls_result <- function(x,
                                             term,
                                             view = c("tested_effect", "effect", "partial"),
                                             component = NULL,
                                             ...) {
  view <- match.arg(view)
  item <- .asca_get_term_view(x, term = term, view = view)
  out <- item$design_profiles
  .asca_select_component_columns(out, component)
}

#' Extract ASCA-PLS Term Table
#'
#' @param x An `asca_pls_result`.
#' @param ... Unused.
#'
#' @return A data frame with term diagnostics and test status.
#' @export
asca_term_table <- function(x, ...) {
  if (!inherits(x, "asca_pls_result")) {
    stop("x must be an asca_pls_result.", call. = FALSE)
  }
  merge(x$diagnostics, x$anova, by = "term", all.x = TRUE, sort = FALSE)
}

#' Plot ASCA-PLS Term Effect Profiles
#'
#' @param x An `asca_pls_result`.
#' @param term Term label.
#' @param view `"tested_effect"`, `"effect"`, or `"partial"`.
#' @param component Component index.
#' @param x_axis Cell-table column for the x axis.
#' @param line Optional cell-table column for line grouping.
#' @param facet Optional cell-table column for facets.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot_asca_effect_profile <- function(x,
                                     term,
                                     view = c("tested_effect", "effect", "partial"),
                                     component = 1L,
                                     x_axis = "condition",
                                     line = NULL,
                                     facet = NULL,
                                     ...) {
  view <- match.arg(view)
  comp <- asca_components(x, term = term, view = view, component = component)
  cells <- comp$cell_table
  if (!x_axis %in% names(cells)) {
    stop("x_axis is not a cell-table column.", call. = FALSE)
  }
  line <- if (is.null(line)) NULL else as.character(line)
  facet <- if (is.null(facet)) NULL else as.character(facet)
  missing <- setdiff(c(line, facet), names(cells))
  if (length(missing)) {
    stop("Unknown cell-table column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  df <- cells
  df$.value <- as.numeric(comp$design_profile[, 1L])
  df$.line <- if (is.null(line)) "profile" else as.character(df[[line]])

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_axis]], y = .data$.value, group = .data$.line, color = .data$.line)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, color = "grey70") +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      x = x_axis,
      y = "Design profile",
      color = line %||% NULL,
      title = paste0(term, " ASCA-PLS ", view, " component ", component)
    ) +
    ggplot2::theme_minimal(base_size = 11)
  if (!is.null(facet)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet)))
  }
  if (is.null(line)) {
    p <- p + ggplot2::guides(color = "none")
  }
  p
}

#' Plot ASCA-PLS Partial-to-Effect Alignment
#'
#' @param x An `asca_pls_result`.
#' @param term Optional term label. When `NULL`, plot all terms.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot_asca_alignment <- function(x, term = NULL, ...) {
  if (!inherits(x, "asca_pls_result")) {
    stop("x must be an asca_pls_result.", call. = FALSE)
  }
  terms <- term %||% names(x$terms)
  rows <- do.call(rbind, lapply(terms, function(term_name) {
    item <- x$terms[[term_name]]
    if (is.null(item)) {
      stop("Unknown ASCA-PLS term: ", term_name, call. = FALSE)
    }
    alignment <- item$bridge$alignment
    if (!length(alignment)) {
      alignment <- NA_real_
    }
    data.frame(
      term = term_name,
      component = seq_along(alignment),
      alignment = alignment,
      stringsAsFactors = FALSE
    )
  }))
  ggplot2::ggplot(rows, ggplot2::aes(x = .data$term, y = .data$alignment)) +
    ggplot2::geom_col(fill = "#3B6EA8") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = NULL, y = "Partial-to-effect alignment") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

#' Plot ASCA-PLS Model Selection Table
#'
#' @param x An `asca_pls_result`.
#' @param p_column `"p_adjusted"` or `"p_value"`.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot_asca_selection <- function(x, p_column = c("p_adjusted", "p_value"), ...) {
  if (!inherits(x, "asca_pls_result")) {
    stop("x must be an asca_pls_result.", call. = FALSE)
  }
  p_column <- match.arg(p_column)
  tab <- x$anova
  tab$.p <- tab[[p_column]]
  ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(.data$term, .data$order), y = .data$.p, fill = .data$status)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 1), na.value = NA_real_) +
    ggplot2::labs(x = NULL, y = p_column, fill = "Status") +
    ggplot2::theme_minimal(base_size = 11)
}

#' @export
plot.asca_pls_result <- function(x,
                                 term = NULL,
                                 view = c("tested_effect", "effect", "partial"),
                                 component = 1L,
                                 ...) {
  if (is.null(term)) {
    return(plot_asca_selection(x, ...))
  }
  plot_asca_effect_profile(x, term = term, view = view, component = component, ...)
}

.asca_resolve_inputs <- function(x, fit = NULL, progress = FALSE, ...) {
  if (inherits(x, "pls_spec")) {
    if (is.null(fit)) {
      fit <- run(x, progress = progress, keep_crossblock = TRUE, ...)
    }
    .validate_task_pls_result(fit)
    if (is.null(.result_crossblock(fit, require = FALSE))) {
      stop("asca_pls() requires a fit produced with keep_crossblock = TRUE.", call. = FALSE)
    }
    return(list(spec = x, result = fit))
  }
  if (inherits(x, "pls_result")) {
    if (!is.null(fit)) {
      stop("fit is only used when x is a pls_spec.", call. = FALSE)
    }
    .validate_task_pls_result(x)
    if (is.null(.result_crossblock(x, require = FALSE))) {
      stop("asca_pls() requires a result produced with keep_crossblock = TRUE.", call. = FALSE)
    }
    return(list(spec = NULL, result = x))
  }
  stop("x must be a pls_spec or pls_result.", call. = FALSE)
}

.asca_get_term_view <- function(x, term, view) {
  if (!inherits(x, "asca_pls_result")) {
    stop("x must be an asca_pls_result.", call. = FALSE)
  }
  item <- x$terms[[term]]
  if (is.null(item)) {
    stop("Unknown ASCA-PLS term: ", term, call. = FALSE)
  }
  view_obj <- item$display[[view]]
  if (is.null(view_obj)) {
    stop("View is not available for term: ", term, call. = FALSE)
  }
  view_obj
}

.asca_select_component_columns <- function(x, component) {
  if (is.null(component)) {
    return(x)
  }
  component <- as.integer(component)
  if (any(is.na(component)) || any(component < 1L) || any(component > ncol(x))) {
    stop("component must identify available component columns.", call. = FALSE)
  }
  x[, component, drop = FALSE]
}

.asca_normalize_test <- function(test, nperm = NULL) {
  if (is.null(test)) {
    test <- partial_test(method = "none")
  } else if (is.character(test)) {
    test <- partial_test(method = test[[1L]], nperm = nperm %||% 0L)
  } else if (!inherits(test, "asca_partial_test")) {
    stop("test must be NULL, a string, or a partial_test() object.", call. = FALSE)
  }
  if (!is.null(nperm)) {
    test$nperm <- as.integer(nperm)
  }
  test
}

.asca_normalize_selection <- function(selection) {
  if (is.null(selection)) {
    return(hierarchical_selection(method = "none"))
  }
  if (is.character(selection)) {
    return(hierarchical_selection(method = selection[[1L]]))
  }
  if (!inherits(selection, "asca_hierarchical_selection")) {
    stop("selection must be NULL, a string, or a hierarchical_selection() object.", call. = FALSE)
  }
  selection
}

.asca_role_names <- function(expr, value) {
  if (identical(expr, quote(NULL))) {
    return(character(0))
  }
  if (is.name(expr)) {
    return(as.character(expr))
  }
  if (is.call(expr) && identical(expr[[1L]], as.name("c"))) {
    return(vapply(as.list(expr[-1L]), as.character, character(1)))
  }
  value <- tryCatch(value, error = function(e) NULL)
  if (is.null(value)) {
    return(character(0))
  }
  if (is.character(value)) {
    return(value)
  }
  as.character(value)
}

.asca_design_roles <- function(id, within, between, strata) {
  structure(
    list(
      id = id,
      within = within,
      between = between,
      strata = strata
    ),
    class = "asca_design_roles"
  )
}

.asca_formula_diagnostics <- function(ctx, roles, adjust, estimand) {
  rows <- vector("list", length(ctx$terms))
  for (i in seq_along(ctx$terms)) {
    term <- ctx$terms[[i]]
    columns <- which(ctx$assign == i)
    effect_basis <- .weighted_basis(ctx$centered_model[, columns, drop = FALSE], ctx$weights)
    reduced <- .asca_reduced_formula(ctx$formula, term)
    partial_basis <- .asca_partial_basis(ctx, term = term, reduced = reduced)
    nominal_df <- length(columns)
    rows[[i]] <- data.frame(
      term = term,
      order = .asca_term_order(term),
      nominal_df = nominal_df,
      rank_effect = effect_basis$rank,
      rank_partial = partial_basis$rank,
      estimable = effect_basis$rank > 0L && partial_basis$rank > 0L,
      partially_aliased = partial_basis$rank > 0L && partial_basis$rank < nominal_df,
      aliased = effect_basis$rank == 0L || partial_basis$rank == 0L,
      reduced = .formula_text(reduced),
      full = .formula_text(.asca_full_formula_for_term(ctx$formula, term)),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "roles") <- roles
  attr(out, "adjust") <- if (is.null(adjust)) NULL else .formula_text(adjust)
  attr(out, "estimand") <- estimand
  out
}

.asca_partial_terms <- function(ctx, statistic) {
  term_fits <- vector("list", length(ctx$terms))
  names(term_fits) <- ctx$terms
  rows <- vector("list", length(ctx$terms))
  for (i in seq_along(ctx$terms)) {
    term <- ctx$terms[[i]]
    reduced <- .asca_reduced_formula(ctx$formula, term)
    basis <- .asca_partial_basis(ctx, term = term, reduced = reduced)
    term_fit <- .design_subspace_svd_one(
      crossblock = ctx$crossblock,
      basis = basis$q,
      term = term,
      rank = basis$rank,
      weights = ctx$weights,
      ncomp = NULL,
      keep_crossblock = FALSE
    )
    stat <- if (identical(statistic, "trace")) term_fit$trace else term_fit$largest_root
    term_fit$reduced <- reduced
    term_fit$term_matrix <- basis$term_matrix
    term_fit$term_matrix_weighted <- basis$term_matrix_weighted
    term_fit$statistic <- stat
    term_fits[[i]] <- term_fit
    rows[[i]] <- data.frame(
      term = term,
      rank_partial = basis$rank,
      statistic = stat,
      statistic_type = statistic,
      p_value = NA_real_,
      p_adjusted = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  anova <- do.call(rbind, rows)
  rownames(anova) <- NULL
  list(term_fits = term_fits, anova = anova, null = NULL)
}

.asca_partial_pvalues <- function(partial, ctx, spec, test) {
  if (identical(test$method, "none") || test$nperm == 0L) {
    partial$anova$p_value <- NA_real_
    partial$anova$p_adjusted <- NA_real_
    return(partial)
  }
  if (!identical(test$method, "freedman_lane")) {
    stop("Unsupported ASCA-PLS test method: ", test$method, call. = FALSE)
  }
  if (is.null(spec)) {
    stop(
      "Reduced-model ASCA-PLS permutation inference requires the original pls_spec. ",
      "Call asca_pls(spec, fit = fit, ...) rather than asca_pls(fit, ...) when nperm > 0.",
      call. = FALSE
    )
  }
  null <- .asca_freedman_lane_raw_null(partial$term_fits, ctx = ctx, spec = spec, test = test)
  observed <- partial$anova$statistic
  partial$anova$p_value <- vapply(seq_along(observed), function(j) {
    (sum(null[, j] >= observed[[j]]) + 1) / (nrow(null) + 1)
  }, numeric(1))
  if (identical(test$correction, "maxT")) {
    max_stat <- apply(null, 1L, max)
    partial$anova$p_adjusted <- vapply(observed, function(x) {
      (sum(max_stat >= x) + 1) / (length(max_stat) + 1)
    }, numeric(1))
  } else {
    partial$anova$p_adjusted <- partial$anova$p_value
  }
  partial$null <- if (isTRUE(test$keep_null)) null else NULL
  partial
}

.asca_freedman_lane_raw_null <- function(term_fits, ctx, spec, test) {
  spec <- .materialize_pls_spec(spec, derive_seed_behavior = TRUE)
  .validate_spec(spec)
  if (!as.integer(spec$method) %in% c(1L, 2L)) {
    stop("Raw ASCA-PLS Freedman-Lane permutations currently support Task PLS methods 1 and 2.", call. = FALSE)
  }
  y <- do.call(rbind, spec$datamat_lst)
  obs <- .asca_observation_table(spec, ctx)
  if (nrow(obs) != nrow(y)) {
    stop("ASCA-PLS observation metadata does not match the stacked datamat.", call. = FALSE)
  }

  nperm <- as.integer(test$nperm)
  out <- matrix(0, nrow = nperm, ncol = length(term_fits))
  colnames(out) <- names(term_fits)
  bscan <- spec$bscan %||% seq_len(spec$num_cond)

  for (j in seq_along(term_fits)) {
    term_fit <- term_fits[[j]]
    reduced <- .asca_raw_reduced_formula(term_fit$reduced, term = names(term_fits)[[j]], exchangeability = test$exchangeability)
    x_reduced <- .asca_raw_model_matrix(reduced, obs, contrasts = ctx$contrasts)
    fitted <- .asca_lm_fitted(x_reduced, y)
    residual <- y - fitted
    perms <- .asca_raw_permutation_orders(obs, term = names(term_fits)[[j]], nperm = nperm, exchangeability = test$exchangeability)

    for (p in seq_len(nperm)) {
      y_perm <- fitted + residual[perms[, p], , drop = FALSE]
      covcor <- pls_get_covcor(
        method = as.integer(spec$method),
        stacked_datamat = y_perm,
        stacked_behavdata = spec$stacked_behavdata,
        num_groups = length(spec$num_subj_lst),
        num_subj_lst = spec$num_subj_lst,
        num_cond = as.integer(spec$num_cond),
        bscan = bscan,
        meancentering_type = as.integer(spec$meancentering_type %||% 0L),
        cormode = as.integer(spec$cormode %||% 0L)
      )
      out[p, j] <- .design_subspace_stat(
        crossblock = covcor$datamatsvd,
        basis = term_fit$basis,
        weights = term_fit$weights,
        statistic = test$statistic
      )
    }
  }

  out
}

.asca_observation_table <- function(spec, ctx) {
  groups <- as.character(spec$groups %||% paste0("Group", seq_along(spec$num_subj_lst)))
  conditions <- as.character(spec$conditions %||% paste0("Cond", seq_len(spec$num_cond)))
  key <- unique(ctx$cell_table[, setdiff(names(ctx$cell_table), c("row_index", "group", "cell_n")), drop = FALSE])
  rows <- vector("list", sum(as.integer(spec$num_subj_lst)) * as.integer(spec$num_cond))
  row_id <- 1L
  for (g in seq_along(spec$num_subj_lst)) {
    for (s in seq_len(as.integer(spec$num_subj_lst[[g]]))) {
      for (cnd in seq_len(as.integer(spec$num_cond))) {
        condition <- conditions[[cnd]]
        condition_row <- key[as.character(key$condition) == condition, , drop = FALSE]
        rows[[row_id]] <- data.frame(
          row_index = row_id,
          group = groups[[g]],
          subject = paste0(groups[[g]], "_", s),
          condition = condition,
          stringsAsFactors = FALSE
        )
        rows[[row_id]] <- cbind(rows[[row_id]], condition_row[, setdiff(names(condition_row), "condition"), drop = FALSE])
        row_id <- row_id + 1L
      }
    }
  }
  out <- do.call(rbind, rows)
  out$group <- factor(out$group, levels = groups)
  out$subject <- factor(out$subject, levels = unique(out$subject))
  out$condition <- factor(out$condition, levels = conditions)
  .factorize_design_columns(out)
}

.asca_raw_reduced_formula <- function(reduced, term, exchangeability) {
  within <- exchangeability$within %||% character(0)
  include_subject <- length(intersect(strsplit(term, ":", fixed = TRUE)[[1L]], within)) > 0L
  reduced_terms <- attr(stats::terms(reduced), "term.labels")
  if (isTRUE(include_subject)) {
    reduced_terms <- c("subject", reduced_terms)
  }
  .asca_formula_from_terms(reduced_terms)
}

.asca_raw_model_matrix <- function(formula, obs, contrasts = "sum") {
  vars <- all.vars(formula)
  data <- obs
  factor_cols <- vars[vapply(data[intersect(vars, names(data))], is.factor, logical(1))]
  contrast_args <- if (length(factor_cols)) {
    stats::setNames(rep(list(stats::contr.sum), length(factor_cols)), factor_cols)
  } else {
    NULL
  }
  stats::model.matrix(formula, data = data, contrasts.arg = contrast_args)
}

.asca_lm_fitted <- function(x, y) {
  fit <- stats::lm.fit(x = x, y = y)
  fitted <- fit$fitted.values
  if (is.null(dim(fitted))) {
    fitted <- matrix(fitted, ncol = 1L)
  }
  fitted
}

.asca_raw_permutation_orders <- function(obs, term, nperm, exchangeability) {
  n <- nrow(obs)
  out <- matrix(0L, nrow = n, ncol = nperm)
  within <- exchangeability$within %||% character(0)
  strata <- intersect(exchangeability$strata %||% character(0), names(obs))
  term_parts <- strsplit(term, ":", fixed = TRUE)[[1L]]
  has_within <- length(intersect(term_parts, within)) > 0L

  if (isTRUE(has_within) && "subject" %in% names(obs)) {
    groups <- split(seq_len(n), obs$subject)
  } else if (length(strata)) {
    groups <- split(seq_len(n), interaction(obs[, strata, drop = FALSE], drop = TRUE, lex.order = TRUE))
  } else {
    groups <- list(seq_len(n))
  }

  for (p in seq_len(nperm)) {
    idx <- seq_len(n)
    for (g in groups) idx[g] <- sample(g, length(g))
    out[, p] <- idx
  }
  out
}

.asca_freedman_lane_null <- function(term_fits, ctx, test) {
  nperm <- as.integer(test$nperm)
  out <- matrix(0, nrow = nperm, ncol = length(term_fits))
  colnames(out) <- names(term_fits)
  for (j in seq_along(term_fits)) {
    term_fit <- term_fits[[j]]
    reduced <- term_fit$reduced
    x_reduced <- .design_model_matrix(reduced, ctx$cell_table, contrasts = ctx$contrasts)
    x_reduced <- ctx$centering_operator %*% x_reduced
    xw <- x_reduced * sqrt(ctx$weights)
    yw <- ctx$crossblock * sqrt(ctx$weights)
    basis_r <- .weighted_basis(x_reduced, ctx$weights)
    fitted <- matrix(0, nrow = nrow(yw), ncol = ncol(yw))
    if (basis_r$rank > 0L) {
      fitted <- basis_r$q %*% crossprod(basis_r$q, yw)
    }
    residual <- yw - fitted
    perms <- .asca_cell_permutation_orders(ctx$cell_table, nperm = nperm, strata = test$exchangeability$strata %||% character(0))
    for (p in seq_len(nperm)) {
      y_perm_w <- fitted + residual[perms[, p], , drop = FALSE]
      out[p, j] <- .asca_weighted_stat(y_perm_w, basis = term_fit$basis, statistic = test$statistic)
    }
  }
  out
}

.asca_cell_permutation_orders <- function(cell_table, nperm, strata = character(0)) {
  n <- nrow(cell_table)
  out <- matrix(0L, nrow = n, ncol = nperm)
  strata <- intersect(strata, names(cell_table))
  if (!length(strata)) {
    for (p in seq_len(nperm)) out[, p] <- sample.int(n)
    return(out)
  }
  key <- interaction(cell_table[, strata, drop = FALSE], drop = TRUE, lex.order = TRUE)
  groups <- split(seq_len(n), key)
  for (p in seq_len(nperm)) {
    idx <- seq_len(n)
    for (g in groups) idx[g] <- sample(g, length(g))
    out[, p] <- idx
  }
  out
}

.asca_weighted_stat <- function(crossblock_w, basis, statistic) {
  if (ncol(basis) == 0L) {
    return(0)
  }
  coordinates <- crossprod(basis, crossblock_w)
  if (identical(statistic, "trace")) {
    return(sum(coordinates^2))
  }
  d <- svd(coordinates, nu = 0L, nv = 0L)$d
  if (!length(d)) 0 else d[[1L]]^2
}

.asca_partial_basis <- function(ctx, term, reduced) {
  term_id <- match(term, ctx$terms)
  term_cols <- which(ctx$assign == term_id)
  term_matrix <- ctx$centered_model[, term_cols, drop = FALSE]
  term_w <- term_matrix * sqrt(ctx$weights)
  x_reduced <- .design_model_matrix(reduced, ctx$cell_table, contrasts = ctx$contrasts)
  x_reduced <- ctx$centering_operator %*% x_reduced
  reduced_basis <- .weighted_basis(x_reduced, ctx$weights)
  residual <- term_w
  if (reduced_basis$rank > 0L) {
    residual <- term_w - reduced_basis$q %*% crossprod(reduced_basis$q, term_w)
  }
  qr_resid <- qr(residual, tol = 1e-10)
  rank <- as.integer(qr_resid$rank)
  q <- if (rank > 0L) qr.Q(qr_resid)[, seq_len(rank), drop = FALSE] else matrix(0, nrow = nrow(term_w), ncol = 0L)
  list(q = q, rank = rank, term_matrix = term_matrix, term_matrix_weighted = term_w)
}

.asca_build_term_results <- function(ctx, diagnostics, effect_fit, partial, selection_table, test) {
  out <- vector("list", length(ctx$terms))
  names(out) <- ctx$terms
  for (term in ctx$terms) {
    diag_row <- diagnostics[diagnostics$term == term, , drop = FALSE]
    effect <- effect_fit$term_fits[[term]]
    partial_fit <- partial$term_fits[[term]]
    bridge <- .asca_bridge_partial_to_effect(partial_fit, ctx$weights)
    test_row <- partial$anova[partial$anova$term == term, , drop = FALSE]
    sel_row <- selection_table[selection_table$term == term, , drop = FALSE]
    out[[term]] <- structure(
      list(
        name = term,
        estimand = list(
          formula_term = term,
          contrast_basis = effect$basis,
          cell_grid = ctx$cell_table,
          coding = ctx$contrasts,
          weights = ctx$weights,
          rank = effect$rank,
          estimability = diag_row
        ),
        test = list(
          space = "partial",
          reduced_formula = .formula_text(partial_fit$reduced),
          statistic = test_row$statistic,
          statistic_type = test_row$statistic_type,
          p_value = test_row$p_value,
          p_adjusted = test_row$p_adjusted,
          permutation_method = test$method,
          nperm = test$nperm,
          selection_status = sel_row$status
        ),
        display = list(
          effect = .asca_display_from_fit(effect, ctx$weights),
          partial = .asca_display_from_fit(partial_fit, ctx$weights),
          tested_effect = bridge$display
        ),
        bridge = bridge$metadata
      ),
      class = "asca_term_result"
    )
  }
  out
}

.asca_display_from_fit <- function(fit, weights) {
  design_profiles <- if (ncol(fit$left_vectors)) {
    fit$left_vectors / sqrt(weights)
  } else {
    matrix(0, nrow = length(weights), ncol = 0L)
  }
  list(
    singular_values = fit$singular_values,
    design_profiles = design_profiles,
    brain_saliences = fit$right_vectors,
    rank = fit$rank
  )
}

.asca_bridge_partial_to_effect <- function(partial_fit, weights) {
  if (!length(partial_fit$singular_values) || ncol(partial_fit$left_vectors) == 0L) {
    return(list(
      display = list(
        singular_values = partial_fit$singular_values,
        design_profiles = matrix(0, nrow = length(weights), ncol = 0L),
        brain_saliences = partial_fit$right_vectors,
        rank = partial_fit$rank
      ),
      metadata = list(
        method = "partial_to_effect_lift",
        alignment = numeric(0),
        warnings = "No estimable partial components."
      )
    ))
  }
  term_w <- partial_fit$term_matrix_weighted
  qr_term <- qr(term_w, tol = 1e-10)
  lifted_w <- matrix(0, nrow = nrow(term_w), ncol = ncol(partial_fit$left_vectors))
  alignment <- rep(NA_real_, ncol(partial_fit$left_vectors))
  for (k in seq_len(ncol(partial_fit$left_vectors))) {
    d <- partial_fit$left_vectors[, k]
    coef <- qr.coef(qr_term, d)
    coef[is.na(coef)] <- 0
    lifted_w[, k] <- term_w %*% coef
    if (sum(d^2) > 0 && sum(lifted_w[, k]^2) > 0) {
      alignment[[k]] <- abs(sum(d * lifted_w[, k]) / sqrt(sum(d^2) * sum(lifted_w[, k]^2)))
    }
  }
  list(
    display = list(
      singular_values = partial_fit$singular_values,
      design_profiles = lifted_w / sqrt(weights),
      brain_saliences = partial_fit$right_vectors,
      rank = partial_fit$rank
    ),
    metadata = list(
      method = "partial_to_effect_lift",
      alignment = alignment,
      warnings = if (any(is.na(alignment) | alignment < 0.95)) "Partial and lifted effect directions are not perfectly aligned." else character(0)
    )
  )
}

.asca_select_terms <- function(anova, selection) {
  out <- anova
  out$order <- .asca_term_order(out$term)
  out$rank_effect <- NA_integer_
  out$status <- ifelse(out$rank_partial > 0L, "kept", "not_estimable")
  out <- out[, c("term", "order", "rank_effect", "rank_partial", "statistic", "statistic_type", "p_value", "p_adjusted", "status")]
  if (!identical(selection$method, "backward") || all(is.na(out[[selection$adjust]]))) {
    return(out[order(out$order, out$term), , drop = FALSE])
  }
  kept <- out$status == "kept"
  for (ord in sort(unique(out$order), decreasing = TRUE)) {
    candidates <- which(kept & out$order == ord)
    for (idx in candidates) {
      term <- out$term[[idx]]
      protected <- any(kept & out$order > ord & vapply(out$term, .asca_term_contains, logical(1), lower = term))
      if (!protected && is.finite(out[[selection$adjust]][[idx]]) && out[[selection$adjust]][[idx]] > selection$alpha) {
        kept[[idx]] <- FALSE
        out$status[[idx]] <- "dropped"
      } else if (protected) {
        out$status[[idx]] <- "protected"
      }
    }
  }
  out$status[kept & out$status == "kept"] <- "kept"
  out[order(out$order, out$term), , drop = FALSE]
}

.asca_formula_from_terms <- function(terms) {
  terms <- terms[nzchar(terms)]
  if (!length(terms)) {
    return(stats::as.formula("~ 1"))
  }
  stats::as.formula(paste("~", paste(unique(terms), collapse = " + ")))
}

.asca_full_formula_for_term <- function(formula, term) {
  all_terms <- attr(stats::terms(formula), "term.labels")
  selected <- all_terms[.asca_term_order(all_terms) <= .asca_term_order(term)]
  .asca_formula_from_terms(selected)
}

.asca_reduced_formula <- function(formula, term) {
  all_terms <- attr(stats::terms(formula), "term.labels")
  order <- .asca_term_order(term)
  lower <- all_terms[.asca_term_order(all_terms) < order]
  .asca_formula_from_terms(lower)
}

.asca_term_order <- function(term) {
  vapply(strsplit(term, ":", fixed = TRUE), length, integer(1))
}

.asca_term_contains <- function(higher, lower) {
  hp <- strsplit(higher, ":", fixed = TRUE)[[1L]]
  lp <- strsplit(lower, ":", fixed = TRUE)[[1L]]
  all(lp %in% hp)
}
