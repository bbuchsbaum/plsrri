#' Predictive CV Core Helpers
#'
#' Internal helpers for subject indexing, spec subsetting, and score-to-subject
#' feature assembly used by the predictive workflow.
#'
#' @keywords internal
NULL

.prediction_validate_outcome <- function(outcome, task, n_subjects) {
  if (length(outcome) != n_subjects) {
    stop(
      sprintf(
        "Outcome length must equal the number of subjects (%d).",
        n_subjects
      ),
      call. = FALSE
    )
  }

  if (task == "classification") {
    if (is.logical(outcome)) {
      outcome <- factor(ifelse(outcome, "TRUE", "FALSE"))
    } else if (is.factor(outcome)) {
      outcome <- droplevels(outcome)
    } else if (is.character(outcome)) {
      outcome <- factor(outcome)
    } else if (is.numeric(outcome)) {
      if (!all(outcome %in% c(0, 1))) {
        stop("Numeric classification outcomes must be coded as 0/1.", call. = FALSE)
      }
      outcome <- factor(outcome, levels = c(0, 1))
    } else {
      stop("Unsupported classification outcome type.", call. = FALSE)
    }

    if (nlevels(outcome) != 2L) {
      stop("Classification currently supports binary outcomes only.", call. = FALSE)
    }
    return(outcome)
  }

  if (!is.numeric(outcome)) {
    stop("Regression outcomes must be numeric.", call. = FALSE)
  }

  as.numeric(outcome)
}

.prediction_subject_index <- function(spec) {
  assert_that(inherits(spec, "pls_spec"))

  if (is.list(spec$num_subj_lst)) {
    stop(
      "Predictive cross-validation currently requires balanced subject counts per group.",
      call. = FALSE
    )
  }

  n_by_group <- as.integer(spec$num_subj_lst)
  k <- as.integer(spec$num_cond)
  groups <- spec$groups %||% paste0("group_", seq_along(n_by_group))

  subjects <- vector("list", sum(n_by_group))
  row_index <- vector("list", sum(n_by_group))

  subject_id <- 1L
  row_offset <- 0L

  for (g in seq_along(n_by_group)) {
    n <- n_by_group[g]
    for (s in seq_len(n)) {
      rows <- row_offset + s + n * (seq_len(k) - 1L)
      subjects[[subject_id]] <- data.frame(
        subject_id = subject_id,
        group_index = g,
        group = as.character(groups[g]),
        subject_in_group = s,
        stringsAsFactors = FALSE
      )
      row_index[[subject_id]] <- as.integer(rows)
      subject_id <- subject_id + 1L
    }
    row_offset <- row_offset + n * k
  }

  list(
    subjects = do.call(rbind, subjects),
    row_index = row_index,
    n_subjects = sum(n_by_group),
    num_cond = k
  )
}

.prediction_order_subjects <- function(index, subject_ids) {
  subject_ids <- unique(as.integer(subject_ids))
  tab <- index$subjects
  ord <- tab$subject_id[tab$subject_id %in% subject_ids]
  ord[order(match(ord, tab$subject_id))]
}

.prediction_subset_spec <- function(spec, subject_ids, index) {
  subject_ids <- .prediction_order_subjects(index, subject_ids)
  tab <- index$subjects[index$subjects$subject_id %in% subject_ids, , drop = FALSE]
  tab <- tab[order(tab$group_index, tab$subject_in_group), , drop = FALSE]

  out <- spec
  out$num_perm <- 0L
  out$num_boot <- 0L
  out$num_split <- 0L

  out$datamat_lst <- vector("list", length(spec$datamat_lst))
  out$num_subj_lst <- integer(length(spec$datamat_lst))

  for (g in seq_along(spec$datamat_lst)) {
    gtab <- tab[tab$group_index == g, , drop = FALSE]
    n_train <- nrow(gtab)
    if (n_train < 1L) {
      stop("Each predictive split must retain at least one subject per group.", call. = FALSE)
    }

    n_full <- as.integer(spec$num_subj_lst[g])
    rows <- as.integer(unlist(lapply(gtab$subject_in_group, function(s) {
      s + n_full * (seq_len(spec$num_cond) - 1L)
    }), use.names = FALSE))

    out$datamat_lst[[g]] <- spec$datamat_lst[[g]][rows, , drop = FALSE]
    out$num_subj_lst[g] <- n_train
  }

  global_rows <- sort(as.integer(unlist(index$row_index[subject_ids], use.names = FALSE)))
  if (!is.null(spec$stacked_behavdata)) {
    out$stacked_behavdata <- spec$stacked_behavdata[global_rows, , drop = FALSE]
  }

  out
}

.prediction_subject_matrix <- function(score_mat, num_subj_lst, num_cond, n_components) {
  stopifnot(is.matrix(score_mat))
  n_components <- as.integer(n_components)
  k <- as.integer(num_cond)
  n_by_group <- as.integer(num_subj_lst)

  pieces <- vector("list", length(n_by_group))
  row_offset <- 0L

  for (g in seq_along(n_by_group)) {
    n <- n_by_group[g]
    g_rows <- row_offset + seq_len(n * k)
    g_scores <- score_mat[g_rows, seq_len(n_components), drop = FALSE]

    cols <- lapply(seq_len(n_components), function(comp) {
      matrix(g_scores[, comp], nrow = n, ncol = k)
    })
    pieces[[g]] <- do.call(cbind, cols)
    row_offset <- row_offset + n * k
  }

  out <- do.call(rbind, pieces)
  colnames(out) <- as.vector(vapply(seq_len(n_components), function(comp) {
    paste0("LV", comp, "_C", seq_len(k))
  }, character(k)))
  out
}
