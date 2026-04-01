#' Predictive CV Resampling Helpers
#'
#' Internal helpers for nested CV split construction and subject-level
#' permutation used by predictive evaluation.
#'
#' @keywords internal
NULL

.prediction_resampling_defaults <- function(task) {
  list(
    outer_folds = 5L,
    outer_repeats = 1L,
    inner_folds = 5L,
    inner_repeats = 1L,
    stratify = identical(task, "classification")
  )
}

.prediction_resampling_config <- function(task, resampling = NULL) {
  cfg <- utils::modifyList(.prediction_resampling_defaults(task), resampling %||% list())

  if (!is.null(cfg$folds)) {
    cfg$outer_folds <- cfg$outer_folds %||% cfg$folds
    cfg$inner_folds <- cfg$inner_folds %||% cfg$folds
  }
  if (!is.null(cfg$repeats)) {
    cfg$outer_repeats <- cfg$outer_repeats %||% cfg$repeats
    cfg$inner_repeats <- cfg$inner_repeats %||% cfg$repeats
  }

  cfg$outer_folds <- as.integer(cfg$outer_folds)
  cfg$outer_repeats <- as.integer(cfg$outer_repeats)
  cfg$inner_folds <- as.integer(cfg$inner_folds)
  cfg$inner_repeats <- as.integer(cfg$inner_repeats)
  cfg$stratify <- isTRUE(cfg$stratify)

  if (cfg$outer_folds < 2L || cfg$inner_folds < 2L) {
    stop("Prediction resampling requires at least 2 folds for both outer and inner CV.", call. = FALSE)
  }
  if (cfg$outer_repeats < 1L || cfg$inner_repeats < 1L) {
    stop("Prediction resampling repeats must be positive integers.", call. = FALSE)
  }

  cfg
}

.prediction_assign_fold_labels <- function(ids, n_folds) {
  ids <- sample(as.integer(ids), length(ids), replace = FALSE)
  fold_ids <- cut(seq_along(ids), breaks = n_folds, labels = FALSE)
  out <- vector("list", n_folds)
  for (f in seq_len(n_folds)) {
    out[[f]] <- ids[fold_ids == f]
  }
  out
}

.prediction_make_splits <- function(index,
                                    subject_ids,
                                    outcome,
                                    n_folds,
                                    n_repeats,
                                    stratify = FALSE,
                                    seed = NULL) {
  subject_ids <- .prediction_order_subjects(index, subject_ids)
  tab <- index$subjects[index$subjects$subject_id %in% subject_ids, , drop = FALSE]
  min_group_n <- min(table(tab$group_index))
  actual_folds <- min(as.integer(n_folds), as.integer(min_group_n))

  if (actual_folds < 2L) {
    stop("Predictive cross-validation requires at least 2 subjects per group in every training split.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(as.integer(seed)[1])
  }

  splits <- vector("list", actual_folds * as.integer(n_repeats))
  idx <- 1L

  for (rep_idx in seq_len(as.integer(n_repeats))) {
    fold_members <- vector("list", actual_folds)
    for (f in seq_len(actual_folds)) {
      fold_members[[f]] <- integer(0)
    }

    for (g in unique(tab$group_index)) {
      group_ids <- tab$subject_id[tab$group_index == g]

      if (isTRUE(stratify)) {
        class_levels <- split(group_ids, outcome[group_ids])
        for (class_ids in class_levels) {
          assigned <- .prediction_assign_fold_labels(class_ids, actual_folds)
          for (f in seq_len(actual_folds)) {
            fold_members[[f]] <- c(fold_members[[f]], assigned[[f]] %||% integer(0))
          }
        }
      } else {
        assigned <- .prediction_assign_fold_labels(group_ids, actual_folds)
        for (f in seq_len(actual_folds)) {
          fold_members[[f]] <- c(fold_members[[f]], assigned[[f]] %||% integer(0))
        }
      }
    }

    all_ids <- .prediction_order_subjects(index, subject_ids)

    for (f in seq_len(actual_folds)) {
      test_ids <- sort(unique(as.integer(fold_members[[f]])))
      train_ids <- setdiff(all_ids, test_ids)

      splits[[idx]] <- data.frame(
        `repeat` = rep_idx,
        fold = f,
        train_subject_ids = I(list(train_ids)),
        test_subject_ids = I(list(test_ids)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  do.call(rbind, splits)
}

.prediction_permute_outcome <- function(outcome, index) {
  permuted <- outcome
  tab <- index$subjects
  for (g in unique(tab$group_index)) {
    ids <- tab$subject_id[tab$group_index == g]
    permuted[ids] <- sample(outcome[ids], length(ids), replace = FALSE)
  }
  permuted
}
