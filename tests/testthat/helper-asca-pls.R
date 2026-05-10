make_asca_pls_synthetic_fixture <- function(seed = 20260510,
                                            n_per_group = c(control = 10L, sdam = 10L),
                                            n_features = 48L,
                                            signal_scale = 1,
                                            noise_sd = 0.25,
                                            meancentering = "grand_mean") {
  set.seed(seed)
  condition_key <- expand.grid(
    level = c("low", "high"),
    task = c("recog", "nback"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  condition_key$condition <- paste(condition_key$task, condition_key$level, sep = "_")
  condition_key <- condition_key[, c("condition", "task", "level")]

  supports <- list(
    `group:task` = 1:8,
    level = 9:16,
    `group:task:level` = 17:24
  )
  n_features <- max(as.integer(n_features), max(unlist(supports)))

  metadata_rows <- list()
  make_group <- function(group, n_subjects) {
    group_code <- if (identical(group, "sdam")) 1 else -1
    rows <- vector("list", n_subjects * nrow(condition_key))
    row_id <- 1L
    for (subject in seq_len(n_subjects)) {
      subject_offset <- rnorm(n_features, sd = 0.10)
      for (i in seq_len(nrow(condition_key))) {
        task_code <- if (condition_key$task[[i]] == "nback") 1 else -1
        level_code <- if (condition_key$level[[i]] == "high") 1 else -1
        x <- rnorm(n_features, sd = noise_sd) + subject_offset
        x[supports$`group:task`] <- x[supports$`group:task`] +
          signal_scale * 2.5 * group_code * task_code
        x[supports$level] <- x[supports$level] +
          signal_scale * 2.0 * level_code
        x[supports$`group:task:level`] <- x[supports$`group:task:level`] +
          signal_scale * 1.8 * group_code * task_code * level_code
        rows[[row_id]] <- x
        metadata_rows[[length(metadata_rows) + 1L]] <<- data.frame(
          subject = paste(group, subject, sep = "_"),
          group = group,
          condition = condition_key$condition[[i]],
          task = condition_key$task[[i]],
          level = condition_key$level[[i]],
          group_code = group_code,
          task_code = task_code,
          level_code = level_code,
          stringsAsFactors = FALSE
        )
        row_id <- row_id + 1L
      }
    }
    do.call(rbind, rows)
  }

  groups <- names(n_per_group)
  if (is.null(groups) || any(!nzchar(groups))) {
    groups <- paste0("g", seq_along(n_per_group))
  }
  datamat_lst <- Map(make_group, groups, as.integer(n_per_group))

  spec <- pls_spec() |>
    add_subjects(datamat_lst, groups = as.integer(n_per_group)) |>
    add_conditions(nrow(condition_key), labels = condition_key$condition) |>
    add_group_labels(groups) |>
    configure(method = "task", meancentering = meancentering)

  fit <- run(spec, progress = FALSE, keep_crossblock = TRUE)
  design <- pls_design(
    ~ group * task * level,
    condition_key = condition_key,
    between = "group",
    within = c("task", "level")
  )

  list(
    spec = spec,
    fit = fit,
    design = design,
    datamat = do.call(rbind, datamat_lst),
    metadata = do.call(rbind, metadata_rows),
    condition_key = condition_key,
    supports = supports,
    n_per_group = n_per_group,
    formula = ~ group * task * level
  )
}
