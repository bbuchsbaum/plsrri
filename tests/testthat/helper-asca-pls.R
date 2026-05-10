make_asca_pls_synthetic_fixture <- function(seed = 20260510,
                                            n_per_group = c(control = 10L, sdam = 10L),
                                            n_features = 48L,
                                            signal_scale = 1,
                                            effects = NULL,
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
    group = 25:32,
    task = 33:40,
    level = 9:16,
    `group:task` = 1:8,
    `group:level` = 41:44,
    `task:level` = 45:48,
    `group:task:level` = 17:24
  )
  n_features <- max(as.integer(n_features), max(unlist(supports)))
  default_effects <- c(
    group = 0,
    task = 0,
    level = 2.0,
    `group:task` = 2.5,
    `group:level` = 0,
    `task:level` = 0,
    `group:task:level` = 1.8
  )
  if (is.null(effects)) {
    effects <- default_effects
  } else {
    effects <- utils::modifyList(as.list(default_effects), as.list(effects))
    effects <- unlist(effects, use.names = TRUE)
  }

  metadata_rows <- list()
  make_group <- function(group, n_subjects) {
    group_code <- if (identical(group, "sdam")) 1 else -1
    subject_offsets <- replicate(n_subjects, rnorm(n_features, sd = 0.10), simplify = FALSE)
    rows <- vector("list", n_subjects * nrow(condition_key))
    row_id <- 1L
    for (i in seq_len(nrow(condition_key))) {
      for (subject in seq_len(n_subjects)) {
        task_code <- if (condition_key$task[[i]] == "nback") 1 else -1
        level_code <- if (condition_key$level[[i]] == "high") 1 else -1
        x <- rnorm(n_features, sd = noise_sd) + subject_offsets[[subject]]
        x[supports$group] <- x[supports$group] + signal_scale * effects[["group"]] * group_code
        x[supports$task] <- x[supports$task] + signal_scale * effects[["task"]] * task_code
        x[supports$level] <- x[supports$level] + signal_scale * effects[["level"]] * level_code
        x[supports$`group:task`] <- x[supports$`group:task`] +
          signal_scale * effects[["group:task"]] * group_code * task_code
        x[supports$`group:level`] <- x[supports$`group:level`] +
          signal_scale * effects[["group:level"]] * group_code * level_code
        x[supports$`task:level`] <- x[supports$`task:level`] +
          signal_scale * effects[["task:level"]] * task_code * level_code
        x[supports$`group:task:level`] <- x[supports$`group:task:level`] +
          signal_scale * effects[["group:task:level"]] * group_code * task_code * level_code
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
    effects = effects,
    n_per_group = n_per_group,
    formula = ~ group * task * level
  )
}
