# Mean-centered Task PLS on the SDAM first-level example maps.
#
# This script is intentionally written as tutorial code.  The helper functions
# make each data decision explicit: how subjects are grouped, how task/measure
# maps become PLS conditions, and how a common analysis mask is derived from
# pre-masked maps.

SDAM_TASKS <- c("recog", "nback")
SDAM_MEASURES <- c("low_mid", "high_sem")
SDAM_GROUPS <- c("control", "sdam")
SDAM_SEED <- 20260508L

sdam_testdata_root <- function(root = Sys.getenv("PLSRRI_SDAM_TESTDATA", "testdata")) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!dir.exists(root)) {
    stop(
      "Could not find the SDAM testdata directory. ",
      "Set PLSRRI_SDAM_TESTDATA or run from the plsrri repository root.",
      call. = FALSE
    )
  }
  root
}

sdam_condition_table <- function() {
  data.frame(
    task = rep(SDAM_TASKS, each = length(SDAM_MEASURES)),
    measure = rep(SDAM_MEASURES, times = length(SDAM_TASKS)),
    stringsAsFactors = FALSE
  ) |>
    transform(condition = paste(task, measure, sep = "_"))
}

sdam_condition_key <- function() {
  key <- sdam_condition_table()[, c("condition", "task", "measure")]
  names(key)[names(key) == "measure"] <- "level"
  key
}

read_sdam_participants <- function(root = sdam_testdata_root()) {
  participants_file <- file.path(root, "participants.tsv")
  participants <- utils::read.table(
    participants_file,
    header = TRUE,
    sep = "",
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE
  )

  participants <- participants[, c("participant_id", "group")]
  names(participants)[names(participants) == "participant_id"] <- "subject"
  participants$subject <- as.character(participants$subject)
  participants$group <- factor(participants$group, levels = SDAM_GROUPS)
  participants <- participants[order(participants$group, participants$subject), ]
  rownames(participants) <- NULL
  participants
}

build_sdam_manifest <- function(root = sdam_testdata_root()) {
  root <- sdam_testdata_root(root)
  participants <- read_sdam_participants(root)
  conditions <- sdam_condition_table()

  rows <- vector("list", nrow(participants) * nrow(conditions))
  row_id <- 1L
  for (subject_id in participants$subject) {
    group <- as.character(participants$group[participants$subject == subject_id])
    for (condition_id in seq_len(nrow(conditions))) {
      task <- conditions$task[[condition_id]]
      measure <- conditions$measure[[condition_id]]
      rows[[row_id]] <- data.frame(
        subject = subject_id,
        group = group,
        task = task,
        measure = measure,
        condition = conditions$condition[[condition_id]],
        file = file.path(
          root,
          sprintf("%s_vgg_model_rsa_%s", subject_id, task),
          "maps",
          sprintf("%s.nii.gz", measure)
        ),
        stringsAsFactors = FALSE
      )
      row_id <- row_id + 1L
    }
  }

  manifest <- do.call(rbind, rows)
  manifest$group <- factor(manifest$group, levels = SDAM_GROUPS)
  manifest$condition <- factor(manifest$condition, levels = conditions$condition)

  missing_files <- manifest$file[!file.exists(manifest$file)]
  if (length(missing_files) > 0L) {
    stop(
      "The manifest references missing map files: ",
      paste(utils::head(missing_files, 5), collapse = ", "),
      call. = FALSE
    )
  }

  manifest
}

summarise_sdam_design <- function(manifest) {
  subject_table <- unique(manifest[, c("group", "subject")])
  group_counts <- as.data.frame(table(subject_table$group), stringsAsFactors = FALSE)
  names(group_counts) <- c("group", "subjects")

  condition_counts <- as.data.frame(table(manifest$condition), stringsAsFactors = FALSE)
  names(condition_counts) <- c("condition", "maps")

  list(
    n_subjects = nrow(subject_table),
    n_maps = nrow(manifest),
    group_counts = group_counts,
    condition_counts = condition_counts
  )
}

derive_common_nonzero_mask <- function(manifest, max_voxels = Inf) {
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required to derive the SDAM analysis mask.", call. = FALSE)
  }

  files <- unique(as.character(manifest$file))
  first_map <- neuroim2::read_vol(files[[1]])
  first_array <- as.array(first_map)
  common_voxels <- is.finite(first_array) & first_array != 0

  if (length(files) > 1L) {
    for (file in files[-1L]) {
      map_array <- as.array(neuroim2::read_vol(file))
      if (!identical(dim(map_array), dim(common_voxels))) {
        stop("All SDAM maps must have the same dimensions.", call. = FALSE)
      }
      common_voxels <- common_voxels & is.finite(map_array) & map_array != 0
    }
  }

  if (is.finite(max_voxels)) {
    keep <- utils::head(which(common_voxels), as.integer(max_voxels))
    common_voxels[] <- FALSE
    common_voxels[keep] <- TRUE
  }

  n_voxels <- sum(common_voxels)
  if (n_voxels == 0L) {
    stop("The derived common nonzero mask contains no voxels.", call. = FALSE)
  }

  mask_array <- array(as.integer(common_voxels), dim = dim(common_voxels))
  neuroim2::NeuroVol(mask_array, neuroim2::space(first_map))
}

make_sdam_task_pls_spec <- function(manifest,
                                    mask,
                                    nperm = 1000L,
                                    nboot = 500L,
                                    meancentering = "within_group") {
  spec <- plsrri::pls_spec()
  spec <- plsrri::add_subjects_map_manifest(
    spec = spec,
    manifest = manifest,
    mask = mask,
    subject_col = "subject",
    condition_col = "condition",
    group_col = "group",
    file_col = "file"
  )
  plsrri::configure(
    spec,
    method = "task",
    meancentering = meancentering,
    nperm = nperm,
    nboot = nboot,
    boot_type = "strat"
  )
}

summarise_sdam_result <- function(result, alpha = 0.05) {
  p_values <- plsrri::significance(result)
  variance <- plsrri::singular_values(result, normalize = TRUE)
  data.frame(
    lv = seq_along(p_values),
    variance_percent = round(as.numeric(variance), 2),
    p_value = round(as.numeric(p_values), 4),
    significant = as.numeric(p_values) < alpha,
    stringsAsFactors = FALSE
  )
}

sdam_values_to_neurovol <- function(values, mask) {
  values <- as.numeric(values)
  mask_array <- as.array(mask) > 0
  if (length(values) != sum(mask_array)) {
    stop(
      "The value vector has length ", length(values),
      " but the mask contains ", sum(mask_array), " voxels.",
      call. = FALSE
    )
  }

  volume_array <- array(NA_real_, dim = dim(mask))
  volume_array[mask_array] <- values
  neuroim2::NeuroVol(volume_array, neuroim2::space(mask))
}

sdam_bsr_neurovol <- function(result, lv = 1L, threshold = NULL) {
  values <- plsrri::bsr(result, lv = lv)
  if (!is.null(threshold)) {
    values[abs(values) < threshold] <- NA_real_
  }
  sdam_values_to_neurovol(values, result$mask)
}

plot_sdam_bsr_volume <- function(result, lv = 1L, threshold = 3) {
  plsrri::plot_brain(
    result,
    lv = lv,
    what = "bsr",
    threshold = threshold
  )
}

sdam_bsr_overlay_limits <- function(result,
                                    lv = 1L,
                                    threshold = 3,
                                    probs = 0.99) {
  values <- abs(as.numeric(plsrri::bsr(result, lv = lv)))
  values <- values[is.finite(values)]
  if (!is.null(threshold)) {
    values <- values[values >= threshold]
  }

  if (!length(values)) {
    threshold_fallback <- if (is.null(threshold)) 1 else threshold
    limit <- max(1, threshold_fallback)
  } else {
    limit <- as.numeric(stats::quantile(values, probs = probs, names = FALSE))
    if (!is.finite(limit) || limit <= 0) {
      threshold_fallback <- if (is.null(threshold)) 1 else threshold
      limit <- max(values, threshold_fallback, na.rm = TRUE)
    }
  }

  c(-limit, limit)
}

sdam_bsr_colorbar <- function(lim,
                              palette = "vik",
                              title = "Bootstrap ratio") {
  if (!requireNamespace("scico", quietly = TRUE)) {
    stop("Package 'scico' is required for the surface BSR colorbar.", call. = FALSE)
  }

  scale_df <- data.frame(
    x = seq(lim[[1]], lim[[2]], length.out = 256L),
    y = 1,
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(scale_df, ggplot2::aes(x = x, y = y, fill = x)) +
    ggplot2::geom_raster() +
    scico::scale_fill_scico(palette = palette, limits = lim, guide = "none") +
    ggplot2::scale_x_continuous(breaks = pretty(lim, n = 5)) +
    ggplot2::labs(x = title, y = NULL) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(color = "grey20"),
      axis.title.x = ggplot2::element_text(color = "grey20", margin = ggplot2::margin(t = 4)),
      axis.ticks.x = ggplot2::element_line(color = "grey45"),
      plot.margin = ggplot2::margin(t = 0, r = 18, b = 4, l = 18)
    )
}

plot_sdam_bsr_surface <- function(result,
                                  lv = 1L,
                                  threshold = 3,
                                  surface_atlas = NULL,
                                  views = c("lateral", "medial"),
                                  overlay_lim = NULL,
                                  overlay_palette = "vik",
                                  base_color = "#e8e8e8") {
  if (!requireNamespace("neuroatlas", quietly = TRUE)) {
    stop("Package 'neuroatlas' is required for the surface BSR plot.", call. = FALSE)
  }

  if (is.null(surface_atlas)) {
    surface_atlas <- neuroatlas::schaefer_surf(
      parcels = 200,
      networks = 7,
      space = "fsaverage6",
      surf = "inflated"
    )
  }

  if (is.null(overlay_lim)) {
    overlay_lim <- sdam_bsr_overlay_limits(result, lv = lv, threshold = threshold)
  }

  base_colors <- stats::setNames(
    rep(base_color, length(surface_atlas$ids)),
    as.character(surface_atlas$ids)
  )

  surface_plot <- neuroatlas::plot_brain(
    surface_atlas,
    colors = base_colors,
    overlay = sdam_bsr_neurovol(result, lv = lv, threshold = NULL),
    overlay_threshold = threshold,
    overlay_palette = overlay_palette,
    overlay_lim = overlay_lim,
    overlay_alpha = 0.9,
    views = views,
    interactive = FALSE,
    style = "ggseg_like",
    colorbar = FALSE,
    title = sprintf("LV%d bootstrap ratio on fsaverage6", lv),
    subtitle = sprintf(
      "neuroatlas vol_to_surf overlay, |projected BSR| > %g",
      threshold
    ),
    caption = sprintf(
      "Neutral surface shows coverage; overlay colours are clipped to [%0.2f, %0.2f].",
      overlay_lim[[1]], overlay_lim[[2]]
    ),
    panel_labels = c(
      "Left Lateral" = "LH lateral",
      "Right Lateral" = "RH lateral",
      "Left Medial" = "LH medial",
      "Right Medial" = "RH medial"
    )
  )

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    return(surface_plot)
  }

  surface_plot / sdam_bsr_colorbar(overlay_lim, palette = overlay_palette) +
    patchwork::plot_layout(heights = c(1, 0.08))
}

make_sdam_asca_design <- function() {
  plsrri::pls_design(
    ~ group * task * level,
    condition_key = sdam_condition_key(),
    between = "group",
    within = c("task", "level")
  )
}

run_sdam_asca_pls <- function(analysis,
                              nperm = 1000L,
                              statistic = "trace",
                              selection_alpha = 0.05,
                              correction = "maxT") {
  nperm <- as.integer(nperm)
  test_method <- if (nperm > 0L) "freedman_lane" else "none"
  selection_method <- if (nperm > 0L) "backward" else "none"
  plsrri::asca_pls(
    analysis$spec,
    fit = analysis$result,
    decompose = ~ group * task * level,
    condition_key = sdam_condition_key(),
    id = "subject",
    within = c("task", "level"),
    between = "group",
    test = plsrri::partial_test(
      method = test_method,
      statistic = statistic,
      nperm = nperm,
      correction = correction
    ),
    selection = plsrri::hierarchical_selection(
      method = selection_method,
      alpha = selection_alpha
    )
  )
}

summarise_sdam_asca <- function(asca_result) {
  tab <- stats::anova(asca_result)
  formula_terms <- attr(stats::terms(asca_result$decompose), "term.labels")
  formula_terms <- formula_terms[formula_terms %in% tab$term]
  if (length(formula_terms)) {
    tab <- tab[match(formula_terms, tab$term), , drop = FALSE]
    rownames(tab) <- NULL
  }
  tab$order <- lengths(strsplit(tab$term, ":", fixed = TRUE))
  tab$effect_type <- c(
    "main_effect",
    "two_way_interaction",
    "three_way_interaction"
  )[pmin(tab$order, 3L)]
  tab <- tab[, c(
    "term",
    "effect_type",
    "order",
    "rank_effect",
    "rank_partial",
    "statistic",
    "statistic_type",
    "p_value",
    "p_adjusted",
    "status"
  )]
  tab$statistic <- round(tab$statistic, 4)
  tab$p_value <- round(tab$p_value, 4)
  tab$p_adjusted <- round(tab$p_adjusted, 4)
  tab
}

write_sdam_asca_formula <- function(asca_result, file) {
  formula_text <- paste(deparse(plsrri::selected_formula(asca_result)), collapse = "")
  writeLines(formula_text, con = file)
  invisible(formula_text)
}

sdam_asca_plottable_terms <- function(asca_result,
                                      terms = stats::anova(asca_result)$term,
                                      component = 1L) {
  terms[vapply(terms, function(term) {
    isTRUE(tryCatch({
      plsrri::asca_components(
        asca_result,
        term = term,
        view = "tested_effect",
        component = component
      )
      TRUE
    }, error = function(e) FALSE))
  }, logical(1))]
}

sdam_asca_supported_terms <- function(asca_result,
                                      statuses = c("kept", "protected")) {
  tab <- stats::anova(asca_result)
  tab$term[tab$status %in% statuses]
}

save_sdam_asca_outputs <- function(asca_result,
                                   output_dir,
                                   component = 1L,
                                   terms = character(0)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to save SDAM ASCA plots.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(asca_result, file.path(output_dir, "sdam-asca-pls-result.rds"))
  utils::write.csv(
    summarise_sdam_asca(asca_result),
    file.path(output_dir, "asca-term-summary.csv"),
    row.names = FALSE
  )
  write_sdam_asca_formula(
    asca_result,
    file.path(output_dir, "asca-selected-formula.txt")
  )

  ggplot2::ggsave(
    file.path(output_dir, "asca-selection.png"),
    plsrri::plot_asca_selection(asca_result),
    width = 7,
    height = 4,
    dpi = 150
  )

  profile_terms <- sdam_asca_plottable_terms(asca_result, terms = terms, component = component)

  alignment_terms <- sdam_asca_plottable_terms(
    asca_result,
    terms = stats::anova(asca_result)$term,
    component = component
  )
  if (length(alignment_terms)) {
    ggplot2::ggsave(
      file.path(output_dir, "asca-alignment.png"),
      plsrri::plot_asca_alignment(asca_result, term = alignment_terms),
      width = 7,
      height = 4,
      dpi = 150
    )
  }

  profile_files <- character(0)
  for (term in profile_terms) {
    profile_file <- file.path(output_dir, sprintf("asca-%s-profile.png", gsub(":", "-", term, fixed = TRUE)))
    ggplot2::ggsave(
      profile_file,
      plsrri::plot_asca_effect_profile(
        asca_result,
        term = term,
        view = "tested_effect",
        component = component,
        x_axis = "level",
        line = "task",
        facet = "group"
      ),
      width = 7,
      height = 4,
      dpi = 150
    )
    profile_files <- c(profile_files, profile_file)
  }

  invisible(list(
    result = asca_result,
    profile_terms = profile_terms,
    profile_files = profile_files
  ))
}

save_sdam_design_plots <- function(result, output_dir, lv = 1L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to save SDAM tutorial plots.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    file.path(output_dir, sprintf("lv%d-design-scores.png", lv)),
    plsrri::plot_scores(result, lv = lv, type = "design", plot_type = "bar"),
    width = 7,
    height = 4,
    dpi = 150
  )
  condition_key <- sdam_condition_key()
  ggplot2::ggsave(
    file.path(output_dir, sprintf("lv%d-design-heatmap.png", lv)),
    plsrri::plot_design_heatmap(
      result,
      lv = lv,
      condition_key = condition_key,
      row = "level",
      column = "task",
      facet = "group"
    ),
    width = 7,
    height = 4,
    dpi = 150
  )
  ggplot2::ggsave(
    file.path(output_dir, sprintf("lv%d-design-interaction.png", lv)),
    plsrri::plot_design_interaction(
      result,
      lv = lv,
      condition_key = condition_key,
      x_axis = "task",
      trace = "level",
      facet = "group"
    ),
    width = 7,
    height = 4,
    dpi = 150
  )
  ggplot2::ggsave(
    file.path(output_dir, sprintf("lv%d-design-contrasts.png", lv)),
    plsrri::plot_design_contrasts(
      result,
      lv = lv,
      condition_key = condition_key
    ),
    width = 7,
    height = 4,
    dpi = 150
  )
}

save_sdam_plots <- function(result, output_dir, lv = 1L, bsr_threshold = 3) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to save SDAM tutorial plots.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    file.path(output_dir, "singular-values.png"),
    plsrri::plot_singular_values(result),
    width = 7,
    height = 4,
    dpi = 150
  )
  save_sdam_design_plots(result, output_dir = output_dir, lv = lv)
  condition_key <- sdam_condition_key()
  if (length(result$s) >= 2L) {
    ggplot2::ggsave(
      file.path(output_dir, "lv1-lv2-design-space.png"),
      plsrri::plot_design_score_space(
        result,
        lv = c(1L, 2L),
        condition_key = condition_key,
        label = c("group", "condition")
      ),
      width = 7,
      height = 5,
      dpi = 150
    )
  }
  ggplot2::ggsave(
    file.path(output_dir, sprintf("lv%d-brain-scores.png", lv)),
    plsrri::plot_scores(result, lv = lv, type = "brain", plot_type = "violin"),
    width = 7,
    height = 4,
    dpi = 150
  )
  ggplot2::ggsave(
    file.path(output_dir, sprintf("lv%d-bsr-volume.png", lv)),
    plot_sdam_bsr_volume(result, lv = lv, threshold = bsr_threshold),
    width = 8,
    height = 6,
    dpi = 150
  )

  if (requireNamespace("neuroatlas", quietly = TRUE)) {
    ggplot2::ggsave(
      file.path(output_dir, sprintf("lv%d-bsr-surface.png", lv)),
      plot_sdam_bsr_surface(result, lv = lv, threshold = bsr_threshold),
      width = 9,
      height = 6,
      dpi = 150
    )
  }
}

run_sdam_task_pls <- function(root = sdam_testdata_root(),
                              nperm = 1000L,
                              nboot = 500L,
                              seed = SDAM_SEED,
                              max_voxels = Inf,
                              progress = TRUE) {
  manifest <- build_sdam_manifest(root)
  mask <- derive_common_nonzero_mask(manifest, max_voxels = max_voxels)
  spec <- make_sdam_task_pls_spec(
    manifest = manifest,
    mask = mask,
    nperm = nperm,
    nboot = nboot
  )

  set.seed(seed)
  result <- plsrri::run(spec, progress = progress, keep_crossblock = TRUE)
  list(manifest = manifest, mask = mask, spec = spec, result = result)
}

main <- function() {
  output_dir <- file.path("artifacts", "sdam-firstlevel-task-pls")
  nperm <- as.integer(Sys.getenv("PLSRRI_SDAM_NPERM", "1000"))
  nboot <- as.integer(Sys.getenv("PLSRRI_SDAM_NBOOT", "500"))
  asca_nperm <- as.integer(Sys.getenv("PLSRRI_SDAM_ASCA_NPERM", as.character(nperm)))

  analysis <- run_sdam_task_pls(nperm = nperm, nboot = nboot)
  asca <- run_sdam_asca_pls(analysis, nperm = asca_nperm)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(analysis$result, file.path(output_dir, "sdam-task-pls-result.rds"))
  utils::write.csv(
    summarise_sdam_result(analysis$result),
    file.path(output_dir, "latent-variable-summary.csv"),
    row.names = FALSE
  )
  save_sdam_plots(analysis$result, output_dir = output_dir, lv = 1L)
  if (length(analysis$result$s) >= 2L) {
    save_sdam_design_plots(analysis$result, output_dir = output_dir, lv = 2L)
  }
  save_sdam_asca_outputs(asca, output_dir = output_dir, component = 1L)

  print(summarise_sdam_design(analysis$manifest))
  print(summarise_sdam_result(analysis$result))
  print(summarise_sdam_asca(asca))
  invisible(list(analysis = analysis, asca = asca))
}

if (sys.nframe() == 0L) {
  main()
}
