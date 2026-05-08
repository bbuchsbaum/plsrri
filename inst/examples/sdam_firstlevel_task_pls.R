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

plot_sdam_bsr_surface <- function(result,
                                  lv = 1L,
                                  threshold = 3,
                                  surface_atlas = NULL,
                                  views = c("lateral", "medial")) {
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

  neuroatlas::plot_brain(
    surface_atlas,
    overlay = sdam_bsr_neurovol(result, lv = lv, threshold = threshold),
    overlay_threshold = threshold,
    overlay_palette = "vik",
    overlay_alpha = 0.65,
    views = views,
    interactive = FALSE,
    style = "ggseg_like",
    colorbar = "bottom",
    colorbar_title = "Bootstrap ratio",
    title = sprintf("LV%d bootstrap ratio on fsaverage6", lv),
    subtitle = sprintf("Volume-to-surface overlay, |BSR| > %g", threshold),
    panel_labels = c(
      "Left Lateral" = "LH lateral",
      "Right Lateral" = "RH lateral",
      "Left Medial" = "LH medial",
      "Right Medial" = "RH medial"
    )
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
  ggplot2::ggsave(
    file.path(output_dir, sprintf("lv%d-design-scores.png", lv)),
    plsrri::plot_scores(result, lv = lv, type = "design", plot_type = "bar"),
    width = 7,
    height = 4,
    dpi = 150
  )
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
  result <- plsrri::run(spec, progress = progress)
  list(manifest = manifest, mask = mask, spec = spec, result = result)
}

main <- function() {
  output_dir <- file.path("artifacts", "sdam-firstlevel-task-pls")
  nperm <- as.integer(Sys.getenv("PLSRRI_SDAM_NPERM", "1000"))
  nboot <- as.integer(Sys.getenv("PLSRRI_SDAM_NBOOT", "500"))

  analysis <- run_sdam_task_pls(nperm = nperm, nboot = nboot)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(analysis$result, file.path(output_dir, "sdam-task-pls-result.rds"))
  utils::write.csv(
    summarise_sdam_result(analysis$result),
    file.path(output_dir, "latent-variable-summary.csv"),
    row.names = FALSE
  )
  save_sdam_plots(analysis$result, output_dir = output_dir, lv = 1L)

  print(summarise_sdam_design(analysis$manifest))
  print(summarise_sdam_result(analysis$result))
  invisible(analysis)
}

if (sys.nframe() == 0L) {
  main()
}
