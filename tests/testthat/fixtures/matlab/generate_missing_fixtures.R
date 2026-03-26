#!/usr/bin/env Rscript

# Generate deterministic missing-data parity fixtures from vendored MATLAB/Octave code.
#
# Run from project root:
#   Rscript tests/testthat/fixtures/matlab/generate_missing_fixtures.R

oct_scalar <- function(x) {
  if (is.na(x)) return("NaN")
  if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
  sprintf("%.17g", x)
}

oct_matrix <- function(M) {
  stopifnot(is.matrix(M))
  rows <- apply(M, 1, function(r) paste(vapply(r, oct_scalar, character(1)), collapse = " "))
  sprintf("[%s]", paste(rows, collapse = "; "))
}

read_csv_matrix <- function(path) {
  x <- as.matrix(utils::read.csv(path, header = FALSE, check.names = FALSE))
  storage.mode(x) <- "double"
  dimnames(x) <- NULL
  x
}

oct_exe <- Sys.which("octave-cli")
if (!nzchar(oct_exe)) {
  oct_exe <- Sys.which("octave")
}
if (!nzchar(oct_exe)) {
  stop("Octave not found on PATH (checked octave-cli and octave).")
}

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
plscmd_dir <- normalizePath(file.path(repo_root, "PLS", "plscmd"), winslash = "/", mustWork = TRUE)
fixture_path <- file.path(repo_root, "tests", "testthat", "fixtures", "matlab", "missing_results.rds")

design_xcor <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 3, ncol = 2)
dat_xcor <- matrix(c(1, 2, 3, 4, NA, 6), nrow = 3, ncol = 2)

beh <- matrix(
  c(
    1, 0,
    2, 1,
    3, 1,
    4, 2,
    5, 3,
    6, 5
  ),
  nrow = 6,
  byrow = TRUE
)

dat_corr <- matrix(
  c(
    1, 2, 3, 4,
    2, 3, NA, 5,
    3, 4, 5, 6,
    4, 5, 6, 7,
    5, NA, 7, 8,
    6, 7, 8, 9
  ),
  nrow = 6,
  byrow = TRUE
)

tmp_dir <- tempfile("octave-missing-fixture-")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- file.path(tmp_dir, "out")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
script_path <- file.path(tmp_dir, "run.m")

lines <- c(
  sprintf("addpath(genpath('%s'));", gsub("'", "''", plscmd_dir)),
  sprintf("outdir = '%s';", gsub("'", "''", normalizePath(out_dir, winslash = "/", mustWork = TRUE))),
  sprintf("design_x = %s;", oct_matrix(design_xcor)),
  sprintf("dat_x = %s;", oct_matrix(dat_xcor)),
  sprintf("beh = %s;", oct_matrix(beh)),
  sprintf("dat_corr = %s;", oct_matrix(dat_corr)),
  "for mode = [0 2 4 6]",
  "  out = missnk_rri_xcor(design_x, dat_x, mode);",
  "  dlmwrite(fullfile(outdir, sprintf('xcor_%d.csv', mode)), out, ',', 'precision', 17);",
  "end",
  "maps_all = missnk_rri_corr_maps(beh, dat_corr, 3, 2, 0);",
  "maps_sel = missnk_rri_corr_maps_notall(beh, dat_corr, 3, [2], 0);",
  "dlmwrite(fullfile(outdir, 'corr_maps_all.csv'), maps_all, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'corr_maps_sel.csv'), maps_sel, ',', 'precision', 17);",
  "[svd3,~,~,~] = missnk_rri_get_covcor(3, dat_corr, beh, 1, [3], 2, [1 2], 0, 0, [], 0, 0, 1:6, 1:6, []);",
  "dlmwrite(fullfile(outdir, 'covcor_m3.csv'), svd3, ',', 'precision', 17);"
)

writeLines(lines, script_path, useBytes = TRUE)

oct_log <- system2(
  oct_exe,
  c("--quiet", "--no-gui", "--no-window-system", script_path),
  stdout = TRUE,
  stderr = TRUE,
  env = c("QT_QPA_PLATFORM=offscreen", "OCTAVE_DISABLE_GUI=1")
)

status <- attr(oct_log, "status")
if (!is.null(status) && status != 0L) {
  stop(paste(c("Octave fixture generation failed:", oct_log), collapse = "\n"))
}

fixtures <- list(
  xcor = list(
    inputs = list(
      design = design_xcor,
      datamat = dat_xcor
    ),
    expected = list(
      mode0 = read_csv_matrix(file.path(out_dir, "xcor_0.csv")),
      mode2 = read_csv_matrix(file.path(out_dir, "xcor_2.csv")),
      mode4 = read_csv_matrix(file.path(out_dir, "xcor_4.csv")),
      mode6 = read_csv_matrix(file.path(out_dir, "xcor_6.csv"))
    )
  ),
  corr_maps = list(
    inputs = list(
      behav = beh,
      datamat = dat_corr,
      n_subj = 3L,
      num_cond = 2L,
      bscan = 2L
    ),
    expected = list(
      all = read_csv_matrix(file.path(out_dir, "corr_maps_all.csv")),
      sel = read_csv_matrix(file.path(out_dir, "corr_maps_sel.csv"))
    )
  ),
  covcor_method3 = list(
    inputs = list(
      method = 3L,
      stacked_datamat = dat_corr,
      stacked_behavdata = beh,
      num_groups = 1L,
      num_subj_lst = c(3L),
      num_cond = 2L,
      bscan = c(1L, 2L),
      meancentering_type = 0L,
      cormode = 0L
    ),
    expected = list(
      datamatsvd = read_csv_matrix(file.path(out_dir, "covcor_m3.csv"))
    )
  )
)

saveRDS(fixtures, fixture_path)
message("Wrote fixture: ", fixture_path)
