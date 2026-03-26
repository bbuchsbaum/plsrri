#!/usr/bin/env Rscript

# Generate deterministic parameter-sweep parity fixtures from vendored MATLAB/Octave code.
#
# Run from project root:
#   Rscript tests/testthat/fixtures/matlab/generate_parameter_fixtures.R

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
fixture_path <- file.path(repo_root, "tests", "testthat", "fixtures", "matlab", "parameter_results.rds")

design <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
datamat <- matrix(c(6, 5, 4, 3, 2, 1), nrow = 3, byrow = TRUE)

behav <- matrix(
  c(
    1, 0,
    2, 1,
    3, 2,
    4, 3,
    5, 4,
    6, 5
  ),
  nrow = 6,
  byrow = TRUE
)

dat_corr <- matrix(
  c(
    1, 2, 3, 4,
    2, 3, 4, 5,
    3, 4, 5, 6,
    4, 5, 6, 7,
    5, 6, 7, 8,
    6, 7, 8, 9
  ),
  nrow = 6,
  byrow = TRUE
)

stacked_datamat <- rbind(
  matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10, 2, 1, 0, 1, -1, 2, -2, -1, 0), nrow = 6, byrow = TRUE),
  matrix(c(0, 1, 2, 3, 2, 1, 4, 6, 8, 1, 0, -1, 0, -2, 1, 2, 1, -1), nrow = 6, byrow = TRUE)
)

stacked_behav <- rbind(
  matrix(c(1, 0, 2, 1, 3, 2, 4, 3, 5, 4, 6, 5), nrow = 6, byrow = TRUE),
  matrix(c(2, 1, 3, 1, 4, 2, 5, 3, 6, 5, 8, 8), nrow = 6, byrow = TRUE)
)

tmp_dir <- tempfile("octave-parameter-fixture-")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- file.path(tmp_dir, "out")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
script_path <- file.path(tmp_dir, "run.m")

lines <- c(
  sprintf("addpath(genpath('%s'));", gsub("'", "''", plscmd_dir)),
  sprintf("outdir = '%s';", gsub("'", "''", normalizePath(out_dir, winslash = "/", mustWork = TRUE))),
  sprintf("design = %s;", oct_matrix(design)),
  sprintf("datamat = %s;", oct_matrix(datamat)),
  sprintf("behav = %s;", oct_matrix(behav)),
  sprintf("dat_corr = %s;", oct_matrix(dat_corr)),
  sprintf("stacked_datamat = %s;", oct_matrix(stacked_datamat)),
  sprintf("stacked_behav = %s;", oct_matrix(stacked_behav)),
  "for mode = [0 2 4 6]",
  "  out = rri_xcor(design, datamat, mode);",
  "  dlmwrite(fullfile(outdir, sprintf('xcor_%d.csv', mode)), out, ',', 'precision', 17);",
  "  maps_all = rri_corr_maps(behav, dat_corr, 3, 2, mode);",
  "  maps_sel = rri_corr_maps_notall(behav, dat_corr, 3, [2], mode);",
  "  dlmwrite(fullfile(outdir, sprintf('corr_all_%d.csv', mode)), maps_all, ',', 'precision', 17);",
  "  dlmwrite(fullfile(outdir, sprintf('corr_sel_%d.csv', mode)), maps_sel, ',', 'precision', 17);",
  "  [svd3,~,lst3,~] = rri_get_covcor(3, stacked_datamat, stacked_behav, 2, [3 3], 2, [1 2], 0, mode, [], 0, 0, 1:12, 1:12, []);",
  "  dlmwrite(fullfile(outdir, sprintf('covcor3_%d.csv', mode)), svd3, ',', 'precision', 17);",
  "  dlmwrite(fullfile(outdir, sprintf('covcor3_lst1_%d.csv', mode)), lst3{1}, ',', 'precision', 17);",
  "  dlmwrite(fullfile(outdir, sprintf('covcor3_lst2_%d.csv', mode)), lst3{2}, ',', 'precision', 17);",
  "end",
  "for mct = [0 1 2 3]",
  "  [svd,~,~,smean] = rri_get_covcor(1, stacked_datamat, [], 2, [3 3], 2, [1 2], mct, 0, [], 1, 1, 1:12, [], []);",
  "  dlmwrite(fullfile(outdir, sprintf('covcor1_svd_mct_%d.csv', mct)), svd, ',', 'precision', 17);",
  "  dlmwrite(fullfile(outdir, sprintf('covcor1_smean_mct_%d.csv', mct)), smean, ',', 'precision', 17);",
  "end"
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
    inputs = list(design = design, datamat = datamat),
    expected = lapply(c(0L, 2L, 4L, 6L), function(mode) read_csv_matrix(file.path(out_dir, sprintf("xcor_%d.csv", mode))))
  ),
  corr_maps = list(
    inputs = list(behav = behav, datamat = dat_corr, n_subj = 3L, num_cond = 2L, bscan = 2L),
    expected_all = lapply(c(0L, 2L, 4L, 6L), function(mode) read_csv_matrix(file.path(out_dir, sprintf("corr_all_%d.csv", mode)))),
    expected_sel = lapply(c(0L, 2L, 4L, 6L), function(mode) read_csv_matrix(file.path(out_dir, sprintf("corr_sel_%d.csv", mode))))
  ),
  covcor_method3 = list(
    inputs = list(
      method = 3L,
      stacked_datamat = stacked_datamat,
      stacked_behavdata = stacked_behav,
      num_groups = 2L,
      num_subj_lst = c(3L, 3L),
      num_cond = 2L,
      bscan = c(1L, 2L),
      meancentering_type = 0L
    ),
    expected = lapply(c(0L, 2L, 4L, 6L), function(mode) {
      list(
        datamatsvd = read_csv_matrix(file.path(out_dir, sprintf("covcor3_%d.csv", mode))),
        datamatcorrs_lst = list(
          read_csv_matrix(file.path(out_dir, sprintf("covcor3_lst1_%d.csv", mode))),
          read_csv_matrix(file.path(out_dir, sprintf("covcor3_lst2_%d.csv", mode)))
        )
      )
    })
  ),
  covcor_method1_meancentering = list(
    inputs = list(
      method = 1L,
      stacked_datamat = stacked_datamat,
      num_groups = 2L,
      num_subj_lst = c(3L, 3L),
      num_cond = 2L,
      cormode = 0L
    ),
    expected = lapply(0:3, function(mct) {
      list(
        datamatsvd = read_csv_matrix(file.path(out_dir, sprintf("covcor1_svd_mct_%d.csv", mct))),
        stacked_smeanmat = read_csv_matrix(file.path(out_dir, sprintf("covcor1_smean_mct_%d.csv", mct)))
      )
    })
  )
)

saveRDS(fixtures, fixture_path)
message("Wrote fixture: ", fixture_path)
