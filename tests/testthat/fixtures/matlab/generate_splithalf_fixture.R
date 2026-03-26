#!/usr/bin/env Rscript

# Generate deterministic split-half parity fixtures from vendored MATLAB/Octave code.
#
# Run from project root:
#   Rscript tests/testthat/fixtures/matlab/generate_splithalf_fixture.R

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
fixture_path <- file.path(repo_root, "tests", "testthat", "fixtures", "matlab", "splithalf_result.rds")

# Deterministic non-rotated task split-half case (method 2, 1 LV).
num_subj <- 6L
num_cond <- 2L
stacked_datamat <- matrix(
  c(
    1, 2, 3, 4,
    2, 3, 4, 5,
    3, 4, 5, 6,
    4, 5, 6, 7,
    5, 6, 7, 8,
    6, 7, 8, 9,
    2, 1, 0, -1,
    3, 1, 0, -1,
    4, 2, 1, -1,
    5, 2, 1, -2,
    6, 3, 2, -3,
    7, 4, 3, -4
  ),
  nrow = 12,
  byrow = TRUE
)
stacked_designdata <- matrix(c(1, -1), nrow = 2, byrow = TRUE)

outer_reorder <- cbind(
  1:12,
  c(7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6),
  c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11)
)

inner_subject_perms <- list(
  list(list(c(1L, 2L, 3L, 4L, 5L, 6L)), list(c(6L, 5L, 4L, 3L, 2L, 1L))),
  list(list(c(2L, 1L, 4L, 3L, 6L, 5L)), list(c(5L, 6L, 3L, 4L, 1L, 2L))),
  list(list(c(3L, 1L, 2L, 6L, 4L, 5L)), list(c(4L, 5L, 6L, 1L, 2L, 3L)))
)

# Build Octave nested-cell literal: { { {[...]}, {[...]} }, ... }
inner_ops <- vapply(seq_along(inner_subject_perms), function(op) {
  splits <- vapply(seq_along(inner_subject_perms[[op]]), function(p) {
    gperm <- inner_subject_perms[[op]][[p]][[1]]
    sprintf("{%s}", oct_matrix(matrix(gperm, nrow = 1)))
  }, character(1))
  sprintf("{%s}", paste(splits, collapse = ","))
}, character(1))
inner_cell <- sprintf("{%s}", paste(inner_ops, collapse = ","))

tmp_dir <- tempfile("octave-splithalf-fixture-")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- file.path(tmp_dir, "out")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
script_path <- file.path(tmp_dir, "run.m")

lines <- c(
  sprintf("addpath(genpath('%s'));", gsub("'", "''", plscmd_dir)),
  sprintf("outdir = '%s';", gsub("'", "''", normalizePath(out_dir, winslash = "/", mustWork = TRUE))),
  sprintf("stacked_datamat = %s;", oct_matrix(stacked_datamat)),
  sprintf("stacked_designdata = %s;", oct_matrix(stacked_designdata)),
  sprintf("outer_reorder = %s;", oct_matrix(outer_reorder)),
  sprintf("inner_perms = %s;", inner_cell),
  "num_groups = 1; num_cond = 2; num_subj_lst = [6];",
  "method = 2; num_split = 2; num_outer = columns(outer_reorder);",
  "meancentering_type = 0; cormode = 0; bscan = [1 2];",
  "num_subj_lst1 = round(num_subj_lst / 2);",
  "num_subj_lst2 = num_subj_lst - num_subj_lst1;",
  "rows1 = []; rows2 = [];",
  "for g = 1:num_groups",
  "  tmp = reshape(1:num_subj_lst(g)*num_cond, num_subj_lst(g), num_cond);",
  "  tmp1 = tmp(1:num_subj_lst1(g),:); tmp1 = tmp1(:);",
  "  tmp2 = tmp(num_subj_lst1(g)+(1:num_subj_lst2(g)),:); tmp2 = tmp2(:);",
  "  offset = sum(num_subj_lst(1:g-1)) * num_cond;",
  "  rows1 = [rows1; offset + tmp1];",
  "  rows2 = [rows2; offset + tmp2];",
  "end",
  "num_lvs = columns(stacked_designdata);",
  "ucorr_distrib = zeros(num_outer, num_lvs);",
  "vcorr_distrib = zeros(num_outer, num_lvs);",
  "for op = 1:num_outer",
  "  datamat_reorder = outer_reorder(:, op);",
  "  [datamatsvd_op,~,~,~] = missnk_rri_get_covcor(method, stacked_datamat, [], num_groups, num_subj_lst, num_cond, bscan, meancentering_type, cormode, [], 0, 0, datamat_reorder, [], []);",
  "  v_op = stacked_designdata;",
  "  crossblock = transpose(normalize(stacked_designdata)) * datamatsvd_op;",
  "  u_op = transpose(crossblock);",
  "  for p = 1:num_split",
  "    gperm = inner_perms{op}{p}{1};",
  "    in_reorder = [];",
  "    for cond = 1:num_cond",
  "      in_reorder = [in_reorder; num_subj_lst(1)*(cond-1) + gperm(:)];",
  "    end",
  "    datamat_reorder1 = datamat_reorder(in_reorder(rows1));",
  "    datamat_reorder2 = datamat_reorder(in_reorder(rows2));",
  "    datamatsvd_p1 = missnk_rri_get_covcor(method, stacked_datamat, [], num_groups, num_subj_lst1, num_cond, bscan, meancentering_type, cormode, [], 0, 0, datamat_reorder1, [], []);",
  "    datamatsvd_p2 = missnk_rri_get_covcor(method, stacked_datamat, [], num_groups, num_subj_lst2, num_cond, bscan, meancentering_type, cormode, [], 0, 0, datamat_reorder2, [], []);",
  "    u_p1 = transpose(datamatsvd_p1) * v_op;",
  "    u_p2 = transpose(datamatsvd_p2) * v_op;",
  "    v_p1 = datamatsvd_p1 * u_op;",
  "    v_p2 = datamatsvd_p2 * u_op;",
  "    for lv = 1:num_lvs",
  "      ucorr_distrib(op,lv) = ucorr_distrib(op,lv) + rri_xcor(u_p1(:,lv), u_p2(:,lv), cormode);",
  "      vcorr_distrib(op,lv) = vcorr_distrib(op,lv) + rri_xcor(v_p1(:,lv), v_p2(:,lv), cormode);",
  "    end",
  "  end",
  "end",
  "ucorr_distrib = ucorr_distrib ./ num_split;",
  "vcorr_distrib = vcorr_distrib ./ num_split;",
  "orig_ucorr = ucorr_distrib(1,:);",
  "orig_vcorr = vcorr_distrib(1,:);",
  "ucorr_prob = zeros(1, num_lvs);",
  "vcorr_prob = zeros(1, num_lvs);",
  "for lv = 1:num_lvs",
  "  ucorr_prob(lv) = sum(ucorr_distrib(2:end,lv) > ucorr_distrib(1,lv)) ./ (num_outer - 1);",
  "  vcorr_prob(lv) = sum(vcorr_distrib(2:end,lv) > vcorr_distrib(1,lv)) ./ (num_outer - 1);",
  "end",
  "clim = 95;",
  "ucorr_ll = percentile(ucorr_distrib(2:end,:), 100-clim);",
  "ucorr_ul = percentile(ucorr_distrib(2:end,:), clim);",
  "vcorr_ll = percentile(vcorr_distrib(2:end,:), 100-clim);",
  "vcorr_ul = percentile(vcorr_distrib(2:end,:), clim);",
  "dlmwrite(fullfile(outdir, 'orig_ucorr.csv'), orig_ucorr, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'orig_vcorr.csv'), orig_vcorr, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'ucorr_prob.csv'), ucorr_prob, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'vcorr_prob.csv'), vcorr_prob, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'ucorr_ll.csv'), ucorr_ll, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'ucorr_ul.csv'), ucorr_ul, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'vcorr_ll.csv'), vcorr_ll, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'vcorr_ul.csv'), vcorr_ul, ',', 'precision', 17);"
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

expected <- list(
  orig_ucorr = as.numeric(read_csv_matrix(file.path(out_dir, "orig_ucorr.csv"))),
  orig_vcorr = as.numeric(read_csv_matrix(file.path(out_dir, "orig_vcorr.csv"))),
  ucorr_prob = as.numeric(read_csv_matrix(file.path(out_dir, "ucorr_prob.csv"))),
  vcorr_prob = as.numeric(read_csv_matrix(file.path(out_dir, "vcorr_prob.csv"))),
  ucorr_ll = as.numeric(read_csv_matrix(file.path(out_dir, "ucorr_ll.csv"))),
  ucorr_ul = as.numeric(read_csv_matrix(file.path(out_dir, "ucorr_ul.csv"))),
  vcorr_ll = as.numeric(read_csv_matrix(file.path(out_dir, "vcorr_ll.csv"))),
  vcorr_ul = as.numeric(read_csv_matrix(file.path(out_dir, "vcorr_ul.csv")))
)

fixtures <- list(
  list(
    args = list(
      stacked_datamat = stacked_datamat,
      stacked_designdata = stacked_designdata,
      num_groups = 1L,
      num_subj_lst = c(num_subj),
      num_cond = num_cond,
      method = 2L,
      num_split = 2L,
      num_outer_perm = 3L,
      bscan = c(1L, 2L),
      meancentering_type = 0L,
      cormode = 0L,
      is_struct = FALSE,
      outer_reorder = outer_reorder,
      inner_subject_perms = inner_subject_perms
    ),
    expected = expected
  )
)

saveRDS(fixtures, fixture_path)
message("Wrote fixture: ", fixture_path)

# Optional local sanity check against current R implementation.
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(repo_root, quiet = TRUE)
  fx <- fixtures[[1]]
  check <- do.call(pls_splithalf_test, c(fx$args, list(progress = FALSE)))
  for (nm in names(fx$expected)) {
    delta <- suppressWarnings(max(abs(check[[nm]] - fx$expected[[nm]]), na.rm = TRUE))
    if (!is.finite(delta)) delta <- 0
    if (delta > 1e-8) {
      stop(sprintf("Sanity check mismatch for %s: max abs diff = %.6g", nm, delta))
    }
  }
  message("Sanity check passed (R matches generated Octave fixture).")
}
