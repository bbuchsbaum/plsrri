# Octave-backed parity checks against the vendored MATLAB toolbox in PLS/plscmd.
#
# These tests are intentionally narrow and deterministic:
# - They validate core non-random algebraic helpers against MATLAB behavior.
# - They document one known legacy MATLAB edge-case bug (Inf handling in missnk_*).

.find_octave <- function() {
  exe <- Sys.which("octave-cli")
  if (!nzchar(exe)) {
    exe <- Sys.which("octave")
  }
  exe
}

.skip_if_no_octave <- function() {
  exe <- .find_octave()
  if (!nzchar(exe)) {
    testthat::skip("Octave not installed")
  }
}

.skip_if_no_plscmd <- function() {
  plscmd <- file.path(.project_root(), "PLS", "plscmd")
  if (!dir.exists(plscmd)) {
    testthat::skip("Vendored MATLAB PLS toolbox (PLS/plscmd) not available")
  }
}

.project_root <- function() {
  normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
}

.octave_quote <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

.to_octave_scalar <- function(x) {
  if (is.na(x)) {
    return("NaN")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "Inf" else "-Inf")
  }
  sprintf("%.17g", x)
}

.to_octave_matrix <- function(x) {
  stopifnot(is.matrix(x))
  rows <- vapply(seq_len(nrow(x)), function(i) {
    vals <- vapply(x[i, ], .to_octave_scalar, character(1))
    paste(vals, collapse = " ")
  }, character(1))
  paste0("[", paste(rows, collapse = "; "), "]")
}

.read_csv_matrix <- function(path) {
  out <- as.matrix(utils::read.csv(path, header = FALSE, check.names = FALSE))
  storage.mode(out) <- "double"
  dimnames(out) <- NULL
  out
}

.align_by_column_correlation <- function(reference, candidate) {
  stopifnot(is.matrix(reference), is.matrix(candidate))
  stopifnot(ncol(reference) == ncol(candidate))

  if (ncol(reference) == 0L) {
    return(candidate)
  }

  corr_abs <- suppressWarnings(abs(stats::cor(reference, candidate, use = "pairwise.complete.obs")))
  corr_abs[!is.finite(corr_abs)] <- 0
  perm <- integer(ncol(reference))
  used <- rep(FALSE, ncol(candidate))

  for (i in seq_len(ncol(reference))) {
    scores <- corr_abs[i, ]
    scores[used] <- -Inf
    if (all(!is.finite(scores))) {
      j <- which(!used)[1]
    } else {
      j <- which.max(scores)
    }
    perm[i] <- j
    used[j] <- TRUE
  }

  aligned <- candidate[, perm, drop = FALSE]
  signs <- sign(colSums(reference * aligned))
  signs[!is.finite(signs) | signs == 0] <- 1
  aligned <- sweep(aligned, 2, signs, "*")
  aligned
}

.expect_matrix_equivalent <- function(actual,
                                       expected,
                                       tolerance = 1e-10,
                                       align_cols = FALSE) {
  stopifnot(is.matrix(actual), is.matrix(expected))
  stopifnot(identical(dim(actual), dim(expected)))
  dimnames(actual) <- NULL
  dimnames(expected) <- NULL

  if (isTRUE(align_cols) && ncol(actual) > 0L) {
    actual <- .align_by_column_correlation(expected, actual)
  }

  testthat::expect_equal(actual, expected, tolerance = tolerance)
}

.run_octave_script <- function(lines) {
  .skip_if_no_octave()
  .skip_if_no_plscmd()

  exe <- .find_octave()
  plscmd <- normalizePath(file.path(.project_root(), "PLS", "plscmd"), winslash = "/", mustWork = TRUE)

  tmp_dir <- tempfile("octave-parity-")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- file.path(tmp_dir, "out")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  script_path <- file.path(tmp_dir, "run.m")

  prelude <- c(
    sprintf("addpath(genpath('%s'));", .octave_quote(plscmd)),
    sprintf("outdir = '%s';", .octave_quote(normalizePath(out_dir, winslash = "/", mustWork = TRUE)))
  )

  writeLines(c(prelude, lines), script_path, useBytes = TRUE)

  out <- system2(
    exe,
    c("--quiet", "--no-gui", "--no-window-system", script_path),
    stdout = TRUE,
    stderr = TRUE,
    env = c("QT_QPA_PLATFORM=offscreen", "OCTAVE_DISABLE_GUI=1")
  )
  status <- attr(out, "status")

  if (!is.null(status) && status != 0L) {
    stop(
      paste(
        c("Octave script failed:", out),
        collapse = "\n"
      )
    )
  }

  list(out_dir = out_dir, output = out)
}

test_that("Octave rri_task_mean matches pls_task_mean", {
  datamat <- matrix(
    c(
      1, 2, 3, 4,
      4, 5, 6, 7,
      7, 8, 9, 10,
      2, 1, 0, -1,
      -1, -2, -3, -4,
      5, 4, 3, 2
    ),
    nrow = 6,
    byrow = TRUE
  )

  run <- .run_octave_script(c(
    sprintf("datamat = %s;", .to_octave_matrix(datamat)),
    "meanmat = rri_task_mean(datamat, 3);",
    "dlmwrite(fullfile(outdir, 'meanmat.csv'), meanmat, ',', 'precision', 17);"
  ))

  octave_mean <- .read_csv_matrix(file.path(run$out_dir, "meanmat.csv"))
  r_mean <- pls_task_mean(datamat, n_subj = 3L)

  expect_equal(octave_mean, r_mean, tolerance = 1e-12)
})

test_that("Octave rri_xcor matches pls_xcor on finite data for all cormodes", {
  design <- matrix(
    c(
      1, 2, 10,
      2, 1, 10,
      3, 0, 10,
      4, -1, 10,
      5, -2, 10,
      6, -3, 10
    ),
    nrow = 6,
    byrow = TRUE
  )

  datamat <- matrix(
    c(
      2, 0, 1, 5,
      4, 1, 1, 5,
      6, 1, 1, 5,
      8, 2, 1, 5,
      10, 3, 1, 5,
      12, 5, 1, 5
    ),
    nrow = 6,
    byrow = TRUE
  )

  run <- .run_octave_script(c(
    sprintf("design = %s;", .to_octave_matrix(design)),
    sprintf("datamat = %s;", .to_octave_matrix(datamat)),
    "for mode = [0 2 4 6]",
    "  out = rri_xcor(design, datamat, mode);",
    "  fname = sprintf('xcor_%d.csv', mode);",
    "  dlmwrite(fullfile(outdir, fname), out, ',', 'precision', 17);",
    "end"
  ))

  for (mode in c(0L, 2L, 4L, 6L)) {
    octave_xcor <- .read_csv_matrix(file.path(run$out_dir, sprintf("xcor_%d.csv", mode)))
    r_xcor <- pls_xcor(design, datamat, cormode = mode)
    expect_equal(octave_xcor, r_xcor, tolerance = 1e-12)
  }
})

test_that("Octave rri_corr_maps and rri_corr_maps_notall match R", {
  n_subj <- 3L
  num_cond <- 2L
  bscan <- c(2L)

  behav <- matrix(
    c(
      1, 0,
      2, 1,
      3, 1,
      3, 2,
      4, 3,
      6, 5
    ),
    nrow = 6,
    byrow = TRUE
  )

  datamat <- matrix(
    c(
      1, 2, 3, 5,
      2, 4, 6, 8,
      3, 6, 9, 13,
      2, 1, 0, -1,
      3, 1, -1, -2,
      5, 2, -2, -3
    ),
    nrow = 6,
    byrow = TRUE
  )

  run <- .run_octave_script(c(
    sprintf("behav = %s;", .to_octave_matrix(behav)),
    sprintf("datamat = %s;", .to_octave_matrix(datamat)),
    "maps_all = rri_corr_maps(behav, datamat, 3, 2, 0);",
    "maps_sel = rri_corr_maps_notall(behav, datamat, 3, [2], 0);",
    "dlmwrite(fullfile(outdir, 'maps_all.csv'), maps_all, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'maps_sel.csv'), maps_sel, ',', 'precision', 17);"
  ))

  octave_all <- .read_csv_matrix(file.path(run$out_dir, "maps_all.csv"))
  octave_sel <- .read_csv_matrix(file.path(run$out_dir, "maps_sel.csv"))

  r_all <- pls_corr_maps(behav, datamat, n_subj = n_subj, num_cond = num_cond, cormode = 0L)
  r_sel <- pls_corr_maps_notall(behav, datamat, n_subj = n_subj, bscan = bscan, cormode = 0L)

  expect_equal(octave_all, r_all, tolerance = 1e-12)
  expect_equal(octave_sel, r_sel, tolerance = 1e-12)
})

test_that("Octave rri_get_covcor method 1 matches R across meancentering types", {
  stacked_datamat <- rbind(
    matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10, 2, 1, 0, 1, -1, 2, -2, -1, 0), nrow = 6, byrow = TRUE),
    matrix(c(0, 1, 2, 3, 2, 1, 4, 6, 8, 1, 0, -1, 0, -2, 1, 2, 1, -1), nrow = 6, byrow = TRUE)
  )

  run <- .run_octave_script(c(
    sprintf("stacked_datamat = %s;", .to_octave_matrix(stacked_datamat)),
    "num_groups = 2;",
    "num_subj_lst = [3 3];",
    "num_cond = 2;",
    "datamat_reorder = (1:rows(stacked_datamat))';",
    "for mct = [0 1 2 3]",
    "  [svd,~,~,smean] = rri_get_covcor(1, stacked_datamat, [], num_groups, num_subj_lst, num_cond, [1 2], mct, 0, [], 1, 1, datamat_reorder, [], []);",
    "  dlmwrite(fullfile(outdir, sprintf('svd_mct_%d.csv', mct)), svd, ',', 'precision', 17);",
    "  dlmwrite(fullfile(outdir, sprintf('smean_mct_%d.csv', mct)), smean, ',', 'precision', 17);",
    "end"
  ))

  for (mct in 0:3) {
    r_cov <- pls_get_covcor(
      method = 1L,
      stacked_datamat = stacked_datamat,
      num_groups = 2L,
      num_subj_lst = c(3L, 3L),
      num_cond = 2L,
      meancentering_type = as.integer(mct),
      cormode = 0L,
      compute_smeanmat = TRUE
    )

    octave_svd <- .read_csv_matrix(file.path(run$out_dir, sprintf("svd_mct_%d.csv", mct)))
    octave_smean <- .read_csv_matrix(file.path(run$out_dir, sprintf("smean_mct_%d.csv", mct)))

    expect_equal(octave_svd, r_cov$datamatsvd, tolerance = 1e-12)
    expect_equal(octave_smean, r_cov$stacked_smeanmat, tolerance = 1e-12)
  }
})

test_that("Octave rri_get_covcor methods 3 and 4 match R", {
  stacked_datamat <- rbind(
    matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 10, 2, 1, 0, 1, -1, 2, -2, -1, 0), nrow = 6, byrow = TRUE),
    matrix(c(0, 1, 2, 3, 2, 1, 4, 6, 8, 1, 0, -1, 0, -2, 1, 2, 1, -1), nrow = 6, byrow = TRUE)
  )

  stacked_behav <- rbind(
    matrix(c(1, 0, 2, 1, 3, 2, 4, 3, 5, 4, 6, 5), nrow = 6, byrow = TRUE),
    matrix(c(2, 1, 3, 1, 4, 2, 5, 3, 6, 5, 8, 8), nrow = 6, byrow = TRUE)
  )

  run <- .run_octave_script(c(
    sprintf("stacked_datamat = %s;", .to_octave_matrix(stacked_datamat)),
    sprintf("stacked_behav = %s;", .to_octave_matrix(stacked_behav)),
    "num_groups = 2;",
    "num_subj_lst = [3 3];",
    "num_cond = 2;",
    "datamat_reorder = (1:rows(stacked_datamat))';",
    "behav_reorder = (1:rows(stacked_behav))';",
    "[svd3,~,lst3,~] = rri_get_covcor(3, stacked_datamat, stacked_behav, num_groups, num_subj_lst, num_cond, [1 2], 0, 0, [], 0, 0, datamat_reorder, behav_reorder, []);",
    "[svd4,svd4u,lst4,smean4] = rri_get_covcor(4, stacked_datamat, stacked_behav, num_groups, num_subj_lst, num_cond, [1 2], 0, 0, [], 1, 1, datamat_reorder, behav_reorder, datamat_reorder);",
    "dlmwrite(fullfile(outdir, 'svd3.csv'), svd3, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'lst3_1.csv'), lst3{1}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'lst3_2.csv'), lst3{2}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'svd4.csv'), svd4, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'svd4u.csv'), svd4u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'lst4_1.csv'), lst4{1}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'lst4_2.csv'), lst4{2}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'smean4.csv'), smean4, ',', 'precision', 17);"
  ))

  r_m3 <- pls_get_covcor(
    method = 3L,
    stacked_datamat = stacked_datamat,
    stacked_behavdata = stacked_behav,
    num_groups = 2L,
    num_subj_lst = c(3L, 3L),
    num_cond = 2L,
    meancentering_type = 0L,
    cormode = 0L
  )

  r_m4 <- pls_get_covcor(
    method = 4L,
    stacked_datamat = stacked_datamat,
    stacked_behavdata = stacked_behav,
    num_groups = 2L,
    num_subj_lst = c(3L, 3L),
    num_cond = 2L,
    bscan = c(1L, 2L),
    meancentering_type = 0L,
    cormode = 0L,
    compute_smeanmat = TRUE
  )

  expect_equal(.read_csv_matrix(file.path(run$out_dir, "svd3.csv")), r_m3$datamatsvd, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "lst3_1.csv")), r_m3$datamatcorrs_lst[[1]], tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "lst3_2.csv")), r_m3$datamatcorrs_lst[[2]], tolerance = 1e-12)

  expect_equal(.read_csv_matrix(file.path(run$out_dir, "svd4.csv")), r_m4$datamatsvd, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "svd4u.csv")), r_m4$datamatsvd_unnorm, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "lst4_1.csv")), r_m4$datamatcorrs_lst[[1]], tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "lst4_2.csv")), r_m4$datamatcorrs_lst[[2]], tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "smean4.csv")), r_m4$stacked_smeanmat, tolerance = 1e-12)
})

test_that("R keeps Inf handling correct even where MATLAB missnk path degrades", {
  design <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
  dat_nan <- matrix(c(1, 2, 3, NaN, 5, 6), nrow = 3, byrow = TRUE)
  dat_inf <- matrix(c(1, 2, 3, Inf, 5, 6), nrow = 3, byrow = TRUE)

  run <- .run_octave_script(c(
    sprintf("design = %s;", .to_octave_matrix(design)),
    sprintf("dat_nan = %s;", .to_octave_matrix(dat_nan)),
    sprintf("dat_inf = %s;", .to_octave_matrix(dat_inf)),
    "oct_nan = missnk_rri_xcor(design, dat_nan, 0);",
    "oct_inf = missnk_rri_xcor(design, dat_inf, 0);",
    "dlmwrite(fullfile(outdir, 'oct_nan.csv'), oct_nan, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'oct_inf.csv'), oct_inf, ',', 'precision', 17);"
  ))

  oct_nan <- .read_csv_matrix(file.path(run$out_dir, "oct_nan.csv"))
  oct_inf <- .read_csv_matrix(file.path(run$out_dir, "oct_inf.csv"))

  r_nan <- pls_xcor(design, dat_nan, cormode = 0L)
  r_inf <- pls_xcor(design, dat_inf, cormode = 0L)

  # For NaN-missing data, our implementation matches MATLAB missnk behavior.
  expect_equal(r_nan, oct_nan, tolerance = 1e-12)

  # For Inf-missing data, MATLAB's legacy miss* helpers rely on isnan(...) and
  # can propagate non-finite values. We treat non-finite values as missing.
  if (all(is.finite(oct_inf))) {
    testthat::skip("Vendored MATLAB Inf-handling appears fixed; revisit this guard")
  }

  expect_true(any(!is.finite(oct_inf)))
  expect_true(all(is.finite(r_inf)))
  expect_equal(r_inf, r_nan, tolerance = 1e-12)
})

test_that("Octave pls_analysis matches R for non-rotated methods 2, 5, and 6", {
  dat <- matrix(
    c(
      1, 2, 3, 4, 5,
      2, 3, 4, 5, 6,
      3, 4, 5, 6, 7,
      4, 5, 6, 7, 8,
      5, 6, 7, 8, 9,
      6, 7, 8, 9, 10,
      2, 1, 0, -1, -2,
      3, 1, 0, -1, -3,
      4, 2, 1, -1, -4,
      5, 2, 1, -2, -5,
      6, 3, 2, -3, -6,
      7, 4, 3, -4, -7
    ),
    nrow = 12,
    byrow = TRUE
  )

  beh <- matrix(
    c(
      1, 0,
      2, 1,
      3, 1,
      4, 2,
      5, 3,
      6, 5,
      2, 1,
      3, 1,
      4, 2,
      5, 3,
      6, 5,
      8, 8
    ),
    nrow = 12,
    byrow = TRUE
  )

  des2 <- matrix(c(1, -1), nrow = 2, byrow = TRUE)
  des5 <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1), nrow = 4, byrow = TRUE)
  des6 <- matrix(c(1, -1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1), nrow = 6, byrow = TRUE)

  run <- .run_octave_script(c(
    sprintf("dat = %s;", .to_octave_matrix(dat)),
    sprintf("beh = %s;", .to_octave_matrix(beh)),
    sprintf("des2 = %s;", .to_octave_matrix(des2)),
    sprintf("des5 = %s;", .to_octave_matrix(des5)),
    sprintf("des6 = %s;", .to_octave_matrix(des6)),
    "opt2 = struct(); opt2.method = 2; opt2.stacked_designdata = des2; opt2.num_perm = 0; opt2.num_boot = 0;",
    "r2 = pls_analysis({dat}, [6], 2, opt2);",
    "dlmwrite(fullfile(outdir, 'm2_s.csv'), r2.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2_u.csv'), r2.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2_v.csv'), r2.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2_usc.csv'), r2.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2_vsc.csv'), r2.vsc, ',', 'precision', 17);",
    "opt5 = struct(); opt5.method = 5; opt5.stacked_designdata = des5; opt5.stacked_behavdata = beh; opt5.num_perm = 0; opt5.num_boot = 0;",
    "r5 = pls_analysis({dat}, [6], 2, opt5);",
    "dlmwrite(fullfile(outdir, 'm5_s.csv'), r5.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5_u.csv'), r5.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5_v.csv'), r5.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5_usc.csv'), r5.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5_vsc.csv'), r5.vsc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5_lvcorrs.csv'), r5.lvcorrs, ',', 'precision', 17);",
    "opt6 = struct(); opt6.method = 6; opt6.stacked_designdata = des6; opt6.stacked_behavdata = beh; opt6.bscan = [1 2]; opt6.num_perm = 0; opt6.num_boot = 0;",
    "r6 = pls_analysis({dat}, [6], 2, opt6);",
    "dlmwrite(fullfile(outdir, 'm6_s.csv'), r6.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_u.csv'), r6.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_v.csv'), r6.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_usc.csv'), r6.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_vsc.csv'), r6.vsc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_lvcorrs.csv'), r6.lvcorrs, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_TBv_1.csv'), r6.TBv{1}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_TBv_2.csv'), r6.TBv{2}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_TBusc_1.csv'), r6.TBusc{1}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_TBusc_2.csv'), r6.TBusc{2}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_TBvsc_1.csv'), r6.TBvsc{1}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6_TBvsc_2.csv'), r6.TBvsc{2}, ',', 'precision', 17);"
  ))

  r2 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 2L,
    stacked_designdata = des2,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m2_s.csv"))), as.numeric(r2$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2_u.csv")), r2$u, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2_v.csv")), r2$v, tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2_usc.csv")), r2$usc, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2_vsc.csv")), r2$vsc, tolerance = 1e-10, align_cols = TRUE)

  r5 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 5L,
    stacked_behavdata = beh,
    stacked_designdata = des5,
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m5_s.csv"))), as.numeric(r5$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5_u.csv")), r5$u, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5_v.csv")), r5$v, tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5_usc.csv")), r5$usc, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5_vsc.csv")), r5$vsc, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5_lvcorrs.csv")), r5$lvcorrs, tolerance = 1e-10, align_cols = TRUE)

  r6 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 6L,
    stacked_behavdata = beh,
    stacked_designdata = des6,
    bscan = c(1L, 2L),
    num_perm = 0L,
    num_boot = 0L,
    progress = FALSE
  )

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m6_s.csv"))), as.numeric(r6$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_u.csv")), r6$u, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_v.csv")), r6$v, tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_usc.csv")), r6$usc, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_vsc.csv")), r6$vsc, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_lvcorrs.csv")), r6$lvcorrs, tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_TBv_1.csv")), r6$TBv[[1]], tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_TBv_2.csv")), r6$TBv[[2]], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_TBusc_1.csv")), r6$TBusc[[1]], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_TBusc_2.csv")), r6$TBusc[[2]], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_TBvsc_1.csv")), r6$TBvsc[[1]], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6_TBvsc_2.csv")), r6$TBvsc[[2]], tolerance = 1e-10, align_cols = TRUE)
})

test_that("Octave and R method 4 parity holds with fixed permutation/bootstrap orders", {
  dat <- matrix(
    c(
      1, 2, 3, 4, 5,
      2, 3, 4, 5, 6,
      3, 4, 5, 6, 7,
      4, 5, 6, 7, 8,
      5, 6, 7, 8, 9,
      6, 7, 8, 9, 10,
      2, 1, 0, -1, -2,
      3, 1, 0, -1, -3,
      4, 2, 1, -1, -4,
      5, 2, 1, -2, -5,
      6, 3, 2, -3, -6,
      7, 4, 3, -4, -7
    ),
    nrow = 12,
    byrow = TRUE
  )

  beh <- matrix(
    c(
      1, 0,
      2, 1,
      3, 1,
      4, 2,
      5, 3,
      6, 5,
      2, 1,
      3, 1,
      4, 2,
      5, 3,
      6, 5,
      8, 8
    ),
    nrow = 12,
    byrow = TRUE
  )

  Tpermsamp <- cbind(
    1:12,
    c(2:12, 1),
    c(3:12, 1, 2),
    c(12, 1:11)
  )

  Bpermsamp <- cbind(
    c(12:1),
    c(6:12, 1:5),
    c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12),
    1:12
  )

  bootsamp <- cbind(
    c(1:12),
    c(4:12, 1:3),
    c(7:12, 1:6),
    c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  )

  bootsamp_4beh <- cbind(
    c(2:12, 1),
    c(5:12, 1:4),
    c(8:12, 1:7),
    c(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 12)
  )

  run <- .run_octave_script(c(
    sprintf("dat = %s;", .to_octave_matrix(dat)),
    sprintf("beh = %s;", .to_octave_matrix(beh)),
    sprintf("Tperm = %s;", .to_octave_matrix(Tpermsamp)),
    sprintf("Bperm = %s;", .to_octave_matrix(Bpermsamp)),
    sprintf("bootA = %s;", .to_octave_matrix(bootsamp)),
    sprintf("bootB = %s;", .to_octave_matrix(bootsamp_4beh)),
    "opt4 = struct();",
    "opt4.method = 4;",
    "opt4.stacked_behavdata = beh;",
    "opt4.bscan = [1 2];",
    "opt4.num_perm = 4;",
    "opt4.num_boot = 4;",
    "opt4.permsamp = Bperm;",
    "opt4.Tpermsamp = Tperm;",
    "opt4.Bpermsamp = Bperm;",
    "opt4.bootsamp = bootA;",
    "opt4.bootsamp_4beh = bootB;",
    "r4 = pls_analysis({dat}, [6], 2, opt4);",
    "dlmwrite(fullfile(outdir, 'm4_s.csv'), r4.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_u.csv'), r4.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_v.csv'), r4.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_usc.csv'), r4.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_vsc.csv'), r4.vsc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_lvcorrs.csv'), r4.lvcorrs, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_perm_sp.csv'), r4.perm_result.sp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_perm_sprob.csv'), r4.perm_result.sprob, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_perm_permsamp.csv'), r4.perm_result.permsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_perm_Tpermsamp.csv'), r4.perm_result.Tpermsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_boot_compare_u.csv'), r4.boot_result.compare_u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_boot_u_se.csv'), r4.boot_result.u_se, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_boot_bootsamp.csv'), r4.boot_result.bootsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm4_boot_bootsamp_4beh.csv'), r4.boot_result.bootsamp_4beh, ',', 'precision', 17);"
  ))

  r4 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 4L,
    stacked_behavdata = beh,
    bscan = c(1L, 2L),
    num_perm = 4L,
    num_boot = 4L,
    permsamp = Bpermsamp,
    Tpermsamp = Tpermsamp,
    Bpermsamp = Bpermsamp,
    bootsamp = bootsamp,
    bootsamp_4beh = bootsamp_4beh,
    progress = FALSE
  )

  n_keep <- sum(r4$s > 1e-8)
  keep <- seq_len(n_keep)

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m4_s.csv"))), as.numeric(r4$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m4_u.csv"))[, keep, drop = FALSE], r4$u[, keep, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m4_v.csv"))[, keep, drop = FALSE], r4$v[, keep, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m4_usc.csv"))[, keep, drop = FALSE], r4$usc[, keep, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m4_vsc.csv"))[, keep, drop = FALSE], r4$vsc[, keep, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m4_lvcorrs.csv"))[, keep, drop = FALSE], r4$lvcorrs[, keep, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)

  oct_sp <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m4_perm_sp.csv")))
  oct_sprob <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m4_perm_sprob.csv")))
  expect_equal(oct_sp[keep], as.numeric(r4$perm_result$sp[keep]), tolerance = 1e-12)

  # MATLAB normalizes permutation probabilities by (num_perm + 1), while
  # this package uses num_perm for regular permutation tests.
  expect_equal(oct_sprob[keep], oct_sp[keep] / (r4$perm_result$num_perm + 1), tolerance = 1e-12)
  expect_equal(as.numeric(r4$perm_result$sprob[keep]), as.numeric(r4$perm_result$sp[keep]) / r4$perm_result$num_perm, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m4_perm_permsamp.csv")), r4$perm_result$permsamp, tolerance = 0)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m4_perm_Tpermsamp.csv")), r4$perm_result$Tpermsamp, tolerance = 0)

  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m4_boot_compare_u.csv"))[, keep, drop = FALSE], r4$boot_result$compare_u[, keep, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m4_boot_u_se.csv"))[, keep, drop = FALSE], r4$boot_result$u_se[, keep, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m4_boot_bootsamp.csv")), r4$boot_result$bootsamp, tolerance = 0)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m4_boot_bootsamp_4beh.csv")), r4$boot_result$bootsamp_4beh, tolerance = 0)

  expect_equal(r4$perm_result$Tpermsamp, Tpermsamp, tolerance = 0)
  expect_equal(r4$perm_result$Bpermsamp, Bpermsamp, tolerance = 0)
  expect_equal(r4$boot_result$bootsamp, bootsamp, tolerance = 0)
  expect_equal(r4$boot_result$bootsamp_4beh, bootsamp_4beh, tolerance = 0)
})

test_that("Octave and R methods 3 and 5 parity hold with fixed resampling orders", {
  dat <- matrix(
    c(
      1, 2, 3, 4, 5,
      2, 3, 4, 5, 6,
      3, 4, 5, 6, 7,
      4, 5, 6, 7, 8,
      5, 6, 7, 8, 9,
      6, 7, 8, 9, 10,
      2, 1, 0, -1, -2,
      3, 1, 0, -1, -3,
      4, 2, 1, -1, -4,
      5, 2, 1, -2, -5,
      6, 3, 2, -3, -6,
      7, 4, 3, -4, -7
    ),
    nrow = 12,
    byrow = TRUE
  )

  beh <- matrix(
    c(
      1, 0,
      2, 1,
      3, 1,
      4, 2,
      5, 3,
      6, 5,
      2, 1,
      3, 1,
      4, 2,
      5, 3,
      6, 5,
      8, 8
    ),
    nrow = 12,
    byrow = TRUE
  )

  des5 <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1), nrow = 4, byrow = TRUE)

  permsamp <- cbind(
    c(12:1),
    c(3:12, 1:2),
    c(5:12, 1:4),
    c(7:12, 1:6)
  )

  bootsamp <- cbind(
    c(1:12),
    c(2:12, 1),
    c(3:12, 1:2),
    c(4:12, 1:3)
  )

  bootsamp_4beh <- cbind(
    c(12:1),
    c(11:1, 12),
    c(10:1, 12, 11),
    c(9:1, 12, 11, 10)
  )

  run <- .run_octave_script(c(
    sprintf("dat = %s;", .to_octave_matrix(dat)),
    sprintf("beh = %s;", .to_octave_matrix(beh)),
    sprintf("des5 = %s;", .to_octave_matrix(des5)),
    sprintf("permA = %s;", .to_octave_matrix(permsamp)),
    sprintf("bootA = %s;", .to_octave_matrix(bootsamp)),
    sprintf("bootB = %s;", .to_octave_matrix(bootsamp_4beh)),
    "opt3 = struct();",
    "opt3.method = 3;",
    "opt3.stacked_behavdata = beh;",
    "opt3.num_perm = 4;",
    "opt3.num_boot = 4;",
    "opt3.permsamp = permA;",
    "opt3.bootsamp = bootA;",
    "opt3.bootsamp_4beh = bootB;",
    "r3 = pls_analysis({dat}, [6], 2, opt3);",
    "dlmwrite(fullfile(outdir, 'm3_s.csv'), r3.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_u.csv'), r3.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_v.csv'), r3.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_usc.csv'), r3.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_vsc.csv'), r3.vsc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_lvcorrs.csv'), r3.lvcorrs, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_perm_sp.csv'), r3.perm_result.sp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_perm_sprob.csv'), r3.perm_result.sprob, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_perm_permsamp.csv'), r3.perm_result.permsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_boot_compare_u.csv'), r3.boot_result.compare_u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_boot_u_se.csv'), r3.boot_result.u_se, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_boot_bootsamp.csv'), r3.boot_result.bootsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm3_boot_bootsamp_4beh.csv'), r3.boot_result.bootsamp_4beh, ',', 'precision', 17);",
    "opt5 = struct();",
    "opt5.method = 5;",
    "opt5.stacked_behavdata = beh;",
    "opt5.stacked_designdata = des5;",
    "opt5.num_perm = 4;",
    "opt5.num_boot = 4;",
    "opt5.permsamp = permA;",
    "opt5.bootsamp = bootA;",
    "opt5.bootsamp_4beh = bootB;",
    "r5 = pls_analysis({dat}, [6], 2, opt5);",
    "dlmwrite(fullfile(outdir, 'm5r_s.csv'), r5.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_u.csv'), r5.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_v.csv'), r5.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_usc.csv'), r5.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_vsc.csv'), r5.vsc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_lvcorrs.csv'), r5.lvcorrs, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_perm_sp.csv'), r5.perm_result.sp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_perm_sprob.csv'), r5.perm_result.sprob, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_perm_permsamp.csv'), r5.perm_result.permsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_boot_compare_u.csv'), r5.boot_result.compare_u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_boot_u_se.csv'), r5.boot_result.u_se, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_boot_bootsamp.csv'), r5.boot_result.bootsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm5r_boot_bootsamp_4beh.csv'), r5.boot_result.bootsamp_4beh, ',', 'precision', 17);"
  ))

  r3 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 3L,
    stacked_behavdata = beh,
    num_perm = 4L,
    num_boot = 4L,
    permsamp = permsamp,
    bootsamp = bootsamp,
    bootsamp_4beh = bootsamp_4beh,
    progress = FALSE
  )

  keep3 <- seq_len(sum(r3$s > 1e-8))

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m3_s.csv"))), as.numeric(r3$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m3_u.csv"))[, keep3, drop = FALSE], r3$u[, keep3, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m3_v.csv"))[, keep3, drop = FALSE], r3$v[, keep3, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m3_usc.csv"))[, keep3, drop = FALSE], r3$usc[, keep3, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m3_vsc.csv"))[, keep3, drop = FALSE], r3$vsc[, keep3, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m3_lvcorrs.csv"))[, keep3, drop = FALSE], r3$lvcorrs[, keep3, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)

  oct_sp3 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m3_perm_sp.csv")))
  oct_sprob3 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m3_perm_sprob.csv")))
  expect_equal(oct_sp3[keep3], as.numeric(r3$perm_result$sp[keep3]), tolerance = 1e-12)
  expect_equal(oct_sprob3[keep3], oct_sp3[keep3] / (r3$perm_result$num_perm + 1), tolerance = 1e-12)
  expect_equal(as.numeric(r3$perm_result$sprob[keep3]), as.numeric(r3$perm_result$sp[keep3]) / r3$perm_result$num_perm, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m3_perm_permsamp.csv")), r3$perm_result$permsamp, tolerance = 0)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m3_boot_compare_u.csv"))[, keep3, drop = FALSE], r3$boot_result$compare_u[, keep3, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m3_boot_u_se.csv"))[, keep3, drop = FALSE], r3$boot_result$u_se[, keep3, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m3_boot_bootsamp.csv")), r3$boot_result$bootsamp, tolerance = 0)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m3_boot_bootsamp_4beh.csv")), r3$boot_result$bootsamp_4beh, tolerance = 0)

  r5 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 5L,
    stacked_behavdata = beh,
    stacked_designdata = des5,
    num_perm = 4L,
    num_boot = 4L,
    permsamp = permsamp,
    bootsamp = bootsamp,
    bootsamp_4beh = bootsamp_4beh,
    progress = FALSE
  )

  keep5 <- seq_len(sum(r5$s > 1e-8))

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m5r_s.csv"))), as.numeric(r5$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5r_u.csv"))[, keep5, drop = FALSE], r5$u[, keep5, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5r_v.csv"))[, keep5, drop = FALSE], r5$v[, keep5, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5r_usc.csv"))[, keep5, drop = FALSE], r5$usc[, keep5, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5r_vsc.csv"))[, keep5, drop = FALSE], r5$vsc[, keep5, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5r_lvcorrs.csv"))[, keep5, drop = FALSE], r5$lvcorrs[, keep5, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)

  oct_sp5 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m5r_perm_sp.csv")))
  oct_sprob5 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m5r_perm_sprob.csv")))
  expect_equal(oct_sp5[keep5], as.numeric(r5$perm_result$sp[keep5]), tolerance = 1e-12)
  expect_equal(oct_sprob5[keep5], oct_sp5[keep5] / (r5$perm_result$num_perm + 1), tolerance = 1e-12)
  expect_equal(as.numeric(r5$perm_result$sprob[keep5]), as.numeric(r5$perm_result$sp[keep5]) / r5$perm_result$num_perm, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m5r_perm_permsamp.csv")), r5$perm_result$permsamp, tolerance = 0)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5r_boot_compare_u.csv"))[, keep5, drop = FALSE], r5$boot_result$compare_u[, keep5, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m5r_boot_u_se.csv"))[, keep5, drop = FALSE], r5$boot_result$u_se[, keep5, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m5r_boot_bootsamp.csv")), r5$boot_result$bootsamp, tolerance = 0)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m5r_boot_bootsamp_4beh.csv")), r5$boot_result$bootsamp_4beh, tolerance = 0)

  expect_equal(r3$perm_result$permsamp, permsamp, tolerance = 0)
  expect_equal(r3$boot_result$bootsamp, bootsamp, tolerance = 0)
  expect_equal(r3$boot_result$bootsamp_4beh, bootsamp_4beh, tolerance = 0)
  expect_equal(r5$perm_result$permsamp, permsamp, tolerance = 0)
  expect_equal(r5$boot_result$bootsamp, bootsamp, tolerance = 0)
  expect_equal(r5$boot_result$bootsamp_4beh, bootsamp_4beh, tolerance = 0)
})

test_that("Octave and R method 1 parity holds with fixed permutation/bootstrap orders", {
  dat <- matrix(
    c(
      1, 2, 3, 4, 5,
      2, 3, 4, 5, 6,
      3, 4, 5, 6, 7,
      4, 5, 6, 7, 8,
      5, 6, 7, 8, 9,
      6, 7, 8, 9, 10,
      2, 1, 0, -1, -2,
      3, 1, 0, -1, -3,
      4, 2, 1, -1, -4,
      5, 2, 1, -2, -5,
      6, 3, 2, -3, -6,
      7, 4, 3, -4, -7
    ),
    nrow = 12,
    byrow = TRUE
  )

  permsamp <- cbind(
    c(2:12, 1),
    c(3:12, 1:2),
    c(12, 1:11),
    c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)
  )

  bootsamp <- cbind(
    c(1:12),
    c(2:12, 1),
    c(1, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  )

  run <- .run_octave_script(c(
    sprintf("dat = %s;", .to_octave_matrix(dat)),
    sprintf("permA = %s;", .to_octave_matrix(permsamp)),
    sprintf("bootA = %s;", .to_octave_matrix(bootsamp)),
    "opt1 = struct();",
    "opt1.method = 1;",
    "opt1.num_perm = 4;",
    "opt1.num_boot = 4;",
    "opt1.permsamp = permA;",
    "opt1.bootsamp = bootA;",
    "r1 = pls_analysis({dat}, [6], 2, opt1);",
    "dlmwrite(fullfile(outdir, 'm1r_s.csv'), r1.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_u.csv'), r1.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_v.csv'), r1.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_usc.csv'), r1.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_vsc.csv'), r1.vsc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_perm_sp.csv'), r1.perm_result.sp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_perm_sprob.csv'), r1.perm_result.sprob, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_perm_permsamp.csv'), r1.perm_result.permsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_boot_compare_u.csv'), r1.boot_result.compare_u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_boot_u_se.csv'), r1.boot_result.u_se, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm1r_boot_bootsamp.csv'), r1.boot_result.bootsamp, ',', 'precision', 17);"
  ))

  r1 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 1L,
    num_perm = 4L,
    num_boot = 4L,
    permsamp = permsamp,
    bootsamp = bootsamp,
    progress = FALSE
  )

  keep1 <- seq_len(sum(r1$s > 1e-8))

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m1r_s.csv"))), as.numeric(r1$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m1r_u.csv"))[, keep1, drop = FALSE], r1$u[, keep1, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m1r_v.csv"))[, keep1, drop = FALSE], r1$v[, keep1, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m1r_usc.csv"))[, keep1, drop = FALSE], r1$usc[, keep1, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m1r_vsc.csv"))[, keep1, drop = FALSE], r1$vsc[, keep1, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)

  oct_sp1 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m1r_perm_sp.csv")))
  oct_sprob1 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m1r_perm_sprob.csv")))
  expect_equal(oct_sp1[keep1], as.numeric(r1$perm_result$sp[keep1]), tolerance = 1e-12)
  expect_equal(oct_sprob1[keep1], oct_sp1[keep1] / (r1$perm_result$num_perm + 1), tolerance = 1e-12)
  expect_equal(as.numeric(r1$perm_result$sprob[keep1]), as.numeric(r1$perm_result$sp[keep1]) / r1$perm_result$num_perm, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m1r_perm_permsamp.csv")), r1$perm_result$permsamp, tolerance = 0)

  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m1r_boot_compare_u.csv"))[, keep1, drop = FALSE], r1$boot_result$compare_u[, keep1, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m1r_boot_u_se.csv"))[, keep1, drop = FALSE], r1$boot_result$u_se[, keep1, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m1r_boot_bootsamp.csv")), r1$boot_result$bootsamp, tolerance = 0)

  expect_equal(r1$perm_result$permsamp, permsamp, tolerance = 0)
  expect_equal(r1$boot_result$bootsamp, bootsamp, tolerance = 0)
})

test_that("Octave and R methods 2 and 6 parity hold with fixed resampling orders", {
  dat <- matrix(
    c(
      1, 2, 3, 4, 5,
      2, 3, 4, 5, 6,
      3, 4, 5, 6, 7,
      4, 5, 6, 7, 8,
      5, 6, 7, 8, 9,
      6, 7, 8, 9, 10,
      2, 1, 0, -1, -2,
      3, 1, 0, -1, -3,
      4, 2, 1, -1, -4,
      5, 2, 1, -2, -5,
      6, 3, 2, -3, -6,
      7, 4, 3, -4, -7
    ),
    nrow = 12,
    byrow = TRUE
  )

  beh <- matrix(
    c(
      1, 0,
      2, 1,
      3, 1,
      4, 2,
      5, 3,
      6, 5,
      2, 1,
      3, 1,
      4, 2,
      5, 3,
      6, 5,
      8, 8
    ),
    nrow = 12,
    byrow = TRUE
  )

  des2 <- matrix(c(1, -1), nrow = 2, byrow = TRUE)
  des6 <- matrix(c(1, -1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1), nrow = 6, byrow = TRUE)

  permsamp2 <- cbind(
    c(2:12, 1),
    c(3:12, 1:2),
    c(12, 1:11),
    c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)
  )

  bootsamp2 <- cbind(
    c(1:12),
    c(2:12, 1),
    c(1, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  )

  Tpermsamp6 <- cbind(
    c(2:12, 1),
    c(3:12, 1:2),
    c(4:12, 1:3),
    c(12:1)
  )

  Bpermsamp6 <- cbind(
    c(12:1),
    c(11:1, 12),
    c(10:1, 12, 11),
    c(9:1, 12, 11, 10)
  )

  bootsamp6 <- cbind(
    c(1:12),
    c(2:12, 1),
    c(3:12, 1:2),
    c(4:12, 1:3)
  )

  bootsamp6_4beh <- cbind(
    c(12:1),
    c(11:1, 12),
    c(10:1, 12, 11),
    c(9:1, 12, 11, 10)
  )

  run <- .run_octave_script(c(
    sprintf("dat = %s;", .to_octave_matrix(dat)),
    sprintf("beh = %s;", .to_octave_matrix(beh)),
    sprintf("des2 = %s;", .to_octave_matrix(des2)),
    sprintf("des6 = %s;", .to_octave_matrix(des6)),
    sprintf("perm2 = %s;", .to_octave_matrix(permsamp2)),
    sprintf("boot2 = %s;", .to_octave_matrix(bootsamp2)),
    sprintf("Tperm6 = %s;", .to_octave_matrix(Tpermsamp6)),
    sprintf("Bperm6 = %s;", .to_octave_matrix(Bpermsamp6)),
    sprintf("boot6A = %s;", .to_octave_matrix(bootsamp6)),
    sprintf("boot6B = %s;", .to_octave_matrix(bootsamp6_4beh)),
    "opt2 = struct();",
    "opt2.method = 2;",
    "opt2.stacked_designdata = des2;",
    "opt2.num_perm = 4;",
    "opt2.num_boot = 4;",
    "opt2.permsamp = perm2;",
    "opt2.bootsamp = boot2;",
    "r2 = pls_analysis({dat}, [6], 2, opt2);",
    "dlmwrite(fullfile(outdir, 'm2r_s.csv'), r2.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_u.csv'), r2.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_v.csv'), r2.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_usc.csv'), r2.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_vsc.csv'), r2.vsc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_perm_sp.csv'), r2.perm_result.sp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_perm_sprob.csv'), r2.perm_result.sprob, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_perm_permsamp.csv'), r2.perm_result.permsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_boot_compare_u.csv'), r2.boot_result.compare_u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_boot_u_se.csv'), r2.boot_result.u_se, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm2r_boot_bootsamp.csv'), r2.boot_result.bootsamp, ',', 'precision', 17);",
    "opt6 = struct();",
    "opt6.method = 6;",
    "opt6.stacked_designdata = des6;",
    "opt6.stacked_behavdata = beh;",
    "opt6.bscan = [1 2];",
    "opt6.num_perm = 4;",
    "opt6.num_boot = 4;",
    "opt6.permsamp = Bperm6;",
    "opt6.Tpermsamp = Tperm6;",
    "opt6.Bpermsamp = Bperm6;",
    "opt6.bootsamp = boot6A;",
    "opt6.bootsamp_4beh = boot6B;",
    "r6 = pls_analysis({dat}, [6], 2, opt6);",
    "dlmwrite(fullfile(outdir, 'm6r_s.csv'), r6.s, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_u.csv'), r6.u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_v.csv'), r6.v, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_usc.csv'), r6.usc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_vsc.csv'), r6.vsc, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_lvcorrs.csv'), r6.lvcorrs, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_TBv_1.csv'), r6.TBv{1}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_TBv_2.csv'), r6.TBv{2}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_TBusc_1.csv'), r6.TBusc{1}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_TBusc_2.csv'), r6.TBusc{2}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_TBvsc_1.csv'), r6.TBvsc{1}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_TBvsc_2.csv'), r6.TBvsc{2}, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_perm_sp.csv'), r6.perm_result.sp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_perm_sprob.csv'), r6.perm_result.sprob, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_perm_permsamp.csv'), r6.perm_result.permsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_perm_Tpermsamp.csv'), r6.perm_result.Tpermsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_boot_compare_u.csv'), r6.boot_result.compare_u, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_boot_u_se.csv'), r6.boot_result.u_se, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_boot_bootsamp.csv'), r6.boot_result.bootsamp, ',', 'precision', 17);",
    "dlmwrite(fullfile(outdir, 'm6r_boot_bootsamp_4beh.csv'), r6.boot_result.bootsamp_4beh, ',', 'precision', 17);"
  ))

  r2 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 2L,
    stacked_designdata = des2,
    num_perm = 4L,
    num_boot = 4L,
    permsamp = permsamp2,
    bootsamp = bootsamp2,
    progress = FALSE
  )

  keep2 <- seq_len(sum(r2$s > 1e-8))

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m2r_s.csv"))), as.numeric(r2$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2r_u.csv"))[, keep2, drop = FALSE], r2$u[, keep2, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2r_v.csv"))[, keep2, drop = FALSE], r2$v[, keep2, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2r_usc.csv"))[, keep2, drop = FALSE], r2$usc[, keep2, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2r_vsc.csv"))[, keep2, drop = FALSE], r2$vsc[, keep2, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)

  oct_sp2 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m2r_perm_sp.csv")))
  oct_sprob2 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m2r_perm_sprob.csv")))
  expect_equal(oct_sp2[keep2], as.numeric(r2$perm_result$sp[keep2]), tolerance = 1e-12)
  expect_equal(oct_sprob2[keep2], oct_sp2[keep2] / (r2$perm_result$num_perm + 1), tolerance = 1e-12)
  expect_equal(as.numeric(r2$perm_result$sprob[keep2]), as.numeric(r2$perm_result$sp[keep2]) / r2$perm_result$num_perm, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m2r_perm_permsamp.csv")), r2$perm_result$permsamp, tolerance = 0)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2r_boot_compare_u.csv"))[, keep2, drop = FALSE], r2$boot_result$compare_u[, keep2, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m2r_boot_u_se.csv"))[, keep2, drop = FALSE], r2$boot_result$u_se[, keep2, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m2r_boot_bootsamp.csv")), r2$boot_result$bootsamp, tolerance = 0)

  r6 <- pls_analysis(
    datamat_lst = list(dat),
    num_subj_lst = c(6L),
    num_cond = 2L,
    method = 6L,
    stacked_behavdata = beh,
    stacked_designdata = des6,
    bscan = c(1L, 2L),
    num_perm = 4L,
    num_boot = 4L,
    permsamp = Bpermsamp6,
    Tpermsamp = Tpermsamp6,
    Bpermsamp = Bpermsamp6,
    bootsamp = bootsamp6,
    bootsamp_4beh = bootsamp6_4beh,
    progress = FALSE
  )

  keep6 <- seq_len(sum(r6$s > 1e-8))

  expect_equal(as.numeric(.read_csv_matrix(file.path(run$out_dir, "m6r_s.csv"))), as.numeric(r6$s), tolerance = 1e-10)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_u.csv"))[, keep6, drop = FALSE], r6$u[, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_v.csv"))[, keep6, drop = FALSE], r6$v[, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_usc.csv"))[, keep6, drop = FALSE], r6$usc[, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_vsc.csv"))[, keep6, drop = FALSE], r6$vsc[, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_lvcorrs.csv"))[, keep6, drop = FALSE], r6$lvcorrs[, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_TBv_1.csv")), r6$TBv[[1]], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_TBv_2.csv")), r6$TBv[[2]], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_TBusc_1.csv"))[, keep6, drop = FALSE], r6$TBusc[[1]][, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_TBusc_2.csv"))[, keep6, drop = FALSE], r6$TBusc[[2]][, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_TBvsc_1.csv"))[, keep6, drop = FALSE], r6$TBvsc[[1]][, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_TBvsc_2.csv"))[, keep6, drop = FALSE], r6$TBvsc[[2]][, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)

  oct_sp6 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m6r_perm_sp.csv")))
  oct_sprob6 <- as.numeric(.read_csv_matrix(file.path(run$out_dir, "m6r_perm_sprob.csv")))
  expect_equal(oct_sp6[keep6], as.numeric(r6$perm_result$sp[keep6]), tolerance = 1e-12)
  expect_equal(oct_sprob6[keep6], oct_sp6[keep6] / (r6$perm_result$num_perm + 1), tolerance = 1e-12)
  expect_equal(as.numeric(r6$perm_result$sprob[keep6]), as.numeric(r6$perm_result$sp[keep6]) / r6$perm_result$num_perm, tolerance = 1e-12)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m6r_perm_permsamp.csv")), r6$perm_result$permsamp, tolerance = 0)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m6r_perm_Tpermsamp.csv")), r6$perm_result$Tpermsamp, tolerance = 0)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_boot_compare_u.csv"))[, keep6, drop = FALSE], r6$boot_result$compare_u[, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  .expect_matrix_equivalent(.read_csv_matrix(file.path(run$out_dir, "m6r_boot_u_se.csv"))[, keep6, drop = FALSE], r6$boot_result$u_se[, keep6, drop = FALSE], tolerance = 1e-10, align_cols = TRUE)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m6r_boot_bootsamp.csv")), r6$boot_result$bootsamp, tolerance = 0)
  expect_equal(.read_csv_matrix(file.path(run$out_dir, "m6r_boot_bootsamp_4beh.csv")), r6$boot_result$bootsamp_4beh, tolerance = 0)

  expect_equal(r2$perm_result$permsamp, permsamp2, tolerance = 0)
  expect_equal(r2$boot_result$bootsamp, bootsamp2, tolerance = 0)
  expect_equal(r6$perm_result$permsamp, Bpermsamp6, tolerance = 0)
  expect_equal(r6$perm_result$Tpermsamp, Tpermsamp6, tolerance = 0)
  expect_equal(r6$perm_result$Bpermsamp, Bpermsamp6, tolerance = 0)
  expect_equal(r6$boot_result$bootsamp, bootsamp6, tolerance = 0)
  expect_equal(r6$boot_result$bootsamp_4beh, bootsamp6_4beh, tolerance = 0)
})
