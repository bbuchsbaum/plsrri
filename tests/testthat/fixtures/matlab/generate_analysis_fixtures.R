#!/usr/bin/env Rscript

# Generate deterministic pls_analysis parity fixtures from vendored MATLAB/Octave code.
#
# Run from project root:
#   Rscript tests/testthat/fixtures/matlab/generate_analysis_fixtures.R

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

read_csv_vector <- function(path) {
  as.numeric(read_csv_matrix(path))
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
fixture_path <- file.path(repo_root, "tests", "testthat", "fixtures", "matlab", "analysis_results.rds")

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

des6 <- matrix(c(1, -1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1), nrow = 6, byrow = TRUE)
des2 <- matrix(c(1, -1), nrow = 2, byrow = TRUE)
des5 <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1), nrow = 4, byrow = TRUE)

dat_ssb1 <- matrix(
  c(
    1, 2, 3, 4, 5,
    2, 3, 4, 5, 6,
    3, 4, 5, 6, 7,
    4, 5, 6, 7, 8,
    5, 6, 7, 8, 9,
    6, 7, 8, 9, 10
  ),
  nrow = 6,
  byrow = TRUE
)

dat_ssb2 <- matrix(
  c(
    2, 1, 0, -1, -2,
    3, 1, 0, -1, -3,
    4, 2, 1, -1, -4,
    5, 2, 1, -2, -5,
    6, 3, 2, -3, -6
  ),
  nrow = 5,
  byrow = TRUE
)

beh_ssb <- matrix(
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
    6, 5
  ),
  nrow = 11,
  byrow = TRUE
)

num_subj_ssb_1 <- c(2, 3, 1)
num_subj_ssb_2 <- c(1, 2, 2)

dat_ssb_mb <- matrix(
  c(
    1, 2, 3, 4, 5,
    2, 3, 4, 5, 6,
    3, 4, 5, 6, 7,
    4, 5, 6, 7, 8,
    5, 6, 7, 8, 9,
    6, 7, 8, 9, 10,
    7, 8, 9, 10, 11,
    8, 9, 10, 11, 12,
    9, 10, 11, 12, 13,
    10, 11, 12, 13, 14
  ),
  nrow = 10,
  byrow = TRUE
)

beh_ssb_mb <- matrix(
  c(
    1, 0,
    2, 1,
    3, 1,
    4, 2,
    5, 3,
    6, 5,
    7, 8,
    8, 13,
    9, 21,
    10, 34
  ),
  nrow = 10,
  byrow = TRUE
)

num_subj_ssb_mb <- c(3, 4, 3)

permsamp_ssb <- cbind(
  c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
  c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9),
  c(3, 1, 2, 7, 4, 5, 6, 10, 8, 9)
)

bootsamp_ssb <- cbind(
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  c(1, 2, 2, 4, 5, 6, 7, 8, 9, 10),
  c(1, 3, 3, 4, 5, 6, 7, 8, 10, 10)
)

Bpermsamp_ssb <- cbind(
  c(10, 9, 8, 4, 5, 6, 7, 3, 2, 1),
  c(8, 9, 10, 4, 5, 6, 7, 1, 2, 3),
  c(2, 3, 1, 4, 5, 6, 7, 9, 10, 8)
)

bootsamp_ssb_4beh <- cbind(
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  c(1, 1, 2, 4, 5, 6, 7, 8, 9, 10),
  c(1, 2, 3, 4, 5, 6, 7, 10, 10, 9)
)

permsamp1 <- cbind(
  c(2:12, 1),
  c(3:12, 1:2),
  c(12, 1:11),
  c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)
)

bootsamp1 <- cbind(
  c(1:12),
  c(2:12, 1),
  c(1, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
)

permsamp3 <- cbind(
  c(12:1),
  c(3:12, 1:2),
  c(5:12, 1:4),
  c(7:12, 1:6)
)

bootsamp3 <- cbind(
  c(1:12),
  c(2:12, 1),
  c(3:12, 1:2),
  c(4:12, 1:3)
)

bootsamp3_4beh <- cbind(
  c(12:1),
  c(11:1, 12),
  c(10:1, 12, 11),
  c(9:1, 12, 11, 10)
)

Tpermsamp4 <- cbind(
  1:12,
  c(2:12, 1),
  c(3:12, 1, 2),
  c(12, 1:11)
)

Bpermsamp4 <- cbind(
  c(12:1),
  c(6:12, 1:5),
  c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12),
  1:12
)

bootsamp4 <- cbind(
  c(1:12),
  c(4:12, 1:3),
  c(7:12, 1:6),
  c(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
)

bootsamp4_4beh <- cbind(
  c(2:12, 1),
  c(5:12, 1:4),
  c(8:12, 1:7),
  c(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 12)
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

tmp_dir <- tempfile("octave-analysis-fixture-")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- file.path(tmp_dir, "out")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
script_path <- file.path(tmp_dir, "run.m")

lines <- c(
  sprintf("addpath(genpath('%s'));", gsub("'", "''", plscmd_dir)),
  sprintf("outdir = '%s';", gsub("'", "''", normalizePath(out_dir, winslash = "/", mustWork = TRUE))),
  sprintf("dat = %s;", oct_matrix(dat)),
  sprintf("beh = %s;", oct_matrix(beh)),
  sprintf("des6 = %s;", oct_matrix(des6)),
  sprintf("des2 = %s;", oct_matrix(des2)),
  sprintf("des5 = %s;", oct_matrix(des5)),
  sprintf("dat_ssb1 = %s;", oct_matrix(dat_ssb1)),
  sprintf("dat_ssb2 = %s;", oct_matrix(dat_ssb2)),
  sprintf("beh_ssb = %s;", oct_matrix(beh_ssb)),
  sprintf("nssb1 = %s;", oct_matrix(matrix(num_subj_ssb_1, nrow = 1))),
  sprintf("nssb2 = %s;", oct_matrix(matrix(num_subj_ssb_2, nrow = 1))),
  sprintf("dat_ssb_mb = %s;", oct_matrix(dat_ssb_mb)),
  sprintf("beh_ssb_mb = %s;", oct_matrix(beh_ssb_mb)),
  sprintf("nssb_mb = %s;", oct_matrix(matrix(num_subj_ssb_mb, nrow = 1))),
  sprintf("perm_ssb = %s;", oct_matrix(permsamp_ssb)),
  sprintf("boot_ssb = %s;", oct_matrix(bootsamp_ssb)),
  sprintf("bperm_ssb = %s;", oct_matrix(Bpermsamp_ssb)),
  sprintf("bootb_ssb = %s;", oct_matrix(bootsamp_ssb_4beh)),
  sprintf("perm1 = %s;", oct_matrix(permsamp1)),
  sprintf("boot1 = %s;", oct_matrix(bootsamp1)),
  sprintf("perm3 = %s;", oct_matrix(permsamp3)),
  sprintf("boot3 = %s;", oct_matrix(bootsamp3)),
  sprintf("boot3b = %s;", oct_matrix(bootsamp3_4beh)),
  sprintf("Tperm4 = %s;", oct_matrix(Tpermsamp4)),
  sprintf("Bperm4 = %s;", oct_matrix(Bpermsamp4)),
  sprintf("boot4 = %s;", oct_matrix(bootsamp4)),
  sprintf("boot4b = %s;", oct_matrix(bootsamp4_4beh)),
  sprintf("Tperm6 = %s;", oct_matrix(Tpermsamp6)),
  sprintf("Bperm6 = %s;", oct_matrix(Bpermsamp6)),
  sprintf("boot6 = %s;", oct_matrix(bootsamp6)),
  sprintf("boot6b = %s;", oct_matrix(bootsamp6_4beh)),
  "opt1 = struct();",
  "opt1.method = 1; opt1.num_perm = 4; opt1.num_boot = 4; opt1.permsamp = perm1; opt1.bootsamp = boot1;",
  "r1 = pls_analysis({dat}, [6], 2, opt1);",
  "dlmwrite(fullfile(outdir, 'm1_s.csv'), r1.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_u.csv'), r1.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_v.csv'), r1.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_usc.csv'), r1.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_vsc.csv'), r1.vsc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_perm_sp.csv'), r1.perm_result.sp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_perm_permsamp.csv'), r1.perm_result.permsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_boot_compare_u.csv'), r1.boot_result.compare_u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_boot_u_se.csv'), r1.boot_result.u_se, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1_boot_bootsamp.csv'), r1.boot_result.bootsamp, ',', 'precision', 17);",
  "opt2 = struct();",
  "opt2.method = 2; opt2.stacked_designdata = des2; opt2.num_perm = 4; opt2.num_boot = 4; opt2.permsamp = perm1; opt2.bootsamp = boot1;",
  "r2 = pls_analysis({dat}, [6], 2, opt2);",
  "dlmwrite(fullfile(outdir, 'm2_s.csv'), r2.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_u.csv'), r2.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_v.csv'), r2.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_usc.csv'), r2.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_vsc.csv'), r2.vsc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_perm_sp.csv'), r2.perm_result.sp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_perm_permsamp.csv'), r2.perm_result.permsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_boot_compare_u.csv'), r2.boot_result.compare_u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_boot_u_se.csv'), r2.boot_result.u_se, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm2_boot_bootsamp.csv'), r2.boot_result.bootsamp, ',', 'precision', 17);",
  "opt3 = struct();",
  "opt3.method = 3; opt3.stacked_behavdata = beh; opt3.num_perm = 4; opt3.num_boot = 4; opt3.permsamp = perm3; opt3.bootsamp = boot3; opt3.bootsamp_4beh = boot3b;",
  "r3 = pls_analysis({dat}, [6], 2, opt3);",
  "dlmwrite(fullfile(outdir, 'm3_s.csv'), r3.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_u.csv'), r3.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_v.csv'), r3.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_usc.csv'), r3.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_vsc.csv'), r3.vsc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_lvcorrs.csv'), r3.lvcorrs, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_perm_sp.csv'), r3.perm_result.sp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_perm_permsamp.csv'), r3.perm_result.permsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_boot_compare_u.csv'), r3.boot_result.compare_u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_boot_u_se.csv'), r3.boot_result.u_se, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_boot_bootsamp.csv'), r3.boot_result.bootsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm3_boot_bootsamp_4beh.csv'), r3.boot_result.bootsamp_4beh, ',', 'precision', 17);",
  "opt5 = struct();",
  "opt5.method = 5; opt5.stacked_designdata = des5; opt5.stacked_behavdata = beh; opt5.num_perm = 4; opt5.num_boot = 4; opt5.permsamp = perm3; opt5.bootsamp = boot3; opt5.bootsamp_4beh = boot3b;",
  "r5 = pls_analysis({dat}, [6], 2, opt5);",
  "dlmwrite(fullfile(outdir, 'm5_s.csv'), r5.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_u.csv'), r5.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_v.csv'), r5.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_usc.csv'), r5.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_vsc.csv'), r5.vsc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_lvcorrs.csv'), r5.lvcorrs, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_perm_sp.csv'), r5.perm_result.sp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_perm_permsamp.csv'), r5.perm_result.permsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_boot_compare_u.csv'), r5.boot_result.compare_u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_boot_u_se.csv'), r5.boot_result.u_se, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_boot_bootsamp.csv'), r5.boot_result.bootsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm5_boot_bootsamp_4beh.csv'), r5.boot_result.bootsamp_4beh, ',', 'precision', 17);",
  "opt4 = struct();",
  "opt4.method = 4; opt4.stacked_behavdata = beh; opt4.bscan = [1 2]; opt4.num_perm = 4; opt4.num_boot = 4; opt4.permsamp = Bperm4; opt4.Tpermsamp = Tperm4; opt4.Bpermsamp = Bperm4; opt4.bootsamp = boot4; opt4.bootsamp_4beh = boot4b;",
  "r4 = pls_analysis({dat}, [6], 2, opt4);",
  "dlmwrite(fullfile(outdir, 'm4_s.csv'), r4.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_u.csv'), r4.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_v.csv'), r4.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_usc.csv'), r4.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_vsc.csv'), r4.vsc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_lvcorrs.csv'), r4.lvcorrs, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_perm_sp.csv'), r4.perm_result.sp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_perm_permsamp.csv'), r4.perm_result.permsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_perm_Tpermsamp.csv'), r4.perm_result.Tpermsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_boot_compare_u.csv'), r4.boot_result.compare_u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_boot_u_se.csv'), r4.boot_result.u_se, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_boot_bootsamp.csv'), r4.boot_result.bootsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4_boot_bootsamp_4beh.csv'), r4.boot_result.bootsamp_4beh, ',', 'precision', 17);",
  "opt6 = struct();",
  "opt6.method = 6; opt6.stacked_designdata = des6; opt6.stacked_behavdata = beh; opt6.bscan = [1 2]; opt6.num_perm = 4; opt6.num_boot = 4; opt6.permsamp = Bperm6; opt6.Tpermsamp = Tperm6; opt6.Bpermsamp = Bperm6; opt6.bootsamp = boot6; opt6.bootsamp_4beh = boot6b;",
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
  "dlmwrite(fullfile(outdir, 'm6_TBvsc_2.csv'), r6.TBvsc{2}, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm6_perm_sp.csv'), r6.perm_result.sp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm6_perm_permsamp.csv'), r6.perm_result.permsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm6_perm_Tpermsamp.csv'), r6.perm_result.Tpermsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm6_boot_compare_u.csv'), r6.boot_result.compare_u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm6_boot_u_se.csv'), r6.boot_result.u_se, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm6_boot_bootsamp.csv'), r6.boot_result.bootsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm6_boot_bootsamp_4beh.csv'), r6.boot_result.bootsamp_4beh, ',', 'precision', 17);",
  "opt1ssb = struct();",
  "opt1ssb.method = 1; opt1ssb.num_perm = 0; opt1ssb.num_boot = 0;",
  "r1ssb = pls_analysis({dat_ssb1, dat_ssb2}, {nssb1, nssb2}, 3, opt1ssb);",
  "dlmwrite(fullfile(outdir, 'm1ssb_s.csv'), r1ssb.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssb_u.csv'), r1ssb.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssb_v.csv'), r1ssb.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssb_usc.csv'), r1ssb.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssb_vsc.csv'), r1ssb.vsc, ',', 'precision', 17);",
  "opt4ssb = struct();",
  "opt4ssb.method = 4; opt4ssb.stacked_behavdata = beh_ssb_mb; opt4ssb.bscan = [1 3]; opt4ssb.num_perm = 0; opt4ssb.num_boot = 0;",
  "r4ssb = pls_analysis({dat_ssb_mb}, {nssb_mb}, 3, opt4ssb);",
  "dlmwrite(fullfile(outdir, 'm4ssb_s.csv'), r4ssb.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssb_u.csv'), r4ssb.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssb_v.csv'), r4ssb.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssb_usc.csv'), r4ssb.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssb_vsc.csv'), r4ssb.vsc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssb_lvcorrs.csv'), r4ssb.lvcorrs, ',', 'precision', 17);",
  "opt1ssbr = struct();",
  "opt1ssbr.method = 1; opt1ssbr.num_perm = 3; opt1ssbr.num_boot = 3; opt1ssbr.permsamp = perm_ssb; opt1ssbr.bootsamp = boot_ssb;",
  "r1ssbr = pls_analysis({dat_ssb_mb}, {nssb_mb}, 3, opt1ssbr);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_s.csv'), r1ssbr.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_u.csv'), r1ssbr.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_v.csv'), r1ssbr.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_usc.csv'), r1ssbr.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_vsc.csv'), r1ssbr.vsc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_perm_sp.csv'), r1ssbr.perm_result.sp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_perm_permsamp.csv'), r1ssbr.perm_result.permsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_boot_compare_u.csv'), r1ssbr.boot_result.compare_u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_boot_u_se.csv'), r1ssbr.boot_result.u_se, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm1ssbr_boot_bootsamp.csv'), r1ssbr.boot_result.bootsamp, ',', 'precision', 17);",
  "opt4ssbr = struct();",
  "opt4ssbr.method = 4; opt4ssbr.stacked_behavdata = beh_ssb_mb; opt4ssbr.bscan = [1 3]; opt4ssbr.num_perm = 3; opt4ssbr.num_boot = 3; opt4ssbr.permsamp = bperm_ssb; opt4ssbr.Tpermsamp = perm_ssb; opt4ssbr.Bpermsamp = bperm_ssb; opt4ssbr.bootsamp = boot_ssb; opt4ssbr.bootsamp_4beh = bootb_ssb;",
  "r4ssbr = pls_analysis({dat_ssb_mb}, {nssb_mb}, 3, opt4ssbr);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_s.csv'), r4ssbr.s, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_u.csv'), r4ssbr.u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_v.csv'), r4ssbr.v, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_usc.csv'), r4ssbr.usc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_vsc.csv'), r4ssbr.vsc, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_lvcorrs.csv'), r4ssbr.lvcorrs, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_perm_sp.csv'), r4ssbr.perm_result.sp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_perm_permsamp.csv'), r4ssbr.perm_result.permsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_perm_Tpermsamp.csv'), r4ssbr.perm_result.Tpermsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_boot_compare_u.csv'), r4ssbr.boot_result.compare_u, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_boot_u_se.csv'), r4ssbr.boot_result.u_se, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_boot_bootsamp.csv'), r4ssbr.boot_result.bootsamp, ',', 'precision', 17);",
  "dlmwrite(fullfile(outdir, 'm4ssbr_boot_bootsamp_4beh.csv'), r4ssbr.boot_result.bootsamp_4beh, ',', 'precision', 17);"
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
  method1 = list(
    args = list(
      datamat_lst = list(dat),
      num_subj_lst = c(6L),
      num_cond = 2L,
      method = 1L,
      num_perm = 4L,
      num_boot = 4L,
      permsamp = permsamp1,
      bootsamp = bootsamp1
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m1_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m1_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m1_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m1_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m1_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m1_vsc.csv"))
      ),
      perm_result = list(
        sp = read_csv_vector(file.path(out_dir, "m1_perm_sp.csv")),
        permsamp = read_csv_matrix(file.path(out_dir, "m1_perm_permsamp.csv"))
      ),
      boot_result = list(
        compare_u = read_csv_matrix(file.path(out_dir, "m1_boot_compare_u.csv")),
        u_se = read_csv_matrix(file.path(out_dir, "m1_boot_u_se.csv")),
        bootsamp = read_csv_matrix(file.path(out_dir, "m1_boot_bootsamp.csv"))
      )
    )
  ),
  method2 = list(
    args = list(
      datamat_lst = list(dat),
      num_subj_lst = c(6L),
      num_cond = 2L,
      method = 2L,
      stacked_designdata = des2,
      num_perm = 4L,
      num_boot = 4L,
      permsamp = permsamp1,
      bootsamp = bootsamp1
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m2_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m2_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m2_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m2_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m2_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m2_vsc.csv"))
      ),
      perm_result = list(
        sp = read_csv_vector(file.path(out_dir, "m2_perm_sp.csv")),
        permsamp = read_csv_matrix(file.path(out_dir, "m2_perm_permsamp.csv"))
      ),
      boot_result = list(
        compare_u = read_csv_matrix(file.path(out_dir, "m2_boot_compare_u.csv")),
        u_se = read_csv_matrix(file.path(out_dir, "m2_boot_u_se.csv")),
        bootsamp = read_csv_matrix(file.path(out_dir, "m2_boot_bootsamp.csv"))
      )
    )
  ),
  method3 = list(
    args = list(
      datamat_lst = list(dat),
      num_subj_lst = c(6L),
      num_cond = 2L,
      method = 3L,
      stacked_behavdata = beh,
      num_perm = 4L,
      num_boot = 4L,
      permsamp = permsamp3,
      bootsamp = bootsamp3,
      bootsamp_4beh = bootsamp3_4beh
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m3_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m3_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m3_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m3_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m3_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m3_vsc.csv")),
        lvcorrs = read_csv_matrix(file.path(out_dir, "m3_lvcorrs.csv"))
      ),
      perm_result = list(
        sp = read_csv_vector(file.path(out_dir, "m3_perm_sp.csv")),
        permsamp = read_csv_matrix(file.path(out_dir, "m3_perm_permsamp.csv"))
      ),
      boot_result = list(
        compare_u = read_csv_matrix(file.path(out_dir, "m3_boot_compare_u.csv")),
        u_se = read_csv_matrix(file.path(out_dir, "m3_boot_u_se.csv")),
        bootsamp = read_csv_matrix(file.path(out_dir, "m3_boot_bootsamp.csv")),
        bootsamp_4beh = read_csv_matrix(file.path(out_dir, "m3_boot_bootsamp_4beh.csv"))
      )
    )
  ),
  method5 = list(
    args = list(
      datamat_lst = list(dat),
      num_subj_lst = c(6L),
      num_cond = 2L,
      method = 5L,
      stacked_behavdata = beh,
      stacked_designdata = des5,
      num_perm = 4L,
      num_boot = 4L,
      permsamp = permsamp3,
      bootsamp = bootsamp3,
      bootsamp_4beh = bootsamp3_4beh
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m5_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m5_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m5_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m5_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m5_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m5_vsc.csv")),
        lvcorrs = read_csv_matrix(file.path(out_dir, "m5_lvcorrs.csv"))
      ),
      perm_result = list(
        sp = read_csv_vector(file.path(out_dir, "m5_perm_sp.csv")),
        permsamp = read_csv_matrix(file.path(out_dir, "m5_perm_permsamp.csv"))
      ),
      boot_result = list(
        compare_u = read_csv_matrix(file.path(out_dir, "m5_boot_compare_u.csv")),
        u_se = read_csv_matrix(file.path(out_dir, "m5_boot_u_se.csv")),
        bootsamp = read_csv_matrix(file.path(out_dir, "m5_boot_bootsamp.csv")),
        bootsamp_4beh = read_csv_matrix(file.path(out_dir, "m5_boot_bootsamp_4beh.csv"))
      )
    )
  ),
  method4 = list(
    args = list(
      datamat_lst = list(dat),
      num_subj_lst = c(6L),
      num_cond = 2L,
      method = 4L,
      stacked_behavdata = beh,
      bscan = c(1L, 2L),
      num_perm = 4L,
      num_boot = 4L,
      permsamp = Bpermsamp4,
      Tpermsamp = Tpermsamp4,
      Bpermsamp = Bpermsamp4,
      bootsamp = bootsamp4,
      bootsamp_4beh = bootsamp4_4beh
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m4_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m4_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m4_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m4_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m4_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m4_vsc.csv")),
        lvcorrs = read_csv_matrix(file.path(out_dir, "m4_lvcorrs.csv"))
      ),
      perm_result = list(
        sp = read_csv_vector(file.path(out_dir, "m4_perm_sp.csv")),
        permsamp = read_csv_matrix(file.path(out_dir, "m4_perm_permsamp.csv")),
        Tpermsamp = read_csv_matrix(file.path(out_dir, "m4_perm_Tpermsamp.csv"))
      ),
      boot_result = list(
        compare_u = read_csv_matrix(file.path(out_dir, "m4_boot_compare_u.csv")),
        u_se = read_csv_matrix(file.path(out_dir, "m4_boot_u_se.csv")),
        bootsamp = read_csv_matrix(file.path(out_dir, "m4_boot_bootsamp.csv")),
        bootsamp_4beh = read_csv_matrix(file.path(out_dir, "m4_boot_bootsamp_4beh.csv"))
      )
    )
  ),
  method6 = list(
    args = list(
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
      bootsamp_4beh = bootsamp6_4beh
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m6_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m6_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m6_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m6_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m6_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m6_vsc.csv")),
        lvcorrs = read_csv_matrix(file.path(out_dir, "m6_lvcorrs.csv")),
        TBv = list(
          read_csv_matrix(file.path(out_dir, "m6_TBv_1.csv")),
          read_csv_matrix(file.path(out_dir, "m6_TBv_2.csv"))
        ),
        TBusc = list(
          read_csv_matrix(file.path(out_dir, "m6_TBusc_1.csv")),
          read_csv_matrix(file.path(out_dir, "m6_TBusc_2.csv"))
        ),
        TBvsc = list(
          read_csv_matrix(file.path(out_dir, "m6_TBvsc_1.csv")),
          read_csv_matrix(file.path(out_dir, "m6_TBvsc_2.csv"))
        )
      ),
      perm_result = list(
        sp = read_csv_vector(file.path(out_dir, "m6_perm_sp.csv")),
        permsamp = read_csv_matrix(file.path(out_dir, "m6_perm_permsamp.csv")),
        Tpermsamp = read_csv_matrix(file.path(out_dir, "m6_perm_Tpermsamp.csv"))
      ),
      boot_result = list(
        compare_u = read_csv_matrix(file.path(out_dir, "m6_boot_compare_u.csv")),
        u_se = read_csv_matrix(file.path(out_dir, "m6_boot_u_se.csv")),
        bootsamp = read_csv_matrix(file.path(out_dir, "m6_boot_bootsamp.csv")),
        bootsamp_4beh = read_csv_matrix(file.path(out_dir, "m6_boot_bootsamp_4beh.csv"))
      )
    )
  ),
  method1_ssb = list(
    args = list(
      datamat_lst = list(dat_ssb1, dat_ssb2),
      num_subj_lst = list(as.integer(num_subj_ssb_1), as.integer(num_subj_ssb_2)),
      num_cond = 3L,
      method = 1L,
      num_perm = 0L,
      num_boot = 0L
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m1ssb_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m1ssb_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m1ssb_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m1ssb_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m1ssb_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m1ssb_vsc.csv"))
      ),
      perm_result = NULL,
      boot_result = NULL
    )
  ),
  method4_ssb = list(
    args = list(
      datamat_lst = list(dat_ssb_mb),
      num_subj_lst = list(as.integer(num_subj_ssb_mb)),
      num_cond = 3L,
      method = 4L,
      stacked_behavdata = beh_ssb_mb,
      bscan = c(1L, 3L),
      num_perm = 0L,
      num_boot = 0L
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m4ssb_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m4ssb_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m4ssb_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m4ssb_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m4ssb_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m4ssb_vsc.csv")),
        lvcorrs = read_csv_matrix(file.path(out_dir, "m4ssb_lvcorrs.csv"))
      ),
      perm_result = NULL,
      boot_result = NULL
    )
  ),
  method1_ssb_resamp = list(
    args = list(
      datamat_lst = list(dat_ssb_mb),
      num_subj_lst = list(as.integer(num_subj_ssb_mb)),
      num_cond = 3L,
      method = 1L,
      num_perm = 3L,
      num_boot = 3L,
      permsamp = permsamp_ssb,
      bootsamp = bootsamp_ssb
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m1ssbr_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m1ssbr_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m1ssbr_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m1ssbr_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m1ssbr_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m1ssbr_vsc.csv"))
      ),
      perm_result = list(
        sp = read_csv_vector(file.path(out_dir, "m1ssbr_perm_sp.csv")),
        permsamp = read_csv_matrix(file.path(out_dir, "m1ssbr_perm_permsamp.csv"))
      ),
      boot_result = list(
        compare_u = read_csv_matrix(file.path(out_dir, "m1ssbr_boot_compare_u.csv")),
        u_se = read_csv_matrix(file.path(out_dir, "m1ssbr_boot_u_se.csv")),
        bootsamp = read_csv_matrix(file.path(out_dir, "m1ssbr_boot_bootsamp.csv"))
      )
    )
  ),
  method4_ssb_resamp = list(
    args = list(
      datamat_lst = list(dat_ssb_mb),
      num_subj_lst = list(as.integer(num_subj_ssb_mb)),
      num_cond = 3L,
      method = 4L,
      stacked_behavdata = beh_ssb_mb,
      bscan = c(1L, 3L),
      num_perm = 3L,
      num_boot = 3L,
      permsamp = Bpermsamp_ssb,
      Tpermsamp = permsamp_ssb,
      Bpermsamp = Bpermsamp_ssb,
      bootsamp = bootsamp_ssb,
      bootsamp_4beh = bootsamp_ssb_4beh
    ),
    expected = list(
      keep = read_csv_vector(file.path(out_dir, "m4ssbr_s.csv")) > 1e-8,
      result = list(
        s = read_csv_vector(file.path(out_dir, "m4ssbr_s.csv")),
        u = read_csv_matrix(file.path(out_dir, "m4ssbr_u.csv")),
        v = read_csv_matrix(file.path(out_dir, "m4ssbr_v.csv")),
        usc = read_csv_matrix(file.path(out_dir, "m4ssbr_usc.csv")),
        vsc = read_csv_matrix(file.path(out_dir, "m4ssbr_vsc.csv")),
        lvcorrs = read_csv_matrix(file.path(out_dir, "m4ssbr_lvcorrs.csv"))
      ),
      perm_result = list(
        sp = read_csv_vector(file.path(out_dir, "m4ssbr_perm_sp.csv")),
        permsamp = read_csv_matrix(file.path(out_dir, "m4ssbr_perm_permsamp.csv")),
        Tpermsamp = read_csv_matrix(file.path(out_dir, "m4ssbr_perm_Tpermsamp.csv"))
      ),
      boot_result = list(
        compare_u = read_csv_matrix(file.path(out_dir, "m4ssbr_boot_compare_u.csv")),
        u_se = read_csv_matrix(file.path(out_dir, "m4ssbr_boot_u_se.csv")),
        bootsamp = read_csv_matrix(file.path(out_dir, "m4ssbr_boot_bootsamp.csv")),
        bootsamp_4beh = read_csv_matrix(file.path(out_dir, "m4ssbr_boot_bootsamp_4beh.csv"))
      )
    )
  )
)

saveRDS(fixtures, fixture_path)
message("Wrote fixture: ", fixture_path)
