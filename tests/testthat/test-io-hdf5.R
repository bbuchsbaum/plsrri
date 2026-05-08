test_that("write_results(format='hdf5') round-trips a basic pls_result", {
  skip_if_not_installed("hdf5r")

  fix <- readRDS(test_path("fixtures", "pls_result_basic.rds"))
  tmp <- withr::local_tempfile(fileext = ".h5")

  expect_invisible(write_results(fix, tmp, format = "hdf5"))
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)

  fix2 <- read_results(tmp)

  expect_s3_class(fix2, "pls_result")
  expect_identical(class(fix), class(fix2))
  expect_equal(fix2$method, fix$method)
  expect_equal(fix2$u, fix$u)
  expect_equal(fix2$s, fix$s)
  expect_equal(fix2$v, fix$v)
  expect_equal(fix2$usc, fix$usc)
  expect_equal(fix2$vsc, fix$vsc)
  expect_equal(fix2$num_subj_lst, fix$num_subj_lst)
  expect_equal(fix2$num_cond, fix$num_cond)
})

test_that("HDF5 round-trip preserves perm_result fields", {
  skip_if_not_installed("hdf5r")

  fix <- readRDS(test_path("fixtures", "pls_result_with_perm.rds"))
  tmp <- withr::local_tempfile(fileext = ".h5")
  write_results(fix, tmp, format = "hdf5")
  fix2 <- read_results(tmp)

  expect_s3_class(fix2$perm_result, "pls_perm_result")
  expect_equal(fix2$perm_result$num_perm, fix$perm_result$num_perm)
  expect_equal(fix2$perm_result$sp, fix$perm_result$sp)
  expect_equal(fix2$perm_result$sprob, fix$perm_result$sprob)
})

test_that("HDF5 round-trip preserves boot_result fields", {
  skip_if_not_installed("hdf5r")

  fix <- readRDS(test_path("fixtures", "pls_result_with_boot.rds"))
  tmp <- withr::local_tempfile(fileext = ".h5")
  write_results(fix, tmp, format = "hdf5")
  fix2 <- read_results(tmp)

  expect_s3_class(fix2$boot_result, "pls_boot_result")
  expect_equal(fix2$boot_result$num_boot, fix$boot_result$num_boot)
  expect_equal(fix2$boot_result$boot_type, fix$boot_result$boot_type)
  expect_equal(fix2$boot_result$clim, fix$boot_result$clim)
  expect_equal(fix2$boot_result$compare_u, fix$boot_result$compare_u)
  expect_equal(fix2$boot_result$u_se, fix$boot_result$u_se)
})

test_that("HDF5 round-trip preserves NeuroVol mask", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("neuroim2")

  fix <- readRDS(test_path("fixtures", "pls_result_full.rds"))
  expect_true(methods::is(fix$mask, "DenseNeuroVol"))

  tmp <- withr::local_tempfile(fileext = ".h5")
  write_results(fix, tmp, format = "hdf5")
  fix2 <- read_results(tmp)

  expect_true(methods::is(fix2$mask, "DenseNeuroVol"))
  expect_equal(as.array(fix2$mask), as.array(fix$mask))
  expect_equal(fix2$mask@space, fix$mask@space)
})

test_that("read_results auto-detects HDF5 vs RDS by magic bytes", {
  skip_if_not_installed("hdf5r")

  fix <- readRDS(test_path("fixtures", "pls_result_basic.rds"))

  rds_path <- withr::local_tempfile(fileext = ".rds")
  h5_path <- withr::local_tempfile(fileext = ".h5")

  saveRDS(fix, rds_path)
  write_results(fix, h5_path, format = "hdf5")

  fix_rds <- read_results(rds_path)
  fix_h5  <- read_results(h5_path)

  expect_s3_class(fix_rds, "pls_result")
  expect_s3_class(fix_h5, "pls_result")
  expect_equal(fix_rds$u, fix_h5$u)
  expect_equal(fix_rds$s, fix_h5$s)
})

test_that("read_results errors clearly on a non-plsrri HDF5 file", {
  skip_if_not_installed("hdf5r")

  tmp <- withr::local_tempfile(fileext = ".h5")
  h5 <- hdf5r::H5File$new(tmp, mode = "w")
  h5$create_dataset("not_a_pls_result", robj = 1:10)
  h5$close_all()

  expect_error(read_results(tmp), "format_version")
})

test_that("read_results errors on a missing file", {
  expect_error(read_results("/no/such/file.h5"), "does not exist")
})

test_that("HDF5 file is self-describing (carries dims and description attrs)", {
  skip_if_not_installed("hdf5r")

  fix <- readRDS(test_path("fixtures", "pls_result_basic.rds"))
  tmp <- withr::local_tempfile(fileext = ".h5")
  write_results(fix, tmp, format = "hdf5")

  h5 <- hdf5r::H5File$new(tmp, mode = "r")
  on.exit(h5$close_all(), add = TRUE)

  expect_equal(hdf5r::h5attr(h5, "format_version"), "plsrri/1.0")
  expect_true("decomposition" %in% names(h5))

  decomp <- h5[["decomposition"]]
  ds_u <- decomp[["u"]]
  attrs <- hdf5r::h5attributes(ds_u)
  expect_true("dims" %in% names(attrs))
  expect_true("description" %in% names(attrs))
  expect_equal(attrs$dims, c("features", "lv"))
})

test_that("perm_test_task_cpp with keep_distribution returns the null matrix", {
  skip_if_not_installed("hdf5r")

  set.seed(123)
  X <- matrix(rnorm(60 * 50), 60, 50)  # 60 rows = 2 groups * 3 cond * 10 subj
  num_subj_lst <- c(10L, 10L)
  num_cond <- 3L
  num_perm <- 50L
  observed_s <- runif(3, 1, 5)

  permsamp <- pls_perm_order(num_subj_lst, num_cond, num_perm)

  res <- perm_test_task_cpp(
    stacked_datamat = X,
    permsamp = permsamp,
    observed_s = observed_s,
    num_groups = 2L,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    meancentering_type = 0L,
    keep_distribution = TRUE
  )

  expect_named(res, c("sp", "perm_singval"))
  expect_length(res$sp, length(observed_s))
  expect_equal(dim(res$perm_singval), c(length(observed_s), num_perm))
  expect_true(all(res$perm_singval >= 0))
})

test_that("pls_analysis(keep_perm_distribution=TRUE) populates perm_singval", {
  skip_if_not_installed("hdf5r")

  set.seed(42)
  X1 <- matrix(rnorm(10 * 3 * 80), 30, 80)
  X2 <- matrix(rnorm(10 * 3 * 80), 30, 80)

  fit <- pls_analysis(
    datamat_lst = list(X1, X2),
    num_subj_lst = c(10L, 10L),
    num_cond = 3L,
    method = 1L,
    num_perm = 30L,
    progress = FALSE,
    keep_perm_distribution = TRUE
  )

  expect_s3_class(fit$perm_result, "pls_perm_result")
  expect_false(is.null(fit$perm_result$perm_singval))
  expect_equal(ncol(fit$perm_result$perm_singval), 30L)

  # And it round-trips through HDF5.
  tmp <- withr::local_tempfile(fileext = ".h5")
  write_results(fit, tmp, format = "hdf5")
  fit2 <- read_results(tmp)
  expect_equal(fit2$perm_result$perm_singval, fit$perm_result$perm_singval)
})
