# Tests for MVA abstraction layer (method protocol, registry, accessors)

test_that("method registry registers and retrieves PLS methods", {
  # All 6 PLS methods should be registered
  methods <- list_methods()
  expect_length(methods, 6)

  expect_true("pls_task" %in% names(methods))
  expect_true("pls_behavior" %in% names(methods))
  expect_true("pls_multiblock" %in% names(methods))
  expect_true("pls_task_nonrotated" %in% names(methods))
  expect_true("pls_behavior_nonrotated" %in% names(methods))
  expect_true("pls_multiblock_nonrotated" %in% names(methods))
})

test_that("method registry resolves aliases", {
  # String aliases

  expect_equal(get_method("task")$name, "pls_task")
  expect_equal(get_method("behavior")$name, "pls_behavior")
  expect_equal(get_method("multiblock")$name, "pls_multiblock")

  # Integer string aliases
  expect_equal(get_method("1")$name, "pls_task")
  expect_equal(get_method("3")$name, "pls_behavior")
  expect_equal(get_method("6")$name, "pls_multiblock_nonrotated")

  # Direct R6 passthrough
  m <- get_method("pls_task")
  expect_identical(get_method(m), m)

  # Unknown method
  expect_error(get_method("nonexistent"), "Unknown analysis method")
})

test_that("MvaMethod capabilities are correct for PLS variants", {
  task <- get_method("pls_task")
  expect_false(task$capabilities$needs_behavior)
  expect_false(task$capabilities$needs_design)
  expect_equal(task$capabilities$component_name, "LV")
  expect_equal(task$family, "pls")

  behav <- get_method("pls_behavior")
  expect_true(behav$capabilities$needs_behavior)
  expect_false(behav$capabilities$needs_design)

  nonrot <- get_method("pls_task_nonrotated")
  expect_false(nonrot$capabilities$needs_behavior)
  expect_true(nonrot$capabilities$needs_design)

  behav_nonrot <- get_method("pls_behavior_nonrotated")
  expect_true(behav_nonrot$capabilities$needs_behavior)
  expect_true(behav_nonrot$capabilities$needs_design)
})

test_that("list_methods filters by family", {
  pls_methods <- list_methods(family = "pls")
  expect_length(pls_methods, 6)

  other <- list_methods(family = "cpca")
  expect_length(other, 0)
})

test_that("MvaMethod ui_config returns expected fields", {
  m <- get_method("pls_task")
  cfg <- m$ui_config()
  expect_true("analysis_stages" %in% names(cfg))
  expect_true("component_name" %in% names(cfg))
  expect_equal(cfg$component_name, "LV")
})

test_that("new_mva_decomposition creates valid object", {
  fw <- matrix(rnorm(100), 50, 2)
  dw <- matrix(rnorm(6), 3, 2)
  imp <- c(2.5, 1.3)

  d <- new_mva_decomposition(
    feature_weights = fw,
    design_weights = dw,
    importance = imp,
    method = "test"
  )

  expect_s3_class(d, "mva_decomposition")
  expect_equal(d$feature_weights, fw)
  expect_equal(d$design_weights, dw)
  expect_equal(d$importance, imp)
  expect_equal(d$method, "test")
  expect_null(d$scores_feature)
})

test_that("new_mva_result creates dual-class object", {
  d <- new_mva_decomposition(
    feature_weights = matrix(1:10, 5, 2),
    importance = c(3, 1),
    method = "pls_task"
  )

  res <- new_mva_result(d, method = "pls_task")
  expect_s3_class(res, "mva_result")
  expect_s3_class(res, "mva_pls_task")
  expect_equal(res$method, "pls_task")
})

test_that("pls_result_to_mva_result preserves all fields", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  d2 <- matrix(rnorm(54 * 30), 54, 30)
  pls_res <- quick_pls(list(d1, d2), c(20, 18), 3,
                        nperm = 5, nboot = 5, progress = FALSE)

  mva_res <- pls_result_to_mva_result(pls_res)

  # Structure
  expect_s3_class(mva_res, "mva_result")
  expect_s3_class(mva_res$decomposition, "mva_decomposition")

  # Numerical identity
  expect_equal(mva_res$decomposition$feature_weights, pls_res$u)
  expect_equal(mva_res$decomposition$design_weights, pls_res$v)
  expect_equal(mva_res$decomposition$importance, pls_res$s)
  expect_equal(mva_res$decomposition$scores_feature, pls_res$usc)
  expect_equal(mva_res$decomposition$scores_design, pls_res$vsc)

  # Inference preserved
  expect_equal(mva_res$perm_result$sprob, pls_res$perm_result$sprob)
  expect_equal(mva_res$boot_result$compare_u, pls_res$boot_result$compare_u)

  # Extra fields preserved
  expect_equal(mva_res$decomposition$extra$pls_method_int, pls_res$method)
  expect_equal(mva_res$decomposition$extra$num_cond, pls_res$num_cond)
})

test_that("mva_result_to_pls_result round-trips correctly", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  d2 <- matrix(rnorm(54 * 30), 54, 30)
  pls_res <- quick_pls(list(d1, d2), c(20, 18), 3, progress = FALSE)

  mva_res <- pls_result_to_mva_result(pls_res)
  pls_back <- mva_result_to_pls_result(mva_res)

  expect_s3_class(pls_back, "pls_result")
  expect_identical(pls_res$u, pls_back$u)
  expect_identical(pls_res$s, pls_back$s)
  expect_identical(pls_res$v, pls_back$v)
  expect_identical(pls_res$usc, pls_back$usc)
  expect_identical(pls_res$vsc, pls_back$vsc)
})

# --- Generic Accessors on mva_result ---

test_that("feature_weights works on mva_result", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  d2 <- matrix(rnorm(54 * 30), 54, 30)
  pls_res <- quick_pls(list(d1, d2), c(20, 18), 3, progress = FALSE)
  mva_res <- pls_result_to_mva_result(pls_res)

  fw_all <- feature_weights(mva_res)
  expect_equal(dim(fw_all), dim(pls_res$u))
  expect_equal(fw_all, pls_res$u)

  fw1 <- feature_weights(mva_res, k = 1)
  expect_equal(ncol(fw1), 1)
  expect_equal(as.numeric(fw1), pls_res$u[, 1])
})

test_that("feature_weights works on pls_result via alias", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  pls_res <- quick_pls(list(d1), c(20), 3, progress = FALSE)

  fw <- feature_weights(pls_res, k = 1)
  sal <- salience(pls_res, lv = 1)
  expect_equal(fw, sal)
})

test_that("importance works on both result types", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  pls_res <- quick_pls(list(d1), c(20), 3, progress = FALSE)
  mva_res <- pls_result_to_mva_result(pls_res)

  expect_equal(importance(mva_res), singular_values(pls_res))
  expect_equal(importance(pls_res), singular_values(pls_res))

  imp_norm <- importance(mva_res, normalize = TRUE)
  sv_norm <- singular_values(pls_res, normalize = TRUE)
  expect_equal(as.numeric(imp_norm), as.numeric(sv_norm))
})

test_that("stability works on both result types", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  pls_res <- quick_pls(list(d1), c(20), 3, nboot = 10, progress = FALSE)
  mva_res <- pls_result_to_mva_result(pls_res)

  stab_mva <- stability(mva_res, k = 1)
  bsr_pls <- bsr(pls_res, lv = 1)
  expect_equal(stab_mva, bsr_pls)

  # With threshold
  stab_thr <- stability(mva_res, k = 1, threshold = 2)
  bsr_thr <- bsr(pls_res, lv = 1, threshold = 2)
  expect_equal(stab_thr, bsr_thr)
})

test_that("n_components works on both result types", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  pls_res <- quick_pls(list(d1), c(20), 3, progress = FALSE)
  mva_res <- pls_result_to_mva_result(pls_res)

  expect_equal(n_components(mva_res), n_lv(pls_res))
  expect_equal(n_components(pls_res), n_lv(pls_res))
})

test_that("scores works on mva_result", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  pls_res <- quick_pls(list(d1), c(20), 3, progress = FALSE)
  mva_res <- pls_result_to_mva_result(pls_res)

  sc_feat <- scores(mva_res, type = "feature")
  expect_equal(sc_feat, pls_res$usc)

  sc_design <- scores(mva_res, type = "design")
  expect_equal(sc_design, pls_res$vsc)

  # "brain" alias
  sc_brain <- scores(mva_res, type = "brain")
  expect_equal(sc_brain, pls_res$usc)
})

test_that("significance works on mva_result", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  pls_res <- quick_pls(list(d1), c(20), 3, nperm = 10, progress = FALSE)
  mva_res <- pls_result_to_mva_result(pls_res)

  sig_mva <- significance(mva_res)
  sig_pls <- significance(pls_res)
  expect_equal(as.numeric(sig_mva), as.numeric(sig_pls))
})

test_that("confidence works on mva_result", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  pls_res <- quick_pls(list(d1), c(20), 3, nboot = 10, progress = FALSE)
  mva_res <- pls_result_to_mva_result(pls_res)

  ci_mva <- confidence(mva_res, what = "feature_weights", lv = 1)
  ci_pls <- confidence(pls_res, what = "salience", lv = 1)
  expect_equal(ci_mva$lower, ci_pls$lower)
  expect_equal(ci_mva$upper, ci_pls$upper)
})

test_that("n_features works on mva_result", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  pls_res <- quick_pls(list(d1), c(20), 3, progress = FALSE)
  mva_res <- pls_result_to_mva_result(pls_res)

  expect_equal(n_features(mva_res), 30)
  expect_equal(n_features(pls_res), 30)
})

# --- PlsMethod$fit() via MvaMethod protocol ---

test_that("PlsMethod$fit() produces identical results to direct pls_analysis", {
  set.seed(42)
  d1 <- matrix(rnorm(60 * 30), 60, 30)
  d2 <- matrix(rnorm(54 * 30), 54, 30)

  # Direct path
  pls_res <- pls_analysis(
    datamat_lst = list(d1, d2),
    num_subj_lst = c(20, 18),
    num_cond = 3,
    method = 1L,
    progress = FALSE
  )

  # Via MvaMethod protocol
  m <- get_method("pls_task")
  spec <- pls_spec()
  spec$datamat_lst <- list(d1, d2)
  spec$num_subj_lst <- c(20, 18)
  spec$num_cond <- 3L
  mva_res <- m$fit(spec, progress = FALSE)

  expect_equal(mva_res$decomposition$feature_weights, pls_res$u)
  expect_equal(mva_res$decomposition$importance, pls_res$s)
  expect_equal(mva_res$decomposition$design_weights, pls_res$v)
})

# --- Custom Method Subclass ---

test_that("custom MvaMethod subclass can be created and registered", {
  # Minimal custom method
  TestMethod <- R6::R6Class("TestMethod",
    inherit = MvaMethod,
    public = list(
      initialize = function() {
        super$initialize(
          name = "test_method",
          label = "Test Method",
          family = "test",
          capabilities = list(component_name = "TC")
        )
      },
      fit = function(spec, progress = TRUE) {
        X <- do.call(rbind, spec$datamat_lst)
        sv <- svd(X, nu = 2, nv = 2)
        decomp <- new_mva_decomposition(
          feature_weights = sv$v,
          importance = sv$d[1:2],
          scores_feature = X %*% sv$v,
          method = self$name
        )
        new_mva_result(decomp, method = self$name)
      }
    )
  )

  register_method("test_method", TestMethod$new(), aliases = c("test"))

  m <- get_method("test")
  expect_equal(m$name, "test_method")
  expect_equal(m$capabilities$component_name, "TC")

  # Fit it
  X <- matrix(rnorm(100), 20, 5)
  spec <- pls_spec()
  spec$datamat_lst <- list(X)
  spec$num_subj_lst <- 20L
  spec$num_cond <- 1L

  res <- m$fit(spec, progress = FALSE)
  expect_s3_class(res, "mva_result")
  expect_equal(n_components(res), 2)
  expect_equal(nrow(feature_weights(res)), 5)

  # Clean up
  .mva_registry$methods[["test_method"]] <- NULL
  .mva_registry$aliases[["test"]] <- NULL
})
