# Tests for fixture integrity and factory functions
#
# These tests verify that mock fixtures are structurally correct
# and can be used with pls_result accessor functions.

describe("Pre-built fixtures", {

  it("pls_result_basic loads and has correct class", {
    result <- load_fixture("pls_result_basic")

    expect_s3_class(result, "pls_result")
    expect_s3_class(result, "pls_task")
    expect_null(result$boot_result)
    expect_null(result$perm_result)
  })

  it("pls_result_with_boot has bootstrap result", {
    result <- load_fixture("pls_result_with_boot")

    expect_s3_class(result, "pls_result")
    expect_false(is.null(result$boot_result))
    expect_s3_class(result$boot_result, "pls_boot_result")
    expect_null(result$perm_result)

    # Check bootstrap components exist
    expect_false(is.null(result$boot_result$compare_u))
    expect_false(is.null(result$boot_result$u_se))
    expect_equal(result$boot_result$num_boot, 100)
  })

  it("pls_result_with_perm has permutation result", {
    result <- load_fixture("pls_result_with_perm")

    expect_s3_class(result, "pls_result")
    expect_null(result$boot_result)
    expect_false(is.null(result$perm_result))
    expect_s3_class(result$perm_result, "pls_perm_result")

    # Check permutation components exist
    expect_false(is.null(result$perm_result$sprob))
    expect_equal(result$perm_result$num_perm, 100)
  })

  it("pls_result_full has all components", {
    result <- load_fixture("pls_result_full")

    expect_s3_class(result, "pls_result")
    expect_false(is.null(result$boot_result))
    expect_false(is.null(result$perm_result))
    expect_false(is.null(result$mask))
  })

  it("fixtures have consistent dimensions", {
    basic <- load_fixture("pls_result_basic")
    full <- load_fixture("pls_result_full")

    # Same default dimensions
    expect_equal(dim(basic$u), dim(full$u))
    expect_equal(length(basic$s), length(full$s))
    expect_equal(dim(basic$v), dim(full$v))
  })

})

describe("Factory functions", {

  it("make_mock_pls_result creates valid object", {
    result <- make_mock_pls_result()

    expect_s3_class(result, "pls_result")
    expect_s3_class(result, "pls_task")
  })

  it("make_mock_pls_result respects dimension parameters", {
    result <- make_mock_pls_result(n_voxels = 100, n_lv = 5, n_obs = 20)

    expect_equal(nrow(result$u), 100)
    expect_equal(ncol(result$u), 5)
    expect_equal(length(result$s), 5)
    expect_equal(nrow(result$v), 20)
    expect_equal(ncol(result$v), 5)
  })

  it("make_mock_pls_result include_boot creates boot_result", {
    result <- make_mock_pls_result(include_boot = TRUE)

    expect_false(is.null(result$boot_result))
    expect_s3_class(result$boot_result, "pls_boot_result")

    # Bootstrap components have correct dimensions
    expect_equal(dim(result$boot_result$compare_u), dim(result$u))
    expect_equal(dim(result$boot_result$u_se), dim(result$u))
  })

  it("make_mock_pls_result include_perm creates perm_result", {
    result <- make_mock_pls_result(include_perm = TRUE)

    expect_false(is.null(result$perm_result))
    expect_s3_class(result$perm_result, "pls_perm_result")

    # Permutation components match n_lv
    expect_equal(length(result$perm_result$sprob), length(result$s))
  })

  it("make_mock_pls_result include_mask creates mask", {
    result <- make_mock_pls_result(include_mask = TRUE)

    expect_false(is.null(result$mask))
  })

  it("make_mock_pls_result is reproducible", {
    result1 <- make_mock_pls_result()
    result2 <- make_mock_pls_result()

    # Same seed inside function = identical results
    expect_equal(result1$u, result2$u)
    expect_equal(result1$s, result2$s)
    expect_equal(result1$v, result2$v)
  })

  it("make_mock_spec creates valid pls_spec", {
    spec <- make_mock_spec()

    expect_s3_class(spec, "pls_spec")
    expect_equal(length(spec$datamat_lst), 1)
    expect_equal(spec$num_cond, 3)
  })

  it("make_mock_spec respects parameters", {
    spec <- make_mock_spec(n_groups = 2, n_subj = 5, n_cond = 4, n_voxels = 200)

    expect_equal(length(spec$datamat_lst), 2)
    expect_equal(spec$num_cond, 4)
    expect_equal(ncol(spec$datamat_lst[[1]]), 200)
    expect_equal(nrow(spec$datamat_lst[[1]]), 5 * 4)  # n_subj * n_cond
  })

  it("make_mock_mask creates volumetric data", {
    mask <- make_mock_mask()

    expect_true(is.array(mask))
    expect_equal(length(dim(mask)), 3)
    expect_equal(dim(mask), c(10, 10, 10))

    # Approximately half the voxels are "brain"
    n_brain <- sum(mask > 0)
    expect_true(n_brain > 400 && n_brain < 600)
  })

})

describe("Edge cases", {

  it("make_single_lv_result creates 1-LV result", {
    result <- make_single_lv_result()

    expect_equal(length(result$s), 1)
    expect_equal(ncol(result$u), 1)
    expect_equal(ncol(result$v), 1)

    # Should still have boot and perm
    expect_false(is.null(result$boot_result))
    expect_false(is.null(result$perm_result))
  })

  it("make_many_lv_result creates 10+ LV result", {
    result <- make_many_lv_result()

    expect_gte(length(result$s), 10)
    expect_gte(ncol(result$u), 10)

    # Should still have boot and perm
    expect_false(is.null(result$boot_result))
    expect_false(is.null(result$perm_result))
  })

})

describe("Fixture compatibility with accessors", {

  it("salience() works with fixtures", {
    result <- load_fixture("pls_result_basic")

    sal <- salience(result)
    expect_equal(dim(sal), dim(result$u))
  })

  it("bsr() works with bootstrap fixture", {
    result <- load_fixture("pls_result_with_boot")

    bsr_vals <- bsr(result)
    expect_equal(dim(bsr_vals), dim(result$u))
  })

  it("scores() works with fixtures", {
    result <- load_fixture("pls_result_basic")

    brain_sc <- scores(result, type = "brain")
    expect_equal(nrow(brain_sc), nrow(result$usc))

    design_sc <- scores(result, type = "design")
    expect_equal(nrow(design_sc), nrow(result$vsc))
  })

  it("significance() works with permutation fixture", {
    result <- load_fixture("pls_result_with_perm")

    pvals <- significance(result)
    expect_equal(length(pvals), length(result$s))
    expect_true(all(pvals >= 0 & pvals <= 1))
  })

  it("singular_values() works with fixtures", {
    result <- load_fixture("pls_result_basic")

    sv <- singular_values(result)
    expect_equal(length(sv), length(result$s))
    expect_true(all(sv >= 0))
  })

  it("n_lv() and n_features() work with fixtures", {
    result <- load_fixture("pls_result_basic")

    expect_equal(n_lv(result), length(result$s))
    expect_equal(n_features(result), nrow(result$u))
  })

})
