# Test fct_surface_mapper.R
#
# These tests run WITHOUT Shiny context - plain testthat.
# Unit tests for surface mapping functions (vol_to_surf wrappers).

# Source the functions directly (works both installed + dev mode)
module_file <- system.file("shiny/R/fct_surface_mapper.R", package = "plsrri")
if (module_file == "") {
  module_file <- testthat::test_path("../../inst/shiny/R/fct_surface_mapper.R")
}
source(module_file, local = FALSE)

describe("get_fsaverage_surfaces()", {

  it("validates geometry parameter", {
    expect_error(
      get_fsaverage_surfaces("invalid"),
      "Invalid geometry.*Must be one of"
    )
  })

  it("accepts valid geometry parameters", {
    # Test parameter validation only (skip actual loading if no neurosurf)
    skip_if_not_installed("neurosurf")

    # These should not error on invalid param
    # Valid options: pial, white, inflated, smoothwm, sphere
    expect_error(get_fsaverage_surfaces("pial"), NA)
    expect_error(get_fsaverage_surfaces("white"), NA)
    expect_error(get_fsaverage_surfaces("inflated"), NA)
    expect_error(get_fsaverage_surfaces("smoothwm"), NA)
    expect_error(get_fsaverage_surfaces("sphere"), NA)
  })

  it("returns list with lh and rh components", {
    skip_if_not_installed("neurosurf")

    surfaces <- get_fsaverage_surfaces("inflated")

    expect_type(surfaces, "list")
    expect_true("lh" %in% names(surfaces))
    expect_true("rh" %in% names(surfaces))
  })

  it("caches results (same object on second call)", {
    skip_if_not_installed("neurosurf")

    # Clear cache first
    clear_surface_cache()

    surfaces1 <- get_fsaverage_surfaces("inflated")
    surfaces2 <- get_fsaverage_surfaces("inflated")

    # Should be the exact same object from cache
    expect_identical(surfaces1, surfaces2)
  })

  it("errors when neurosurf not available", {
    skip_if(requireNamespace("neurosurf", quietly = TRUE),
            "neurosurf is installed - skipping unavailable test")

    # Clear cache so it tries to load
    clear_surface_cache()

    expect_error(
      get_fsaverage_surfaces("inflated"),
      "neurosurf package required"
    )
  })

})

describe("create_surface_sampler()", {

  it("validates surfaces parameter", {
    expect_error(
      create_surface_sampler(NULL, "mask", "lh"),
      "surfaces and mask are required"
    )
  })

  it("validates mask parameter", {
    expect_error(
      create_surface_sampler(list(lh = "surf"), NULL, "lh"),
      "surfaces and mask are required"
    )
  })

  it("validates hemisphere parameter", {
    expect_error(
      create_surface_sampler(list(lh = "surf"), "mask", "invalid"),
      "hemisphere must be 'lh' or 'rh'"
    )
  })

  it("accepts lh hemisphere", {
    skip_if_not_installed("neurosurf")
    skip_if_not_installed("mockery")

    # Mock neurosurf::surface_sampler
    mock_sampler <- mockery::mock("mock_sampler_object")
    mockery::stub(create_surface_sampler, "neurosurf::surface_sampler", mock_sampler)

    surfaces <- list(lh = "mock_surf_lh", rh = "mock_surf_rh")
    result <- create_surface_sampler(surfaces, "mock_mask", "lh")

    mockery::expect_called(mock_sampler, 1)
    args <- mockery::mock_args(mock_sampler)[[1]]
    expect_equal(args$surf_wm, "mock_surf_lh")
  })

  it("accepts rh hemisphere", {
    skip_if_not_installed("neurosurf")
    skip_if_not_installed("mockery")

    mock_sampler <- mockery::mock("mock_sampler_object")
    mockery::stub(create_surface_sampler, "neurosurf::surface_sampler", mock_sampler)

    surfaces <- list(lh = "mock_surf_lh", rh = "mock_surf_rh")
    result <- create_surface_sampler(surfaces, "mock_mask", "rh")

    args <- mockery::mock_args(mock_sampler)[[1]]
    expect_equal(args$surf_wm, "mock_surf_rh")
  })

  it("passes correct default parameters", {
    skip_if_not_installed("neurosurf")
    skip_if_not_installed("mockery")

    mock_sampler <- mockery::mock("sampler")
    mockery::stub(create_surface_sampler, "neurosurf::surface_sampler", mock_sampler)

    surfaces <- list(lh = "surf")
    create_surface_sampler(surfaces, "mask", "lh")

    args <- mockery::mock_args(mock_sampler)[[1]]
    expect_equal(args$knn, 6)
    expect_equal(args$dthresh, 16)
    expect_equal(args$sampling, "midpoint")
  })

  it("errors when neurosurf not available", {
    skip_if(requireNamespace("neurosurf", quietly = TRUE),
            "neurosurf is installed - skipping unavailable test")

    expect_error(
      create_surface_sampler(list(lh = "surf"), "mask", "lh"),
      "neurosurf package required"
    )
  })

})

describe("map_volume_to_surface()", {

  it("validates sampler parameter", {
    expect_error(
      map_volume_to_surface(NULL, "volume"),
      "sampler is required"
    )
  })

  it("validates volume parameter", {
    expect_error(
      map_volume_to_surface("sampler", NULL),
      "volume is required"
    )
  })

  it("calls apply_surface_sampler with correct arguments", {
    skip_if_not_installed("neurosurf")
    skip_if_not_installed("mockery")

    mock_apply <- mockery::mock("mapped_data")
    mockery::stub(map_volume_to_surface, "neurosurf::apply_surface_sampler", mock_apply)

    result <- map_volume_to_surface("my_sampler", "my_volume", fun = "nn")

    mockery::expect_called(mock_apply, 1)
    args <- mockery::mock_args(mock_apply)[[1]]
    expect_equal(args[[1]], "my_sampler")
    expect_equal(args[[2]], "my_volume")
    expect_equal(args$fun, "nn")
  })

  it("uses avg as default aggregation function", {
    skip_if_not_installed("neurosurf")
    skip_if_not_installed("mockery")

    mock_apply <- mockery::mock("mapped")
    mockery::stub(map_volume_to_surface, "neurosurf::apply_surface_sampler", mock_apply)

    map_volume_to_surface("sampler", "volume")

    args <- mockery::mock_args(mock_apply)[[1]]
    expect_equal(args$fun, "avg")
  })

  it("errors when neurosurf not available", {
    skip_if(requireNamespace("neurosurf", quietly = TRUE),
            "neurosurf is installed - skipping unavailable test")

    expect_error(
      map_volume_to_surface("sampler", "volume"),
      "neurosurf package required"
    )
  })

})

describe("map_result_to_surfaces()", {

  it("validates result parameter", {
    expect_error(
      map_result_to_surfaces(NULL, 1, "bsr", list(), list()),
      "result is required"
    )
  })

  it("validates surfaces parameter", {
    expect_error(
      map_result_to_surfaces("result", 1, "bsr", NULL, list(lh = "s", rh = "s")),
      "surfaces and samplers are required"
    )
  })

  it("validates samplers parameter", {
    expect_error(
      map_result_to_surfaces("result", 1, "bsr", list(lh = "s"), NULL),
      "surfaces and samplers are required"
    )
  })

  it("validates what parameter", {
    expect_error(
      map_result_to_surfaces("result", 1, "invalid", list(), list()),
      "what must be 'bsr' or 'salience'"
    )
  })

  it("extracts bsr correctly", {
    skip_if_not_installed("mockery")

    # Mock plsrri::bsr
    mock_bsr <- mockery::mock("bsr_volume")
    mockery::stub(map_result_to_surfaces, "plsrri::bsr", mock_bsr)

    # Mock map_volume_to_surface
    mock_map <- mockery::mock("mapped_lh", "mapped_rh")
    mockery::stub(map_result_to_surfaces, "map_volume_to_surface", mock_map)

    result <- map_result_to_surfaces(
      result = "my_result",
      lv = 2,
      what = "bsr",
      surfaces = list(lh = "surf_lh", rh = "surf_rh"),
      samplers = list(lh = "samp_lh", rh = "samp_rh")
    )

    mockery::expect_called(mock_bsr, 1)
    bsr_args <- mockery::mock_args(mock_bsr)[[1]]
    expect_equal(bsr_args[[1]], "my_result")
    expect_equal(bsr_args$lv, 2)
    expect_equal(bsr_args$as_neurovol, TRUE)
  })

  it("extracts salience correctly", {
    skip_if_not_installed("mockery")

    mock_salience <- mockery::mock("salience_volume")
    mockery::stub(map_result_to_surfaces, "plsrri::salience", mock_salience)

    mock_map <- mockery::mock("mapped_lh", "mapped_rh")
    mockery::stub(map_result_to_surfaces, "map_volume_to_surface", mock_map)

    result <- map_result_to_surfaces(
      result = "my_result",
      lv = 1,
      what = "salience",
      surfaces = list(lh = "surf_lh", rh = "surf_rh"),
      samplers = list(lh = "samp_lh", rh = "samp_rh")
    )

    mockery::expect_called(mock_salience, 1)
    sal_args <- mockery::mock_args(mock_salience)[[1]]
    expect_equal(sal_args$lv, 1)
    expect_equal(sal_args$as_neurovol, TRUE)
  })

  it("returns list with lh and rh components", {
    skip_if_not_installed("mockery")

    mock_bsr <- mockery::mock("vol")
    mockery::stub(map_result_to_surfaces, "plsrri::bsr", mock_bsr)

    mock_map <- mockery::mock("mapped_lh", "mapped_rh")
    mockery::stub(map_result_to_surfaces, "map_volume_to_surface", mock_map)

    result <- map_result_to_surfaces(
      result = "res",
      lv = 1,
      what = "bsr",
      surfaces = list(lh = "sl", rh = "sr"),
      samplers = list(lh = "spl", rh = "spr")
    )

    expect_type(result, "list")
    expect_true("lh" %in% names(result))
    expect_true("rh" %in% names(result))
    expect_equal(result$lh, "mapped_lh")
    expect_equal(result$rh, "mapped_rh")
  })

})

describe("clear_surface_cache()", {

  it("clears cached surfaces", {
    skip_if_not_installed("neurosurf")

    # Load a surface to populate cache
    get_fsaverage_surfaces("inflated")

    # Verify something is cached
    cache_before <- ls(.surface_cache)
    expect_true(length(cache_before) > 0)

    # Clear cache
    clear_surface_cache()

    # Verify cache is empty
    cache_after <- ls(.surface_cache)
    expect_equal(length(cache_after), 0)
  })

  it("returns NULL invisibly", {
    result <- clear_surface_cache()
    expect_null(result)
  })

})
