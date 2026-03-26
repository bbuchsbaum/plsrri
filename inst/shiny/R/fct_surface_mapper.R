# Surface Mapper Functions
# Pure functions for mapping volume data to cortical surfaces via neurosurf
# Part of Phase 4: surfwidget Integration

# Environment-based cache for fsaverage surfaces
.surface_cache <- new.env(parent = emptyenv())

#' Get fsaverage surfaces
#'
#' @description
#' Load fsaverage surfaces from neurosurf. Results are cached per geometry type
#' to avoid repeated loading.
#'
#' @param geometry Character: "pial", "white", "inflated", "smoothwm", or "sphere"
#' @return List with components lh (left hemisphere) and rh (right hemisphere)
#'
#' @keywords internal
get_fsaverage_surfaces <- function(geometry = "inflated") {
  # Validate geometry parameter (matches neurosurf::load_fsaverage_std8 options)
  valid_geometries <- c("pial", "white", "inflated", "smoothwm", "sphere")
  if (!geometry %in% valid_geometries) {
    stop("Invalid geometry '", geometry, "'. Must be one of: ",
         paste(valid_geometries, collapse = ", "), call. = FALSE)
  }

  # Check cache
  cache_key <- paste0("fsaverage_", geometry)
  if (exists(cache_key, envir = .surface_cache)) {
    return(get(cache_key, envir = .surface_cache))
  }

  # Load surfaces via neurosurf
  if (exists("ensure_rgl_use_null", mode = "function")) {
    ensure_rgl_use_null()
  }
  if (!requireNamespace("neurosurf", quietly = TRUE)) {
    stop("neurosurf package required for surface mapping", call. = FALSE)
  }

  surfaces <- neurosurf::load_fsaverage_std8(geometry)

  # Cache result
  assign(cache_key, surfaces, envir = .surface_cache)

  surfaces
}

#' Create surface sampler for repeated mapping
#'
#' @description
#' Creates a vol_to_surf sampler that can be reused for mapping multiple volumes
#' to the same surface. This is much faster than calling vol_to_surf() repeatedly.
#'
#' @param surfaces List with lh and rh surface geometries
#' @param mask NeuroVol mask defining the volume space
#' @param hemisphere Character: "lh" for left, "rh" for right
#' @param knn Number of nearest neighbors (default 6)
#' @param dthresh Distance threshold in mm (default 16)
#' @param sampling Sampling method: "midpoint" (default), "pial", "white"
#' @return Surface sampler object for use with apply_surface_sampler
#'
#' @keywords internal
create_surface_sampler <- function(surfaces, mask, hemisphere = "lh",
                                    knn = 6, dthresh = 16, sampling = "midpoint") {
  if (is.null(surfaces) || is.null(mask)) {
    stop("surfaces and mask are required", call. = FALSE)
  }

  if (!hemisphere %in% c("lh", "rh")) {
    stop("hemisphere must be 'lh' or 'rh'", call. = FALSE)
  }

  if (exists("ensure_rgl_use_null", mode = "function")) {
    ensure_rgl_use_null()
  }
  if (!requireNamespace("neurosurf", quietly = TRUE)) {
    stop("neurosurf package required for surface mapping", call. = FALSE)
  }

  surf <- surfaces[[hemisphere]]

  neurosurf::surface_sampler(
    surf_wm = surf,
    surf_pial = surf,  # Use same surface for inflated/flat geometries
    vol_template = mask,
    mask = mask,
    sampling = sampling,
    knn = knn,
    dthresh = dthresh
  )
}

#' Map volume data to surface using pre-computed sampler
#'
#' @description
#' Applies a pre-computed sampler to map volume data to surface vertices.
#' Much faster than vol_to_surf() when mapping multiple volumes.
#'
#' @param sampler Surface sampler from create_surface_sampler()
#' @param volume NeuroVol object to map
#' @param fun Aggregation function: "avg" (default), "nn", "mode"
#' @param fill Value for outside-mask vertices (default 0)
#' @return Mapped surface values
#'
#' @keywords internal
map_volume_to_surface <- function(sampler, volume, fun = "avg", fill = 0) {
  if (is.null(sampler)) {
    stop("sampler is required", call. = FALSE)
  }
  if (is.null(volume)) {
    stop("volume is required", call. = FALSE)
  }

  if (exists("ensure_rgl_use_null", mode = "function")) {
    ensure_rgl_use_null()
  }
  if (!requireNamespace("neurosurf", quietly = TRUE)) {
    stop("neurosurf package required for surface mapping", call. = FALSE)
  }

  neurosurf::apply_surface_sampler(sampler, volume, fun = fun)
}

#' Map PLS result to both hemisphere surfaces
#'
#' @description
#' High-level function that extracts a volume from PLS result (BSR or salience)
#' and maps it to both left and right hemisphere surfaces.
#'
#' @param result A pls_result object
#' @param lv Latent variable number to map
#' @param what What to map: "bsr" or "salience"
#' @param surfaces List with lh and rh surface geometries
#' @param samplers List with lh and rh samplers (from create_surface_sampler)
#' @param fun Aggregation function (default "avg")
#' @return List with components lh and rh containing mapped surface values
#'
#' @keywords internal
map_result_to_surfaces <- function(result, lv, what, surfaces, samplers, fun = "avg") {
  if (is.null(result)) {
    stop("result is required", call. = FALSE)
  }
  if (is.null(surfaces) || is.null(samplers)) {
    stop("surfaces and samplers are required", call. = FALSE)
  }
  if (!what %in% c("bsr", "salience")) {
    stop("what must be 'bsr' or 'salience'", call. = FALSE)
  }

  # Extract volume from result
  vol <- if (what == "bsr") {
    plsrri::bsr(result, lv = lv, as_neurovol = TRUE)
  } else {
    plsrri::salience(result, lv = lv, as_neurovol = TRUE)
  }

  # Map to each hemisphere
  mapped_lh <- map_volume_to_surface(samplers$lh, vol, fun = fun)
  mapped_rh <- map_volume_to_surface(samplers$rh, vol, fun = fun)

  list(
    lh = mapped_lh,
    rh = mapped_rh
  )
}

#' Clear surface cache
#'
#' @description
#' Removes all cached fsaverage surfaces. Useful for memory management or
#' forcing a reload.
#'
#' @keywords internal
clear_surface_cache <- function() {
  rm(list = ls(.surface_cache), envir = .surface_cache)
  invisible(NULL)
}
