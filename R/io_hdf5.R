#' HDF5 I/O for plsrri results
#'
#' @description
#' Native HDF5 reader/writer for `pls_result` objects. Provides lossless
#' round-trip for all six PLS methods (including multiblock and non-rotated
#' variants) along with optional `perm_result`, `boot_result`,
#' `splithalf_result`, NeuroVol masks, and site/diagnostics metadata.
#'
#' The schema (root attribute `format_version = "plsrri/1.0"`) groups fields
#' by category — `/decomposition`, `/scores`, `/correlations`, `/multiblock`,
#' `/perm_result`, `/boot_result`, `/splithalf_result`, `/inputs`,
#' `/metadata` — and uses gzip+shuffle chunking for large arrays. Datasets
#' carry self-describing `dims` and `description` attributes so the file is
#' inspectable in any HDF5 tool (HDFView, h5dump, h5py).
#'
#' Requires the `hdf5r` package (Suggests).
#'
#' @name plsrri-hdf5
#' @keywords internal
NULL

# ---------------------------------------------------------------------------
# Format constants
# ---------------------------------------------------------------------------

.plsrri_hdf5_format_version <- "plsrri/1.0"

# Per-method-class version of class-attr we expect on read.
.plsrri_pls_method_classes <- c(
  "pls_task",
  "pls_task_nonrot",
  "pls_behavior",
  "pls_multiblock",
  "pls_behavior_nonrot",
  "pls_multiblock_nonrot"
)

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.plsrri_check_hdf5r <- function() {
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop(
      "The 'hdf5r' package is required for HDF5 I/O. ",
      "Install it with install.packages('hdf5r').",
      call. = FALSE
    )
  }
}

# Threshold below which we skip chunking/compression (overhead exceeds gain).
.plsrri_hdf5_min_chunk <- 1024L

# Choose chunk dims tuned to typical access pattern (read whole columns/LVs).
.plsrri_hdf5_chunk <- function(dim_vec) {
  if (length(dim_vec) == 1L) {
    return(if (dim_vec[1] >= .plsrri_hdf5_min_chunk) min(dim_vec[1], 65536L) else NULL)
  }
  if (length(dim_vec) == 2L) {
    rows <- min(dim_vec[1], 4096L)
    return(c(rows, dim_vec[2]))
  }
  if (length(dim_vec) == 3L) {
    return(c(dim_vec[1], dim_vec[2], min(dim_vec[3], 64L)))
  }
  # Fallback: full extent
  pmin(dim_vec, 64L)
}

# Set group-level attributes (scalars and short character strings).
.plsrri_h5_set_attr <- function(obj, name, value) {
  if (is.null(value)) return(invisible(NULL))
  hdf5r::h5attr(obj, name) <- value
  invisible(NULL)
}

# Write an array/vector as a dataset with optional compression and metadata
# attributes. NULL inputs are skipped (absence-of-dataset = NULL on read).
.plsrri_h5_write_array <- function(grp, name, value,
                                    compression = 4L,
                                    dims_attr = NULL, description = NULL) {
  if (is.null(value)) return(invisible(NULL))

  # Strip names/dimnames before writing — HDF5 has no portable concept of
  # named dimensions; we re-attach on read where applicable.
  attr_names <- names(value)
  attr_dimnames <- if (is.array(value) || is.matrix(value)) dimnames(value) else NULL

  v <- value
  names(v) <- NULL
  if (!is.null(attr_dimnames)) dimnames(v) <- NULL

  shape <- if (is.array(v) || is.matrix(v)) dim(v) else length(v)
  chunks <- if (prod(shape) >= .plsrri_hdf5_min_chunk) .plsrri_hdf5_chunk(shape) else NULL

  if (is.null(chunks)) {
    grp$create_dataset(name, robj = v)
  } else {
    grp$create_dataset(
      name, robj = v,
      chunk_dims = chunks,
      gzip_level = compression
    )
  }

  ds <- grp[[name]]
  if (!is.null(attr_names)) hdf5r::h5attr(ds, "names") <- attr_names
  if (!is.null(attr_dimnames)) {
    if (!is.null(attr_dimnames[[1]])) hdf5r::h5attr(ds, "rownames") <- attr_dimnames[[1]]
    if (!is.null(attr_dimnames[[2]])) hdf5r::h5attr(ds, "colnames") <- attr_dimnames[[2]]
  }
  if (!is.null(dims_attr)) hdf5r::h5attr(ds, "dims") <- dims_attr
  if (!is.null(description)) hdf5r::h5attr(ds, "description") <- description
  invisible(NULL)
}

# Read a dataset back as a vector/matrix, restoring names/dimnames if stored.
.plsrri_h5_read_array <- function(ds) {
  v <- ds$read()

  attrs <- tryCatch(hdf5r::h5attributes(ds), error = function(e) list())
  if (!is.null(attrs[["names"]]) && is.null(dim(v))) {
    names(v) <- attrs[["names"]]
  }
  if (!is.null(attrs[["rownames"]]) || !is.null(attrs[["colnames"]])) {
    if (length(dim(v)) == 2L) {
      dimnames(v) <- list(
        attrs[["rownames"]] %||% NULL,
        attrs[["colnames"]] %||% NULL
      )
    }
  }
  v
}

# Null coalesce — local rather than depend on rlang.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Read all child datasets in a group into a named list (skips subgroups).
.plsrri_h5_group_to_list <- function(grp) {
  out <- list()
  if (is.null(grp)) return(out)
  for (nm in names(grp)) {
    obj <- grp[[nm]]
    if (inherits(obj, "H5D")) {
      out[[nm]] <- .plsrri_h5_read_array(obj)
    }
    obj$close()
  }
  out
}

# Set of attributes-as-fields under a group, returned as a named list.
.plsrri_h5_attrs_to_list <- function(obj, exclude = character()) {
  attrs <- tryCatch(hdf5r::h5attributes(obj), error = function(e) list())
  attrs[setdiff(names(attrs), exclude)]
}

# Write a named list of matrices as a numbered subgroup with `length` attr.
# Used for fields like `datamatcorrs_lst`.
.plsrri_h5_write_list_of_matrices <- function(parent, name, lst) {
  if (is.null(lst) || length(lst) == 0L) return(invisible(NULL))
  grp <- parent$create_group(name)
  hdf5r::h5attr(grp, "length") <- length(lst)
  if (!is.null(names(lst))) {
    hdf5r::h5attr(grp, "names") <- names(lst)
  }
  for (i in seq_along(lst)) {
    item <- lst[[i]]
    if (!is.null(item)) {
      .plsrri_h5_write_array(grp, sprintf("%d", i - 1L), item)
    }
  }
  grp$close()
  invisible(NULL)
}

# Inverse of the above.
.plsrri_h5_read_list_of_matrices <- function(grp) {
  if (is.null(grp)) return(NULL)
  attrs <- .plsrri_h5_attrs_to_list(grp)
  n <- attrs[["length"]]
  if (is.null(n)) n <- length(names(grp))
  if (n == 0L) return(list())
  out <- vector("list", n)
  if (!is.null(attrs[["names"]])) names(out) <- attrs[["names"]]
  for (i in seq_len(n)) {
    key <- sprintf("%d", i - 1L)
    if (key %in% names(grp)) {
      ds <- grp[[key]]
      out[[i]] <- .plsrri_h5_read_array(ds)
      ds$close()
    }
  }
  out
}

# ---------------------------------------------------------------------------
# NeuroVol mask handling
# ---------------------------------------------------------------------------
#
# Strategy: store volumetric data as a 3-D dataset (inspectable from any
# HDF5 viewer) plus the NeuroSpace as serialized R bytes. On read, if
# `neuroim2` is available, reconstruct a DenseNeuroVol; otherwise return
# the bare array with the dim/space attrs.

.plsrri_h5_write_mask <- function(parent, mask) {
  if (is.null(mask)) return(invisible(NULL))
  grp <- parent$create_group("mask")

  is_neurovol <- isS4(mask) && tryCatch(
    methods::is(mask, "DenseNeuroVol"),
    error = function(e) FALSE
  )

  if (is_neurovol) {
    data_arr <- as.array(mask@.Data)
    .plsrri_h5_write_array(grp, "data", data_arr,
                            description = "Volumetric mask values (DenseNeuroVol@.Data)")
    space_bytes <- serialize(mask@space, NULL)
    .plsrri_h5_write_array(grp, "space_serialized", as.integer(space_bytes),
                            description = "R-serialized NeuroSpace (use unserialize() on int->raw)")
    hdf5r::h5attr(grp, "neurovol_class") <- "DenseNeuroVol"
    hdf5r::h5attr(grp, "format") <- "neurovol/r-serialized-space/1.0"
  } else if (is.array(mask) || is.matrix(mask) || is.numeric(mask) || is.logical(mask)) {
    .plsrri_h5_write_array(grp, "data", mask, description = "Mask data")
    hdf5r::h5attr(grp, "neurovol_class") <- "array"
    hdf5r::h5attr(grp, "format") <- "array/1.0"
  } else {
    # Fallback: serialize the whole object.
    bytes <- serialize(mask, NULL)
    .plsrri_h5_write_array(grp, "serialized", as.integer(bytes),
                            description = "R-serialized mask object (use unserialize() on int->raw)")
    hdf5r::h5attr(grp, "neurovol_class") <- "serialized"
    hdf5r::h5attr(grp, "format") <- "serialized/1.0"
  }
  grp$close()
  invisible(NULL)
}

.plsrri_h5_read_mask <- function(grp) {
  if (is.null(grp)) return(NULL)
  attrs <- .plsrri_h5_attrs_to_list(grp)
  cls <- attrs[["neurovol_class"]] %||% "array"

  if (cls == "DenseNeuroVol") {
    data_ds <- grp[["data"]]
    data_arr <- .plsrri_h5_read_array(data_ds); data_ds$close()
    space_ds <- grp[["space_serialized"]]
    space_int <- .plsrri_h5_read_array(space_ds); space_ds$close()

    if (requireNamespace("neuroim2", quietly = TRUE)) {
      space <- unserialize(as.raw(space_int))
      return(methods::new("DenseNeuroVol", .Data = data_arr, space = space))
    } else {
      warning(
        "Mask was a DenseNeuroVol; install 'neuroim2' to reconstruct it. ",
        "Returning the bare data array.", call. = FALSE
      )
      return(data_arr)
    }
  }

  if (cls == "serialized") {
    ds <- grp[["serialized"]]
    bytes_int <- .plsrri_h5_read_array(ds); ds$close()
    return(unserialize(as.raw(bytes_int)))
  }

  # cls == "array" or unknown; just return the raw data.
  if ("data" %in% names(grp)) {
    ds <- grp[["data"]]
    out <- .plsrri_h5_read_array(ds); ds$close()
    return(out)
  }
  NULL
}

# ---------------------------------------------------------------------------
# Writer
# ---------------------------------------------------------------------------

.plsrri_write_pls_hdf5 <- function(x, file, compression = 4L) {
  .plsrri_check_hdf5r()
  if (!inherits(x, "pls_result")) {
    stop("write_results(format='hdf5') requires a 'pls_result' object", call. = FALSE)
  }

  h5 <- hdf5r::H5File$new(file, mode = "w")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)

  # Root attrs
  hdf5r::h5attr(h5, "format_version") <- .plsrri_hdf5_format_version
  hdf5r::h5attr(h5, "created_by") <- "plsrri"
  hdf5r::h5attr(h5, "created_at") <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  hdf5r::h5attr(h5, "plsrri_version") <- as.character(utils::packageVersion("plsrri"))
  hdf5r::h5attr(h5, "R_version") <- R.version.string
  hdf5r::h5attr(h5, "method") <- as.integer(x$method)
  hdf5r::h5attr(h5, "method_name") <- pls_method_int_to_name(x$method)
  hdf5r::h5attr(h5, "method_label") <- pls_method_label(x$method)
  hdf5r::h5attr(h5, "class") <- class(x)
  hdf5r::h5attr(h5, "is_struct") <- isTRUE(x$is_struct)

  write_arr <- function(grp, nm, value, ..., dims_attr = NULL, description = NULL) {
    .plsrri_h5_write_array(grp, nm, value,
                            compression = compression,
                            dims_attr = dims_attr, description = description)
  }

  # /decomposition
  decomp <- h5$create_group("decomposition")
  write_arr(decomp, "u", x$u,
            dims_attr = c("features", "lv"),
            description = "Left singular vectors / saliences")
  write_arr(decomp, "s", x$s,
            dims_attr = "lv",
            description = "Singular values")
  write_arr(decomp, "v", x$v,
            dims_attr = c("design_or_behavior", "lv"),
            description = "Right singular vectors / loadings")
  decomp$close()

  # /scores
  sc <- h5$create_group("scores")
  write_arr(sc, "usc", x$usc,
            dims_attr = c("observation", "lv"),
            description = "Brain scores (X projected into LV space)")
  write_arr(sc, "vsc", x$vsc,
            dims_attr = c("design_or_behavior", "lv"),
            description = "Design or behavior scores")
  sc$close()

  # /correlations
  corr <- h5$create_group("correlations")
  write_arr(corr, "lvcorrs", x$lvcorrs,
            description = "Per-LV correlations (behavior PLS)")
  if (!is.null(x$datamatcorrs_lst) && length(x$datamatcorrs_lst) > 0L) {
    .plsrri_h5_write_list_of_matrices(corr, "datamatcorrs_lst", x$datamatcorrs_lst)
  }
  corr$close()

  # /multiblock
  if (!is.null(x$TBv) || !is.null(x$TBusc) || !is.null(x$TBvsc)) {
    mb <- h5$create_group("multiblock")
    write_arr(mb, "TBv", x$TBv,
              description = "Task/Behavior loadings (multiblock)")
    write_arr(mb, "TBusc", x$TBusc,
              description = "Task/Behavior brain scores (multiblock)")
    write_arr(mb, "TBvsc", x$TBvsc,
              description = "Task/Behavior design/behavior scores (multiblock)")
    mb$close()
  }

  # /perm_result
  if (!is.null(x$perm_result)) {
    pr <- h5$create_group("perm_result")
    p <- x$perm_result
    hdf5r::h5attr(pr, "num_perm") <- as.integer(p$num_perm %||% 0L)
    write_arr(pr, "sp", p$sp,
              dims_attr = "lv", description = "Counts: permuted s >= observed s")
    write_arr(pr, "sprob", p$sprob,
              dims_attr = "lv", description = "Permutation p-values (sp / num_perm)")
    write_arr(pr, "permsamp", p$permsamp,
              dims_attr = c("row", "perm"))
    write_arr(pr, "Tpermsamp", p$Tpermsamp,
              dims_attr = c("row", "perm"))
    write_arr(pr, "Bpermsamp", p$Bpermsamp,
              dims_attr = c("row", "perm"))
    write_arr(pr, "perm_singval", p$perm_singval,
              dims_attr = c("lv", "perm"),
              description = "Full permutation null distribution of singular values")
    pr$close()
  }

  # /boot_result
  if (!is.null(x$boot_result)) {
    br <- h5$create_group("boot_result")
    b <- x$boot_result
    hdf5r::h5attr(br, "num_boot") <- as.integer(b$num_boot %||% 0L)
    hdf5r::h5attr(br, "boot_type") <- as.character(b$boot_type %||% "strat")
    hdf5r::h5attr(br, "clim") <- as.numeric(b$clim %||% 95)
    hdf5r::h5attr(br, "nonrotated_boot") <- isTRUE(b$nonrotated_boot)
    boot_array_fields <- c(
      "compare_u", "u_se", "u_mean", "compare_v",
      "bootsamp", "bootsamp_4beh", "distrib", "prop",
      "Tdistrib", "Tprop",
      "usc2", "orig_usc", "ulusc", "llusc", "ulusc_adj", "llusc_adj",
      "orig_corr", "ulcorr", "llcorr", "ulcorr_adj", "llcorr_adj",
      "vsc_distrib_boot", "usc_distrib_boot",
      "ci_u", "ci_u_adj"
    )
    for (nm in boot_array_fields) {
      if (!is.null(b[[nm]])) write_arr(br, nm, b[[nm]])
    }
    if (!is.null(b$num_LowVariability_behav_boots)) {
      hdf5r::h5attr(br, "num_LowVariability_behav_boots") <-
        as.integer(b$num_LowVariability_behav_boots)
    }
    if (!is.null(b$badbeh)) write_arr(br, "badbeh", b$badbeh)
    if (!is.null(b$countnewtotal)) write_arr(br, "countnewtotal", b$countnewtotal)
    br$close()
  }

  # /splithalf_result
  if (!is.null(x$splithalf_result)) {
    sr <- h5$create_group("splithalf_result")
    s <- x$splithalf_result
    hdf5r::h5attr(sr, "num_outer_perm") <- as.integer(s$num_outer_perm %||% 0L)
    hdf5r::h5attr(sr, "num_split") <- as.integer(s$num_split %||% 0L)
    sh_fields <- c(
      "orig_ucorr", "orig_vcorr",
      "ucorr_prob", "vcorr_prob",
      "ucorr_ul", "ucorr_ll", "vcorr_ul", "vcorr_ll"
    )
    for (nm in sh_fields) {
      if (!is.null(s[[nm]])) write_arr(sr, nm, s[[nm]])
    }
    sr$close()
  }

  # /inputs
  inp <- h5$create_group("inputs")
  if (!is.null(x$num_subj_lst)) {
    if (is.list(x$num_subj_lst)) {
      # SSB shape: list of integer vectors. Store via list-of-matrices helper.
      .plsrri_h5_write_list_of_matrices(inp, "num_subj_lst", x$num_subj_lst)
      hdf5r::h5attr(inp, "num_subj_lst_is_list") <- TRUE
    } else {
      write_arr(inp, "num_subj_lst", as.integer(x$num_subj_lst))
      hdf5r::h5attr(inp, "num_subj_lst_is_list") <- FALSE
    }
  }
  if (!is.null(x$num_cond)) hdf5r::h5attr(inp, "num_cond") <- as.integer(x$num_cond)
  if (!is.null(x$bscan)) write_arr(inp, "bscan", as.integer(x$bscan))
  write_arr(inp, "stacked_designdata", x$stacked_designdata)
  write_arr(inp, "stacked_behavdata", x$stacked_behavdata)
  if (!is.null(x$other_input) && length(x$other_input) > 0L) {
    oi <- inp$create_group("other_input")
    for (nm in names(x$other_input)) {
      val <- x$other_input[[nm]]
      if (is.null(val)) next
      if (is.atomic(val) && length(val) == 1L) {
        hdf5r::h5attr(oi, nm) <- val
      } else {
        write_arr(oi, nm, val)
      }
    }
    oi$close()
  }
  inp$close()

  # /metadata
  meta <- h5$create_group("metadata")
  if (!is.null(x$mask)) .plsrri_h5_write_mask(meta, x$mask)
  if (!is.null(x$site)) {
    if (is.atomic(x$site) && length(x$site) == 1L) {
      hdf5r::h5attr(meta, "site") <- x$site
    } else {
      write_arr(meta, "site", x$site)
    }
  }
  if (!is.null(x$site_diagnostics)) {
    sd_grp <- meta$create_group("site_diagnostics")
    for (nm in names(x$site_diagnostics)) {
      val <- x$site_diagnostics[[nm]]
      if (is.null(val)) next
      if (is.atomic(val) && length(val) == 1L) {
        hdf5r::h5attr(sd_grp, nm) <- val
      } else {
        write_arr(sd_grp, nm, val)
      }
    }
    sd_grp$close()
  }
  meta$close()

  h5$close_all()
  invisible(file)
}

# ---------------------------------------------------------------------------
# Reader
# ---------------------------------------------------------------------------

.plsrri_read_pls_hdf5 <- function(file) {
  .plsrri_check_hdf5r()
  if (!hdf5r::is_hdf5(file)) {
    stop("File '", file, "' is not a valid HDF5 file.", call. = FALSE)
  }

  h5 <- hdf5r::H5File$new(file, mode = "r")
  on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)

  root_attrs <- .plsrri_h5_attrs_to_list(h5)
  ver <- root_attrs[["format_version"]]
  if (is.null(ver)) {
    stop("HDF5 file is missing 'format_version' attribute. ",
         "It was not produced by plsrri.", call. = FALSE)
  }
  if (!grepl("^plsrri/1\\.", ver)) {
    stop("Unsupported plsrri HDF5 format version: ", ver, ". ",
         "This plsrri build supports plsrri/1.x files.", call. = FALSE)
  }

  method <- as.integer(root_attrs[["method"]])

  read_grp <- function(name) {
    if (name %in% names(h5)) {
      grp <- h5[[name]]
      out <- list(grp = grp, list = .plsrri_h5_group_to_list(grp),
                  attrs = .plsrri_h5_attrs_to_list(grp))
      return(out)
    }
    NULL
  }

  # /decomposition
  decomp <- read_grp("decomposition")
  u <- decomp$list[["u"]]
  s <- decomp$list[["s"]]
  v <- decomp$list[["v"]]

  # /scores
  sc <- read_grp("scores")
  usc <- sc$list[["usc"]]
  vsc <- sc$list[["vsc"]]

  # /correlations
  corr <- read_grp("correlations")
  lvcorrs <- corr$list[["lvcorrs"]]
  datamatcorrs_lst <- if (!is.null(corr) && "datamatcorrs_lst" %in% names(corr$grp)) {
    sub <- corr$grp[["datamatcorrs_lst"]]
    out <- .plsrri_h5_read_list_of_matrices(sub)
    sub$close()
    out
  } else NULL

  # /multiblock
  mb <- read_grp("multiblock")
  TBv <- mb$list[["TBv"]] %||% NULL
  TBusc <- mb$list[["TBusc"]] %||% NULL
  TBvsc <- mb$list[["TBvsc"]] %||% NULL

  # /perm_result
  perm_result <- NULL
  if ("perm_result" %in% names(h5)) {
    pr <- read_grp("perm_result")
    perm_result <- new_pls_perm_result(
      num_perm = as.integer(pr$attrs[["num_perm"]] %||% 0L),
      sp = pr$list[["sp"]],
      sprob = pr$list[["sprob"]],
      permsamp = pr$list[["permsamp"]],
      Tpermsamp = pr$list[["Tpermsamp"]],
      Bpermsamp = pr$list[["Bpermsamp"]],
      perm_singval = pr$list[["perm_singval"]]
    )
  }

  # /boot_result
  boot_result <- NULL
  if ("boot_result" %in% names(h5)) {
    br <- read_grp("boot_result")
    a <- br$attrs
    boot_args <- list(
      num_boot = as.integer(a[["num_boot"]] %||% 0L),
      boot_type = as.character(a[["boot_type"]] %||% "strat"),
      clim = as.numeric(a[["clim"]] %||% 95),
      nonrotated_boot = isTRUE(a[["nonrotated_boot"]])
    )
    if (!is.null(a[["num_LowVariability_behav_boots"]])) {
      boot_args$num_LowVariability_behav_boots <-
        as.integer(a[["num_LowVariability_behav_boots"]])
    }
    # Map array fields directly from the group's datasets.
    for (nm in names(br$list)) {
      boot_args[[nm]] <- br$list[[nm]]
    }
    boot_result <- do.call(new_pls_boot_result, boot_args)
  }

  # /splithalf_result
  splithalf_result <- NULL
  if ("splithalf_result" %in% names(h5)) {
    sr <- read_grp("splithalf_result")
    splithalf_result <- new_pls_splithalf_result(
      num_outer_perm = as.integer(sr$attrs[["num_outer_perm"]] %||% 0L),
      num_split = as.integer(sr$attrs[["num_split"]] %||% 0L),
      orig_ucorr = sr$list[["orig_ucorr"]],
      orig_vcorr = sr$list[["orig_vcorr"]],
      ucorr_prob = sr$list[["ucorr_prob"]],
      vcorr_prob = sr$list[["vcorr_prob"]],
      ucorr_ul = sr$list[["ucorr_ul"]],
      ucorr_ll = sr$list[["ucorr_ll"]],
      vcorr_ul = sr$list[["vcorr_ul"]],
      vcorr_ll = sr$list[["vcorr_ll"]]
    )
  }

  # /inputs
  num_subj_lst <- NULL
  num_cond <- NULL
  bscan <- NULL
  stacked_designdata <- NULL
  stacked_behavdata <- NULL
  other_input <- NULL
  if ("inputs" %in% names(h5)) {
    inp <- read_grp("inputs")
    if (isTRUE(inp$attrs[["num_subj_lst_is_list"]]) &&
        "num_subj_lst" %in% names(inp$grp)) {
      sub <- inp$grp[["num_subj_lst"]]
      num_subj_lst <- .plsrri_h5_read_list_of_matrices(sub)
      sub$close()
    } else {
      num_subj_lst <- inp$list[["num_subj_lst"]]
    }
    num_cond <- inp$attrs[["num_cond"]]
    bscan <- inp$list[["bscan"]]
    stacked_designdata <- inp$list[["stacked_designdata"]]
    stacked_behavdata <- inp$list[["stacked_behavdata"]]
    if ("other_input" %in% names(inp$grp)) {
      oi <- inp$grp[["other_input"]]
      other_input <- c(.plsrri_h5_attrs_to_list(oi), .plsrri_h5_group_to_list(oi))
      oi$close()
    }
  }

  # /metadata
  mask <- NULL
  site <- NULL
  site_diagnostics <- NULL
  if ("metadata" %in% names(h5)) {
    meta <- h5[["metadata"]]
    if ("mask" %in% names(meta)) {
      mg <- meta[["mask"]]
      mask <- .plsrri_h5_read_mask(mg)
      mg$close()
    }
    meta_attrs <- .plsrri_h5_attrs_to_list(meta)
    if (!is.null(meta_attrs[["site"]])) site <- meta_attrs[["site"]]
    if (is.null(site) && "site" %in% names(meta)) {
      ds <- meta[["site"]]; site <- .plsrri_h5_read_array(ds); ds$close()
    }
    if ("site_diagnostics" %in% names(meta)) {
      sd_grp <- meta[["site_diagnostics"]]
      site_diagnostics <- c(
        .plsrri_h5_attrs_to_list(sd_grp),
        .plsrri_h5_group_to_list(sd_grp)
      )
      sd_grp$close()
    }
    meta$close()
  }

  is_struct <- isTRUE(root_attrs[["is_struct"]])

  res <- new_pls_result(
    method = method,
    u = u, s = s, v = v,
    usc = usc, vsc = vsc,
    datamatcorrs_lst = datamatcorrs_lst,
    lvcorrs = lvcorrs,
    perm_result = perm_result,
    boot_result = boot_result,
    splithalf_result = splithalf_result,
    num_subj_lst = num_subj_lst,
    num_cond = num_cond,
    bscan = bscan,
    stacked_designdata = stacked_designdata,
    stacked_behavdata = stacked_behavdata,
    other_input = other_input,
    TBv = TBv, TBusc = TBusc, TBvsc = TBvsc,
    is_struct = is_struct,
    mask = mask,
    site = site,
    site_diagnostics = site_diagnostics
  )

  res
}
