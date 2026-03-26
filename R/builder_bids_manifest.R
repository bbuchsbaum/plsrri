# Internal indirection helpers (make bidser calls mockable in tests)
.bidser_bids_project <- function(...) bidser::bids_project(...)
.bidser_participants <- function(...) bidser::participants(...)
.bidser_mask_files <- function(...) bidser::mask_files(...)
.bidser_search_files <- function(...) bidser::search_files(...)

#' Add Subjects from BIDS as a Lagged NIfTI Manifest
#'
#' @description
#' Builds a manifest (subject × condition rows) from a BIDS dataset using
#' `bidser`, then routes through `add_subjects_manifest()` so that **NIfTI dim4**
#' is interpreted as **lags** and folded as voxel×lag features.
#'
#' This is intended for event-related/fIR-style derivatives where each file is a
#' 4D NIfTI whose 4th dimension corresponds to lags (time since event onset).
#'
#' @param spec A `pls_spec` object.
#' @param bids_dir Path to BIDS dataset root.
#' @param groups NULL/"all" or character vector of group labels found in
#'   `participants.tsv` (same semantics as `add_subjects()`).
#' @param group_col Optional grouping column name in `participants.tsv`. When
#'   provided, `groups` are interpreted as values from this column.
#' @param task Optional task filter passed to `bidser::search_files()`.
#' @param space Optional space filter passed to `bidser::search_files()` and
#'   `bidser::mask_files()`.
#' @param file_regex Regex applied to filenames to find candidate NIfTI files.
#'   Defaults to any NIfTI.
#' @param condition_keys Character vector of BIDS entity keys to use to define
#'   conditions. Values are extracted from filenames using patterns like
#'   `_<key>-<value>` and joined with `_`.
#' @param volumes Optional integer vector (1-based) or compact spec string
#'   (e.g., `"1:8"`) selecting a subset of dim4 volumes (lags). When provided,
#'   the manifest `file` column uses `file[volspec]` syntax.
#' @param mask_method How to derive a brain mask if `spec$mask` is NULL:
#'   `"intersection"` (default), `"union"`, or `"first"`.
#' @param strict Logical. If TRUE (default), error on missing condition keys or
#'   duplicated (group, subject, condition) rows.
#' @param ... Additional filters forwarded to `bidser::search_files()` and
#'   `bidser::mask_files()`.
#'
#' @return Updated `pls_spec` object with manifest-based inputs.
#' @export
add_subjects_bids_manifest <- function(spec,
                                       bids_dir,
                                       groups = NULL,
                                       group_col = NULL,
                                       task = NULL,
                                       space = NULL,
                                       file_regex = ".*\\.nii(\\.gz)?$",
                                       condition_keys = "cond",
                                       volumes = NULL,
                                       mask_method = "intersection",
                                       strict = TRUE,
                                       ...) {
  assert_that(inherits(spec, "pls_spec"))

  if (!requireNamespace("bidser", quietly = TRUE)) {
    stop("Package 'bidser' is required for BIDS manifest building.", call. = FALSE)
  }
  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package 'neuroim2' is required for BIDS manifest building.", call. = FALSE)
  }

  if (!is.character(bids_dir) || length(bids_dir) != 1L || !dir.exists(bids_dir)) {
    stop("bids_dir must be an existing directory path", call. = FALSE)
  }

  condition_keys <- as.character(condition_keys)
  condition_keys <- condition_keys[nzchar(condition_keys)]
  if (length(condition_keys) == 0L) {
    stop("condition_keys must contain at least one key (e.g., 'cond' or 'desc')", call. = FALSE)
  }

  # Build bidser project (assume derivatives may be present)
  bids <- .bidser_bids_project(bids_dir, fmriprep = TRUE)

  # Determine subjects by group (same logic as add_subjects())
  all_participants <- .bidser_participants(bids)
  if (length(all_participants) == 0) stop("No participants found in BIDS project", call. = FALSE)

  group_list <- NULL
  group_names <- NULL

  if (is.null(groups) || identical(groups, "all")) {
    group_list <- list(all = all_participants)
    group_names <- "all"
  } else if (is.character(groups)) {
    part_df <- bids$part_df
    if (is.null(part_df) || !is.data.frame(part_df)) stop("BIDS participants.tsv not available in bidser project", call. = FALSE)

    if (!is.null(group_col)) {
      group_col <- as.character(group_col)[1]
      if (is.na(group_col) || !nzchar(group_col) || !group_col %in% names(part_df)) {
        stop("group_col must be a column name in participants.tsv", call. = FALSE)
      }
    } else {
      group_col <- NULL
      for (col in names(part_df)) {
        if (col != "participant_id" && all(groups %in% unique(part_df[[col]]))) {
          group_col <- col
          break
        }
      }
    }
    if (is.null(group_col)) {
      stop(
        "Could not find group column in participants.tsv matching: ",
        paste(groups, collapse = ", "),
        "\nAvailable columns: ", paste(names(part_df), collapse = ", "),
        call. = FALSE
      )
    }

    group_list <- lapply(groups, function(g) {
      ids <- part_df$participant_id[part_df[[group_col]] == g]
      gsub("^sub-", "", as.character(ids))
    })
    names(group_list) <- groups
    group_names <- groups
  } else {
    stop("groups must be NULL, 'all', or a character vector", call. = FALSE)
  }

  # Compute mask if needed
  if (is.null(spec$mask)) {
    all_masks <- list()
    subjects_all <- unique(unname(unlist(group_list)))
    for (subj in subjects_all) {
      mask_args <- list(
        bids,
        subid = paste0("^", subj, "$"),
        full_path = TRUE
      )
      if (!is.null(space) && nzchar(as.character(space)[1])) {
        mask_args$space <- space
      }
      mask_args <- c(mask_args, list(...))

      mask_files <- do.call(.bidser_mask_files, mask_args)
      if (!is.null(mask_files) && length(mask_files) > 0) {
        all_masks[[length(all_masks) + 1L]] <- neuroim2::read_vol(mask_files[1]) > 0
      }
    }
    if (length(all_masks) == 0) {
      stop("Could not locate any mask files via bidser::mask_files(); provide spec$mask.", call. = FALSE)
    }

    combined_mask <- switch(
      mask_method,
      intersection = Reduce(`&`, all_masks),
      union = Reduce(`|`, all_masks),
      first = all_masks[[1]],
      all_masks[[1]]
    )

    spec$mask <- neuroim2::NeuroVol(
      as.numeric(combined_mask),
      neuroim2::space(all_masks[[1]])
    )
  }

  volspec <- NULL
  if (!is.null(volumes)) {
    if (is.character(volumes) && length(volumes) == 1L) {
      volspec <- gsub("\\s+", "", volumes)
      if (!nzchar(volspec)) volspec <- NULL
    } else {
      volumes <- suppressWarnings(as.integer(volumes))
      volumes <- volumes[!is.na(volumes)]
      if (length(volumes) == 0L) {
        volspec <- NULL
      } else {
        volspec <- paste(volumes, collapse = ",")
      }
    }
  }

  extract_key <- function(path, key) {
    bn <- basename(path)
    m <- regexec(paste0("_", key, "-([^_]+)"), bn, perl = TRUE)
    reg <- regmatches(bn, m)[[1]]
    if (length(reg) == 0L) return(NA_character_)
    reg[2]
  }

  condition_from_path <- function(path) {
    vals <- vapply(condition_keys, function(k) extract_key(path, k), character(1))
    if (anyNA(vals) || any(!nzchar(vals))) {
      if (isTRUE(strict)) {
        stop("Could not extract condition from file: ", basename(path), call. = FALSE)
      }
      return(NA_character_)
    }
    paste(vals, collapse = "_")
  }

  rows <- list()
  hit_counts <- integer(0)
  names(hit_counts) <- character(0)
  for (g in group_names) {
    subj_ids <- unique(group_list[[g]])
    subj_ids <- gsub("^sub-", "", as.character(subj_ids))

    for (subj in subj_ids) {
      search_args <- list(
        bids,
        regex = paste0("^sub-", subj, ".*", file_regex),
        full_path = TRUE
      )
      if (!is.null(task) && nzchar(as.character(task)[1])) {
        search_args$task <- task
      }
      if (!is.null(space) && nzchar(as.character(space)[1])) {
        search_args$space <- space
      }
      search_args <- c(search_args, list(...))

      files <- do.call(.bidser_search_files, search_args)
      hit_counts[subj] <- if (is.null(files)) 0L else length(files)
      if (is.null(files) || length(files) == 0) next

      for (f in files) {
        cond <- condition_from_path(f)
        if (is.na(cond) || !nzchar(cond)) next

        f2 <- if (!is.null(volspec)) paste0(f, "[", volspec, "]") else f

        rows[[length(rows) + 1L]] <- data.frame(
          group = g,
          subject = subj,
          condition = cond,
          file = f2,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    tried <- paste(names(hit_counts), hit_counts, sep = "=", collapse = ", ")
    rx_example <- paste0("^sub-", utils::head(names(hit_counts), 1), ".*", file_regex)
    msg <- paste0(
      "No matching files found for the requested BIDS filters. ",
      "Example regex: ", rx_example,
      if (nzchar(tried)) paste0(" (hits per subject: ", tried, ")") else ""
    )
    stop(msg, call. = FALSE)
  }

  manifest <- do.call(rbind, rows)

  if (isTRUE(strict)) {
    key <- paste(manifest$group, manifest$subject, manifest$condition, sep = "\t")
    if (anyDuplicated(key)) {
      stop(
        "Duplicate (group, subject, condition) rows found. ",
        "Consider adding another condition key (e.g., 'run') or refining file_regex.",
        call. = FALSE
      )
    }
  }

  spec <- add_subjects_manifest(
    spec = spec,
    manifest = manifest,
    mask = spec$mask,
    base_dir = bids_dir
  )

  spec$.bids <- bids
  spec$.bids_raw <- FALSE
  spec
}
