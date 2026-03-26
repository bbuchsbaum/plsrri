#' PLS Method Implementations for MvaMethod Protocol
#'
#' @description
#' Wraps the existing PLS analysis engine as MvaMethod implementations.
#' These call `pls_analysis()` unchanged, preserving MATLAB parity,
#' and map the output to the universal `mva_decomposition` shape.
#'
#' @name mva-pls-methods
NULL

# --- Base PLS Method ---

#' @keywords internal
PlsMethod <- R6::R6Class("PlsMethod",
  inherit = MvaMethod,
  public = list(
    #' @field pls_method_int The integer method code (1-6) for pls_analysis()
    pls_method_int = NULL,

    initialize = function(name, label, pls_method_int, capabilities = list()) {
      self$pls_method_int <- pls_method_int
      super$initialize(
        name = name,
        label = label,
        family = "pls",
        capabilities = utils::modifyList(
          list(component_name = "LV", importance_name = "Singular Value"),
          capabilities
        )
      )
    },

    fit = function(spec, progress = TRUE) {
      # Extract PLS-specific config from spec
      # Supports both pls_spec (native fields) and mva_spec (method_config)
      cfg <- spec$method_config %||% list()

      raw <- pls_analysis(
        datamat_lst        = spec$datamat_lst,
        num_subj_lst       = spec$num_subj_lst,
        num_cond           = spec$num_cond %||% cfg$num_cond,
        method             = self$pls_method_int,
        num_perm           = spec$num_perm %||% spec$nperm %||% 0L,
        num_boot           = spec$num_boot %||% spec$nboot %||% 0L,
        num_split          = spec$num_split %||% spec$nsplit %||% 0L,
        clim               = spec$clim %||% 95,
        stacked_behavdata  = spec$stacked_behavdata %||% spec$behavdata,
        stacked_designdata = spec$stacked_designdata %||% spec$designdata,
        bscan              = spec$bscan %||% cfg$bscan,
        meancentering_type = spec$meancentering_type %||% cfg$meancentering_type %||% 0L,
        cormode            = spec$cormode %||% cfg$cormode %||% 0L,
        boot_type          = spec$boot_type %||% cfg$boot_type %||% "strat",
        is_struct          = spec$is_struct %||% cfg$is_struct %||% FALSE,
        progress           = progress
      )

      # Wrap as mva_result
      pls_result_to_mva_result(raw, spec = spec, method_name = self$name)
    },

    validate_spec = function(spec) {
      # Delegate to the existing PLS validation where possible
      caps <- self$capabilities
      errors <- character(0)

      if (length(spec$datamat_lst) == 0 && !isTRUE(spec$.trial_raw)) {
        errors <- c(errors, "No data matrices loaded. Use add_subjects() first.")
      }
      if (is.null(spec$num_cond) && is.null(spec$method_config$num_cond)) {
        errors <- c(errors, "Number of conditions not set. Use add_conditions() first.")
      }
      if (isTRUE(caps$needs_behavior)) {
        behav <- spec$stacked_behavdata %||% spec$behavdata
        if (is.null(behav)) {
          errors <- c(errors, paste0(self$label, " requires behavior data."))
        }
      }
      if (isTRUE(caps$needs_design)) {
        design <- spec$stacked_designdata %||% spec$designdata
        if (is.null(design)) {
          errors <- c(errors, paste0(self$label, " requires design contrasts."))
        }
      }
      if (length(errors) > 0) {
        stop(paste(c(paste0(self$label, " validation failed:"), errors),
                   collapse = "\n  - "), call. = FALSE)
      }
      invisible(TRUE)
    },

    ui_config = function() {
      list(
        analysis_stages = c("Validating", "Computing Covariance",
                           "Running SVD", "Permutation Test", "Bootstrap"),
        explore_panels = c("feature_weights", "scores", "importance",
                          "design_weights"),
        inspector_metrics = c("singular_value", "variance_explained",
                             "significance", "bootstrap_ratio"),
        component_name = "LV",
        importance_name = "Singular Value"
      )
    }
  )
)

# --- Concrete PLS Variants ---

#' @keywords internal
PlsTaskMethod <- R6::R6Class("PlsTaskMethod",
  inherit = PlsMethod,
  public = list(
    initialize = function() {
      super$initialize(
        name = "pls_task",
        label = "Mean-Centering Task PLS",
        pls_method_int = 1L,
        capabilities = list(
          needs_behavior = FALSE,
          needs_design   = FALSE
        )
      )
    }
  )
)

#' @keywords internal
PlsTaskNonrotMethod <- R6::R6Class("PlsTaskNonrotMethod",
  inherit = PlsMethod,
  public = list(
    initialize = function() {
      super$initialize(
        name = "pls_task_nonrotated",
        label = "Non-Rotated Task PLS",
        pls_method_int = 2L,
        capabilities = list(
          needs_behavior = FALSE,
          needs_design   = TRUE
        )
      )
    }
  )
)

#' @keywords internal
PlsBehaviorMethod <- R6::R6Class("PlsBehaviorMethod",
  inherit = PlsMethod,
  public = list(
    initialize = function() {
      super$initialize(
        name = "pls_behavior",
        label = "Regular Behavior PLS",
        pls_method_int = 3L,
        capabilities = list(
          needs_behavior = TRUE,
          needs_design   = FALSE
        )
      )
    }
  )
)

#' @keywords internal
PlsMultiblockMethod <- R6::R6Class("PlsMultiblockMethod",
  inherit = PlsMethod,
  public = list(
    initialize = function() {
      super$initialize(
        name = "pls_multiblock",
        label = "Regular Multiblock PLS",
        pls_method_int = 4L,
        capabilities = list(
          needs_behavior = TRUE,
          needs_design   = FALSE
        )
      )
    }
  )
)

#' @keywords internal
PlsBehaviorNonrotMethod <- R6::R6Class("PlsBehaviorNonrotMethod",
  inherit = PlsMethod,
  public = list(
    initialize = function() {
      super$initialize(
        name = "pls_behavior_nonrotated",
        label = "Non-Rotated Behavior PLS",
        pls_method_int = 5L,
        capabilities = list(
          needs_behavior = TRUE,
          needs_design   = TRUE
        )
      )
    }
  )
)

#' @keywords internal
PlsMultiblockNonrotMethod <- R6::R6Class("PlsMultiblockNonrotMethod",
  inherit = PlsMethod,
  public = list(
    initialize = function() {
      super$initialize(
        name = "pls_multiblock_nonrotated",
        label = "Non-Rotated Multiblock PLS",
        pls_method_int = 6L,
        capabilities = list(
          needs_behavior = TRUE,
          needs_design   = TRUE
        )
      )
    }
  )
)

# --- Result Conversion ---

#' Convert a pls_result to mva_result
#'
#' @description
#' Maps the native PLS result fields (u, s, v, usc, vsc) to the universal
#' `mva_decomposition` / `mva_result` shape. The original pls_result fields
#' are preserved in `extra` for backward compatibility.
#'
#' @param pls_res A `pls_result` object from `pls_analysis()`
#' @param spec The spec used (optional, for metadata)
#' @param method_name Character, method name for class dispatch
#'
#' @return An `mva_result` object
#' @export
pls_result_to_mva_result <- function(pls_res, spec = NULL, method_name = NULL) {
  stopifnot(inherits(pls_res, "pls_result"))

  method_name <- method_name %||% pls_method_int_to_name(pls_res$method)

  decomp <- new_mva_decomposition(
    feature_weights = pls_res$u,
    design_weights  = pls_res$v,
    importance      = pls_res$s,
    scores_feature  = pls_res$usc,
    scores_design   = pls_res$vsc,
    method          = method_name,
    extra = list(
      datamatcorrs_lst = pls_res$datamatcorrs_lst,
      lvcorrs          = pls_res$lvcorrs,
      TBv              = pls_res$TBv,
      TBusc            = pls_res$TBusc,
      TBvsc            = pls_res$TBvsc,
      lvintercorrs     = pls_res$lvintercorrs,
      other_input      = pls_res$other_input,
      num_subj_lst     = pls_res$num_subj_lst,
      num_cond         = pls_res$num_cond,
      bscan            = pls_res$bscan,
      stacked_designdata = pls_res$stacked_designdata,
      stacked_behavdata  = pls_res$stacked_behavdata,
      is_struct          = pls_res$is_struct,
      pls_method_int     = pls_res$method
    )
  )

  new_mva_result(
    decomposition = decomp,
    perm_result   = pls_res$perm_result,
    boot_result   = pls_res$boot_result,
    split_result  = pls_res$splithalf_result,
    spec          = spec,
    method        = method_name,
    mask          = pls_res$mask
  )
}

#' Convert an mva_result back to pls_result
#'
#' @description
#' Reconstructs a `pls_result` from an `mva_result` for backward compatibility
#' with code that accesses PLS-specific fields directly.
#'
#' @param mva_res An `mva_result` object
#' @return A `pls_result` object
#' @export
mva_result_to_pls_result <- function(mva_res) {
  stopifnot(inherits(mva_res, "mva_result"))
  d <- mva_res$decomposition
  ex <- d$extra

  new_pls_result(
    method             = ex$pls_method_int %||% 1L,
    u                  = d$feature_weights,
    s                  = d$importance,
    v                  = d$design_weights,
    usc                = d$scores_feature,
    vsc                = d$scores_design,
    datamatcorrs_lst   = ex$datamatcorrs_lst,
    lvcorrs            = ex$lvcorrs,
    perm_result        = mva_res$perm_result,
    boot_result        = mva_res$boot_result,
    splithalf_result   = mva_res$split_result,
    num_subj_lst       = ex$num_subj_lst,
    num_cond           = ex$num_cond,
    bscan              = ex$bscan,
    stacked_designdata = ex$stacked_designdata,
    stacked_behavdata  = ex$stacked_behavdata,
    other_input        = ex$other_input,
    TBv                = if (is.list(ex$TBv)) ex$TBv[[1]] else NULL,
    TBusc              = if (is.list(ex$TBusc)) ex$TBusc[[1]] else NULL,
    TBvsc              = if (is.list(ex$TBvsc)) ex$TBvsc[[1]] else NULL,
    is_struct          = ex$is_struct %||% FALSE,
    mask               = mva_res$mask
  )
}

# --- Method Registration ---

#' Register all PLS method variants
#'
#' @description
#' Called during package load to register all 6 PLS variants in the
#' method registry. Each gets its canonical name plus aliases for
#' backward compatibility.
#'
#' @keywords internal
.register_pls_methods <- function() {
  register_method("pls_task", PlsTaskMethod$new(),
                  aliases = c("task", "mean_centering_task", "1"))
  register_method("pls_task_nonrotated", PlsTaskNonrotMethod$new(),
                  aliases = c("task_nonrotated", "non_rotated_task", "2"))
  register_method("pls_behavior", PlsBehaviorMethod$new(),
                  aliases = c("behavior", "regular_behavior", "3"))
  register_method("pls_multiblock", PlsMultiblockMethod$new(),
                  aliases = c("multiblock", "regular_multiblock", "4"))
  register_method("pls_behavior_nonrotated", PlsBehaviorNonrotMethod$new(),
                  aliases = c("behavior_nonrotated", "non_rotated_behavior", "5"))
  register_method("pls_multiblock_nonrotated", PlsMultiblockNonrotMethod$new(),
                  aliases = c("multiblock_nonrotated", "non_rotated_multiblock", "6"))
}
