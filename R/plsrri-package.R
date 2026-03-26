#' plsrri: Partial Least Squares for Neuroimaging
#'
#' @description
#' A modern R implementation of the McIntosh Lab PLS package for neuroimaging
#' analysis. Supports task PLS, behavior PLS, and multiblock PLS methods with
#' permutation testing, bootstrap confidence intervals, and split-half
#' validation.
#'
#' @section PLS Methods:
#' The package supports six PLS analysis methods:
#'
#' \describe{
#'   \item{Method 1: Mean-Centering Task PLS}{
#'     Identifies patterns of brain activity that optimally distinguish
#'     experimental conditions. The most common approach for task-based fMRI.
#'   }
#'   \item{Method 2: Non-Rotated Task PLS}{
#'     Tests specific hypotheses about condition differences using predefined
#'     contrasts. Use when you have specific predictions.
#'   }
#'   \item{Method 3: Regular Behavior PLS}{
#'     Identifies brain patterns that maximally correlate with behavioral
#'     measures. For brain-behavior relationship analysis.
#'   }
#'   \item{Method 4: Regular Multiblock PLS}{
#'     Combines task effects and behavior-brain correlations in a single
#'     analysis. Examines both task and behavior simultaneously.
#'   }
#'   \item{Method 5: Non-Rotated Behavior PLS}{
#'     Tests specific hypotheses about behavior-brain relationships using
#'     predefined contrasts.
#'   }
#'   \item{Method 6: Non-Rotated Multiblock PLS}{
#'     Tests combined hypotheses about task effects and behavior-brain
#'     relationships.
#'   }
#' }
#'
#' @section Builder API:
#' The recommended way to run analyses is using the builder API:
#'
#' ```r
#' result <- pls_spec(mask = brain_mask) |>
#'   add_subjects(bids_dir, groups = c("control", "patient")) |>
#'   add_conditions(events_df) |>
#'   configure(method = "task", nperm = 1000, nboot = 500) |>
#'   run()
#' ```
#'
#' @section Key Functions:
#' \describe{
#'   \item{Analysis}{
#'     \code{\link{pls_analysis}}: Core analysis function
#'     \code{\link{pls_spec}}: Create analysis specification
#'   }
#'   \item{Builder API}{
#'     \code{\link{add_subjects}}: Add subject data
#'     \code{\link{add_conditions}}: Specify conditions
#'     \code{\link{add_behavior}}: Add behavioral measures
#'     \code{\link{add_design}}: Add design contrasts
#'     \code{\link{configure}}: Set analysis parameters
#'     \code{\link{run}}: Execute analysis
#'   }
#'   \item{Results}{
#'     \code{\link{salience}}: Extract brain loadings
#'     \code{\link{bsr}}: Extract bootstrap ratios
#'     \code{\link{scores}}: Extract brain/design scores
#'     \code{\link{significance}}: Get permutation p-values
#'     \code{\link{confidence}}: Get bootstrap CIs
#'   }
#'   \item{Visualization}{
#'     \code{\link{plot_brain}}: Brain slice plots
#'     \code{\link{plot_scores}}: Score plots
#'     \code{\link{plot_loadings}}: Loading bar plots
#'     \code{\link{render_report}}: Generate Quarto report
#'   }
#' }
#'
#' @section Integration:
#' The package integrates with:
#' \itemize{
#'   \item \pkg{neuroim2}: Imaging data structures (NeuroVol, NeuroVec)
#'   \item \pkg{bidser}: BIDS directory access and fMRIPrep derivatives
#'   \item \pkg{neurosurf}: Surface visualization (planned)
#' }
#'
#' @section Performance:
#' For large-scale analyses (500+ subjects, 10k permutations), the package
#' provides Rcpp-accelerated versions of key functions. These are used
#' automatically when available.
#'
#' @references
#' McIntosh AR, Lobaugh NJ (2004). Partial least squares analysis of
#' neuroimaging data: applications and advances. NeuroImage 23: S250-S263.
#'
#' McIntosh AR, Bookstein FL, Haxby JV, Grady CL (1996). Spatial pattern
#' analysis of functional brain images using partial least squares.
#' NeuroImage 3: 143-157.
#'
#' @name plsrri-package
#' @aliases plsrri
#'
#' @import assertthat
#' @import ggplot2
#' @importFrom grDevices colorRampPalette
#' @importFrom stats sd var cov cor quantile approx qnorm aggregate as.formula
#'   contr.helmert contr.treatment contr.poly contr.sum
#' @importFrom Matrix sparseMatrix
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#'   cli_progress_bar cli_progress_update cli_progress_done cli_h1 cli_h2 cli_text
#' @importFrom methods is
#' @importFrom utils packageVersion
#'
#' @useDynLib plsrri, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' @keywords internal
"_PACKAGE"
