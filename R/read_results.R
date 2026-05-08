#' Read a PLS Result From Disk
#'
#' @description
#' Symmetric reader for files written by [`write_results()`]. Auto-detects
#' the on-disk format by file content (HDF5 magic bytes vs. R serialized
#' format), so a path written via either format round-trips correctly.
#'
#' For HDF5 inputs, the schema (see `?plsrri-hdf5`) supports lossless
#' round-trip of all six PLS methods including multiblock layouts,
#' permutation null distributions (when retained via
#' `pls_analysis(..., keep_perm_distribution = TRUE)`), bootstrap and
#' split-half results, NeuroVol masks, and site/diagnostics metadata.
#'
#' @param file Path to a `.rds` or `.h5`/`.hdf5` file.
#'
#' @return A `pls_result` object.
#'
#' @seealso [write_results()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- pls_analysis(...)
#' write_results(fit, "fit.h5", format = "hdf5")
#' fit2 <- read_results("fit.h5")
#' identical(fit, fit2)
#' }
read_results <- function(file) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file, call. = FALSE)
  }
  if (.is_hdf5_file(file)) {
    return(.plsrri_read_pls_hdf5(file))
  }
  # Default to RDS
  obj <- readRDS(file)
  obj
}

# HDF5 files start with the 8-byte signature: \x89 H D F \r \n \x1a \n
.is_hdf5_file <- function(file) {
  con <- file(file, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- tryCatch(readBin(con, what = "raw", n = 8L), error = function(e) raw(0))
  if (length(bytes) < 8L) return(FALSE)
  identical(
    as.integer(bytes[1:8]),
    c(0x89L, 0x48L, 0x44L, 0x46L, 0x0DL, 0x0AL, 0x1AL, 0x0AL)
  )
}
