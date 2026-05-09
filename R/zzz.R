#' @keywords internal
.onLoad <- function(libname, pkgname) {
  .register_pls_methods()
  .plsrri_register_multifer_adapters()
}
