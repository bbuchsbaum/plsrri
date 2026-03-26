#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required for scripts/dev-plscli.R", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE, export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
plscli_main(args)
