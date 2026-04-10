#!/usr/bin/env Rscript

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(...) NA_character_
)
if (is.na(script_path)) {
  script_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", script_args, value = TRUE)
  if (length(file_arg)) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  }
}

script_dir <- dirname(script_path)
repo_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
wrapper <- file.path(repo_root, "exec", "plscli")

if (!file.exists(wrapper)) {
  stop("Could not locate exec/plscli from inst/scripts/plscli.R", call. = FALSE)
}

source(wrapper, local = new.env(parent = globalenv()))
