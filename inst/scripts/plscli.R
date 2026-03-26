#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages(library(plsrri))
plsrri::plscli_main(args)
