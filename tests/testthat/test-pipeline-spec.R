# Tests for R/pipeline_spec.R

# --- .pipeline_nested ---

test_that(".pipeline_nested retrieves nested value", {
  spec <- list(a = list(b = list(c = 42)))
  expect_equal(plsrri:::.pipeline_nested(spec, c("a", "b", "c")), 42)
})

test_that(".pipeline_nested returns default when key is missing", {
  spec <- list(a = list(b = 1))
  expect_null(plsrri:::.pipeline_nested(spec, c("a", "z")))
  expect_equal(plsrri:::.pipeline_nested(spec, c("a", "z"), default = "fallback"), "fallback")
})

test_that(".pipeline_nested returns default when path is empty list", {
  spec <- list(a = 1)
  expect_equal(plsrri:::.pipeline_nested(spec, c("missing"), default = 99), 99)
})

test_that(".pipeline_nested handles single-key path", {
  spec <- list(x = "hello")
  expect_equal(plsrri:::.pipeline_nested(spec, "x"), "hello")
})

# --- .pipeline_set_defaults ---

test_that(".pipeline_set_defaults fills in all top-level sections", {
  spec <- plsrri:::.pipeline_set_defaults(list())
  expect_true(!is.null(spec$dataset))
  expect_true(!is.null(spec$design))
  expect_true(!is.null(spec$first_level))
  expect_true(!is.null(spec$pls))
  expect_true(!is.null(spec$execution))
  expect_true(!is.null(spec$outputs))
})

test_that(".pipeline_set_defaults sets expected scalar defaults", {
  spec <- plsrri:::.pipeline_set_defaults(list())
  expect_equal(spec$design$block, "~ run")
  expect_equal(spec$first_level$strategy, "runwise")
  expect_equal(spec$first_level$nchunks, 1L)
  expect_false(spec$first_level$progress)
  expect_true(spec$first_level$save_fit)
  expect_equal(spec$first_level$output$type, "estimates")
  expect_equal(spec$pls$method, "task")
  expect_equal(spec$execution$mode, "local")
  expect_equal(spec$execution$parallelism, 1L)
})

test_that(".pipeline_set_defaults does not overwrite existing values", {
  spec <- list(
    design = list(block = "~ session"),
    first_level = list(strategy = "subjectwise", nchunks = 4L)
  )
  result <- plsrri:::.pipeline_set_defaults(spec)
  expect_equal(result$design$block, "~ session")
  expect_equal(result$first_level$strategy, "subjectwise")
  expect_equal(result$first_level$nchunks, 4L)
})

# --- .pipeline_read_table_file ---

test_that(".pipeline_read_table_file reads CSV", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:3, b = letters[1:3]), tmp, row.names = FALSE)
  on.exit(unlink(tmp))
  result <- plsrri:::.pipeline_read_table_file(tmp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3L)
  expect_true("a" %in% names(result))
})

test_that(".pipeline_read_table_file reads TSV", {
  tmp <- tempfile(fileext = ".tsv")
  write.table(data.frame(x = 1:4, y = c("a","b","c","d")),
              tmp, sep = "\t", row.names = FALSE, quote = FALSE)
  on.exit(unlink(tmp))
  result <- plsrri:::.pipeline_read_table_file(tmp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4L)
})

test_that(".pipeline_read_table_file reads RDS", {
  tmp <- tempfile(fileext = ".rds")
  df <- data.frame(val = 1:5)
  saveRDS(df, tmp)
  on.exit(unlink(tmp))
  result <- plsrri:::.pipeline_read_table_file(tmp)
  expect_equal(result, df)
})

test_that(".pipeline_read_table_file errors on unsupported extension", {
  tmp <- tempfile(fileext = ".xlsx")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(plsrri:::.pipeline_read_table_file(tmp), "Unsupported")
})

# --- write_pipeline_template ---

test_that("write_pipeline_template creates a YAML file", {
  skip_if_not_installed("yaml")
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  result <- write_pipeline_template(tmp)
  expect_equal(result, tmp)
  expect_true(file.exists(tmp))
})

test_that("write_pipeline_template writes parseable YAML", {
  skip_if_not_installed("yaml")
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  write_pipeline_template(tmp)
  parsed <- yaml::read_yaml(tmp)
  expect_true(is.list(parsed))
  expect_true(!is.null(parsed$dataset))
  expect_true(!is.null(parsed$design))
  expect_true(!is.null(parsed$pls))
})

test_that("write_pipeline_template creates parent directories as needed", {
  skip_if_not_installed("yaml")
  tmp_dir <- file.path(tempdir(), paste0("pls_test_", as.integer(Sys.time())))
  tmp <- file.path(tmp_dir, "template.yaml")
  on.exit(unlink(tmp_dir, recursive = TRUE))
  write_pipeline_template(tmp)
  expect_true(file.exists(tmp))
})

test_that("write_pipeline_template template has required keys", {
  skip_if_not_installed("yaml")
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  write_pipeline_template(tmp)
  parsed <- yaml::read_yaml(tmp)
  expect_true(!is.null(parsed$dataset$bids_dir))
  expect_true(!is.null(parsed$design$formula))
  expect_true(!is.null(parsed$first_level$strategy))
  expect_true(!is.null(parsed$pls$method))
  expect_true(!is.null(parsed$execution$mode))
})
