# Tests for R/plscli.R

test_that(".plscli_usage returns a non-empty character string", {
  result <- plsrri:::.plscli_usage()
  expect_type(result, "character")
  expect_true(nzchar(result))
  expect_true(grepl("Usage", result))
  expect_true(grepl("Commands", result))
})

test_that(".plscli_parse_args returns help for --help flag", {
  result <- plsrri:::.plscli_parse_args(c("--help"))
  expect_equal(result$command, "help")
})

test_that(".plscli_parse_args returns help for -h flag", {
  result <- plsrri:::.plscli_parse_args(c("-h"))
  expect_equal(result$command, "help")
})

test_that(".plscli_parse_args returns help for empty args", {
  result <- plsrri:::.plscli_parse_args(character(0))
  expect_equal(result$command, "help")
})

test_that(".plscli_parse_args parses command with --spec option", {
  result <- plsrri:::.plscli_parse_args(c("validate", "--spec", "spec.yml"))
  expect_equal(result$command, "validate")
  expect_equal(result$options$spec, "spec.yml")
})

test_that(".plscli_parse_args parses discover command", {
  result <- plsrri:::.plscli_parse_args(c("discover", "--spec", "spec.yml"))
  expect_equal(result$command, "discover")
  expect_equal(result$options$spec, "spec.yml")
})

test_that(".plscli_parse_args parses multiple options", {
  result <- plsrri:::.plscli_parse_args(
    c("report", "--spec", "spec.yml", "--format", "pdf", "--title", "My Report")
  )
  expect_equal(result$command, "report")
  expect_equal(result$options$spec, "spec.yml")
  expect_equal(result$options$format, "pdf")
  expect_equal(result$options$title, "My Report")
})

test_that(".plscli_parse_args treats --help anywhere in args as help command", {
  # --help anywhere in the argument vector triggers the help path
  result <- plsrri:::.plscli_parse_args(c("validate", "--help"))
  expect_equal(result$command, "help")
})

test_that(".plscli_parse_args errors on unexpected positional argument", {
  expect_error(
    plsrri:::.plscli_parse_args(c("validate", "unexpected_positional")),
    "Unexpected positional argument"
  )
})

test_that(".plscli_parse_args errors when option has no value", {
  expect_error(
    plsrri:::.plscli_parse_args(c("validate", "--spec")),
    "Option requires a value"
  )
})

test_that(".plscli_print_table prints a data.frame without error", {
  df <- data.frame(x = 1:3, y = letters[1:3], stringsAsFactors = FALSE)
  out <- capture.output(plsrri:::.plscli_print_table(df))
  expect_true(length(out) > 0)
})

test_that(".plscli_print_table prints a scalar without error", {
  out <- capture.output(plsrri:::.plscli_print_table(42L))
  expect_true(length(out) > 0)
})

test_that(".plscli_print_table returns input invisibly for data.frame", {
  df <- data.frame(x = 1:3)
  result <- plsrri:::.plscli_print_table(df)
  expect_identical(result, df)
})

test_that("plscli_main with --help prints usage and returns NULL invisibly", {
  out <- capture.output(result <- plscli_main(c("--help")))
  expect_null(result)
  expect_true(any(grepl("Usage", out)))
})

test_that("plscli_main with unknown command errors", {
  expect_error(
    plscli_main(c("nonexistent-command", "--spec", "dummy.yml")),
    "Unknown plscli command"
  )
})

test_that("plscli_main errors when --spec missing for spec-requiring commands", {
  expect_error(
    plscli_main(c("validate")),
    "--spec is required"
  )
})

test_that("plscli_main errors when --input missing for report command", {
  expect_error(
    plscli_main(c("report")),
    "--input or --spec is required"
  )
})
