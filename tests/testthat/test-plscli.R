test_that(".plscli_usage returns global and command-specific help", {
  global <- plsrri:::.plscli_usage()
  report <- plsrri:::.plscli_usage("report")

  expect_type(global, "character")
  expect_match(global, "Usage:")
  expect_match(global, "plscli help \\[command\\]")
  expect_match(report, "plscli report")
  expect_match(report, "--no-open", fixed = TRUE)
})

test_that(".plscli_parse_args supports help forms and key=value options", {
  expect_equal(plsrri:::.plscli_parse_args(character(0))$command, "help")
  expect_equal(plsrri:::.plscli_parse_args(c("--help"))$command, "help")
  expect_equal(plsrri:::.plscli_parse_args(c("help", "report"))$topic, "report")
  expect_equal(plsrri:::.plscli_parse_args(c("report", "--help"))$topic, "report")

  parsed <- plsrri:::.plscli_parse_args(c(
    "report",
    "--input=/tmp/artifacts",
    "--format=pdf",
    "--json",
    "--no-open"
  ))

  expect_equal(parsed$command, "report")
  expect_equal(parsed$options$input, "/tmp/artifacts")
  expect_equal(parsed$options$format, "pdf")
  expect_true(parsed$options$json)
  expect_false(parsed$options$open)
})

test_that(".plscli_parse_args rejects unknown options and missing values", {
  expect_error(
    plsrri:::.plscli_parse_args(c("validate", "--bogus")),
    "Unknown option"
  )

  expect_error(
    plsrri:::.plscli_parse_args(c("template", "--out")),
    "Option requires a value"
  )

  expect_error(
    plsrri:::.plscli_parse_args(c("validate", "unexpected")),
    "Unexpected positional argument"
  )
})

test_that(".plscli_print_table prints common result shapes", {
  df <- data.frame(x = 1:3, y = letters[1:3], stringsAsFactors = FALSE)

  expect_true(length(capture.output(plsrri:::.plscli_print_table(df))) > 0L)
  expect_true(length(capture.output(plsrri:::.plscli_print_table(42L))) > 0L)
  expect_identical(plsrri:::.plscli_print_table(df), df)
})

test_that("plscli_main returns status 0 for help and template", {
  out <- capture.output(status <- plscli_main(c("--help")))
  expect_identical(status, 0L)
  expect_true(any(grepl("Usage:", out)))

  template_path <- tempfile(fileext = ".yml")
  status <- plscli_main(c("template", "--out", template_path))
  expect_identical(status, 0L)
  expect_true(file.exists(template_path))
})

test_that("plscli_main returns usage error status for parser and dispatch failures", {
  expect_identical(plscli_main(c("validate")), 2L)
  expect_identical(plscli_main(c("nonexistent-command", "--spec", "dummy.yml")), 2L)
  expect_identical(plscli_main(c("report")), 2L)
})

test_that("plscli_main returns domain failure status for invalid specs", {
  skip_if_not_installed("yaml")

  spec_path <- tempfile(fileext = ".yml")
  yaml::write_yaml(
    list(
      dataset = list(
        bids_dir = tempfile("missing-bids-")
      )
    ),
    spec_path
  )

  expect_identical(plscli_main(c("validate", "--spec", spec_path)), 1L)
})

test_that("plscli_main emits parseable json output", {
  skip_if_not_installed("jsonlite")

  template_path <- tempfile(fileext = ".yml")
  out <- capture.output(status <- plscli_main(c("template", "--out", template_path, "--json")))

  expect_identical(status, 0L)
  payload <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  expect_identical(payload$command, "template")
  expect_identical(basename(payload$output), basename(template_path))
})

test_that("install_cli copies wrapper and refuses overwrite by default", {
  dest <- tempfile("plsrri-cli-")
  dir.create(dest)

  installed <- install_cli(dest_dir = dest)
  wrapper <- file.path(dest, "plscli")

  expect_length(installed, 1L)
  expect_true(file.exists(wrapper))
  expect_match(paste(readLines(wrapper, warn = FALSE), collapse = "\n"), "plsrri::plscli_main", fixed = TRUE)

  expect_error(
    install_cli(dest_dir = dest),
    "Refusing to overwrite existing command"
  )
})

test_that("exec/plscli wrapper runs help successfully", {
  wrapper <- normalizePath(
    file.path(testthat::test_path("..", ".."), "exec", "plscli"),
    winslash = "/",
    mustWork = TRUE
  )
  cmd <- file.path(R.home("bin"), "Rscript")
  out <- system2(cmd, c(wrapper, "--help"), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status") %||% 0L

  expect_identical(as.integer(status), 0L)
  expect_true(any(grepl("Usage:", out)))
})
