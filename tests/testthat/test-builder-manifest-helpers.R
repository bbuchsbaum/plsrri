# Tests for builder_manifest.R internal helpers (no neuroim2 required)

# ---- .read_manifest_file() ----

test_that(".read_manifest_file returns data.frame unchanged", {
  df <- data.frame(subject = "s1", condition = "A", file = "x.nii")
  result <- plsrri:::.read_manifest_file(df)
  expect_identical(result, df)
})

test_that(".read_manifest_file reads a CSV temp file", {
  df <- data.frame(subject = c("s1", "s2"), condition = c("A", "B"),
                   file = c("a.nii", "b.nii"), stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".csv")
  write.csv(df, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  result <- plsrri:::.read_manifest_file(tmp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_equal(result$subject, c("s1", "s2"))
})

test_that(".read_manifest_file reads a TSV temp file", {
  df <- data.frame(subject = c("s1", "s2"), condition = c("A", "B"),
                   file = c("a.nii", "b.nii"), stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".tsv")
  write.table(df, tmp, sep = "\t", row.names = FALSE, quote = FALSE)
  on.exit(unlink(tmp))

  result <- plsrri:::.read_manifest_file(tmp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
})

test_that(".read_manifest_file reads an RDS temp file", {
  df <- data.frame(subject = "s1", condition = "A", file = "x.nii",
                   stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".rds")
  saveRDS(df, tmp)
  on.exit(unlink(tmp))

  result <- plsrri:::.read_manifest_file(tmp)
  expect_identical(result, df)
})

test_that(".read_manifest_file errors on missing file", {
  expect_error(
    plsrri:::.read_manifest_file("/nonexistent/path/manifest.csv"),
    "does not exist"
  )
})

test_that(".read_manifest_file errors on unsupported extension", {
  tmp <- tempfile(fileext = ".xlsx")
  writeLines("dummy", tmp)
  on.exit(unlink(tmp))
  expect_error(plsrri:::.read_manifest_file(tmp), "Unsupported manifest extension")
})

test_that(".read_manifest_file errors on non-character non-data.frame input", {
  expect_error(plsrri:::.read_manifest_file(42), "manifest must be a data.frame")
})

# ---- .parse_int_spec() ----

test_that(".parse_int_spec parses single integer", {
  expect_equal(plsrri:::.parse_int_spec("5"), 5L)
})

test_that(".parse_int_spec parses range", {
  expect_equal(plsrri:::.parse_int_spec("1:4"), 1:4)
})

test_that(".parse_int_spec parses comma-separated list", {
  expect_equal(plsrri:::.parse_int_spec("1,3,5"), c(1L, 3L, 5L))
})

test_that(".parse_int_spec parses mixed range and list", {
  expect_equal(plsrri:::.parse_int_spec("1:3,6:8"), c(1L, 2L, 3L, 6L, 7L, 8L))
})

test_that(".parse_int_spec returns integer(0) for empty string", {
  expect_equal(plsrri:::.parse_int_spec(""), integer(0))
})

test_that(".parse_int_spec errors on invalid spec", {
  expect_error(plsrri:::.parse_int_spec("1-3"), "Invalid integer spec")
})

# ---- .parse_file_ref() ----

test_that(".parse_file_ref returns path with NULL volumes when no bracket", {
  result <- plsrri:::.parse_file_ref("path/to/file.nii.gz")
  expect_equal(result$path, "path/to/file.nii.gz")
  expect_null(result$volumes)
})

test_that(".parse_file_ref parses single volume selector", {
  result <- plsrri:::.parse_file_ref("path/to/file.nii[3]")
  expect_equal(result$path, "path/to/file.nii")
  expect_equal(result$volumes, 3L)
})

test_that(".parse_file_ref parses range volume selector", {
  result <- plsrri:::.parse_file_ref("/data/sub.nii.gz[1:8]")
  expect_equal(result$path, "/data/sub.nii.gz")
  expect_equal(result$volumes, 1:8)
})

test_that(".parse_file_ref parses comma-separated volume selector", {
  result <- plsrri:::.parse_file_ref("file.nii[1,3,5]")
  expect_equal(result$path, "file.nii")
  expect_equal(result$volumes, c(1L, 3L, 5L))
})


# ---- .normalize_manifest() ----

test_that(".normalize_manifest adds canonical dot-columns", {
  df <- data.frame(
    subject   = c("s1", "s1"),
    condition = c("A", "B"),
    file      = c("f1.nii", "f2.nii"),
    stringsAsFactors = FALSE
  )
  result <- plsrri:::.normalize_manifest(
    df,
    subject_col   = "subject",
    condition_col = "condition",
    file_col      = "file"
  )

  expect_true(all(c(".subject", ".condition", ".file_raw", ".group", ".lag") %in% names(result)))
  expect_equal(result$.subject, c("s1", "s1"))
  expect_equal(as.character(result$.condition), c("A", "B"))
  expect_equal(result$.group, c("all", "all"))
  expect_true(all(is.na(result$.lag)))
})

test_that(".normalize_manifest uses group column when present", {
  df <- data.frame(
    subject   = c("s1", "s2"),
    condition = c("A", "A"),
    file      = c("f1.nii", "f2.nii"),
    group     = c("g1", "g2"),
    stringsAsFactors = FALSE
  )
  result <- plsrri:::.normalize_manifest(
    df,
    subject_col   = "subject",
    condition_col = "condition",
    file_col      = "file",
    group_col     = "group"
  )
  expect_equal(result$.group, c("g1", "g2"))
})

test_that(".normalize_manifest errors when required columns missing", {
  df <- data.frame(subject = "s1", file = "f.nii", stringsAsFactors = FALSE)
  expect_error(
    plsrri:::.normalize_manifest(df, subject_col = "subject",
                                 condition_col = "condition",
                                 file_col = "file"),
    "subject, condition, and file"
  )
})

test_that(".normalize_manifest errors when volume and bracket spec both present", {
  df <- data.frame(
    subject   = "s1",
    condition = "A",
    file      = "f.nii[1:3]",  # bracket spec
    volume    = "1",            # also volume column
    stringsAsFactors = FALSE
  )
  expect_error(
    plsrri:::.normalize_manifest(
      df,
      subject_col   = "subject",
      condition_col = "condition",
      file_col      = "file",
      volume_col    = "volume"
    ),
    "either via.*not both"
  )
})
