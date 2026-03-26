# Tests for R/pipeline_core.R

# --- .pipeline_normalize_subject ---

test_that(".pipeline_normalize_subject strips sub- prefix", {
  expect_equal(plsrri:::.pipeline_normalize_subject("sub-001"), "001")
  expect_equal(plsrri:::.pipeline_normalize_subject("sub-abc"), "abc")
})

test_that(".pipeline_normalize_subject leaves non-prefixed strings unchanged", {
  expect_equal(plsrri:::.pipeline_normalize_subject("001"), "001")
  expect_equal(plsrri:::.pipeline_normalize_subject("patient01"), "patient01")
})

test_that(".pipeline_normalize_subject coerces to character", {
  expect_equal(plsrri:::.pipeline_normalize_subject(123), "123")
})

test_that(".pipeline_normalize_subject vectorizes", {
  result <- plsrri:::.pipeline_normalize_subject(c("sub-01", "sub-02", "03"))
  expect_equal(result, c("01", "02", "03"))
})

# --- .pipeline_extract_entity ---

test_that(".pipeline_extract_entity extracts known BIDS entity", {
  path <- "/data/sub-01/func/sub-01_task-rest_run-02_bold.nii.gz"
  expect_equal(plsrri:::.pipeline_extract_entity(path, "run"), "02")
  expect_equal(plsrri:::.pipeline_extract_entity(path, "task"), "rest")
})

test_that(".pipeline_extract_entity returns NA when entity absent", {
  path <- "/data/sub-01_task-rest_bold.nii.gz"
  expect_equal(plsrri:::.pipeline_extract_entity(path, "run"), NA_character_)
})

test_that(".pipeline_extract_entity operates on basename only", {
  # 'run' appears in the directory name but not in the filename itself
  path <- "/data/run-99/sub-01_bold.nii.gz"
  expect_equal(plsrri:::.pipeline_extract_entity(path, "run"), NA_character_)
})

# --- .pipeline_safe_name ---

test_that(".pipeline_safe_name replaces special characters with hyphens", {
  expect_equal(plsrri:::.pipeline_safe_name("hello world"), "hello-world")
  expect_equal(plsrri:::.pipeline_safe_name("foo/bar:baz"), "foo-bar-baz")
})

test_that(".pipeline_safe_name leaves alphanumeric, dot, hyphen unchanged", {
  expect_equal(plsrri:::.pipeline_safe_name("abc.XYZ-123"), "abc.XYZ-123")
})

test_that(".pipeline_safe_name collapses multiple special chars to one hyphen", {
  expect_equal(plsrri:::.pipeline_safe_name("a  b"), "a-b")
})

test_that(".pipeline_safe_name coerces to character", {
  expect_equal(plsrri:::.pipeline_safe_name(42), "42")
})

# --- .pipeline_order_runs ---

test_that(".pipeline_order_runs returns single path unchanged", {
  path <- "/data/sub-01_run-01_bold.nii.gz"
  expect_equal(plsrri:::.pipeline_order_runs(path), path)
})

test_that(".pipeline_order_runs returns single no-run path unchanged", {
  path <- "/data/sub-01_bold.nii.gz"
  expect_equal(plsrri:::.pipeline_order_runs(path), path)
})

# --- .pipeline_relativize_path and .pipeline_resolve_path ---

test_that(".pipeline_relativize_path makes absolute paths relative to root", {
  root <- "/tmp/myproject"
  path <- "/tmp/myproject/data/file.txt"
  result <- plsrri:::.pipeline_relativize_path(path, root)
  expect_equal(result, "data/file.txt")
})

test_that(".pipeline_relativize_path leaves paths outside root unchanged", {
  root <- "/tmp/myproject"
  path <- "/other/dir/file.txt"
  result <- plsrri:::.pipeline_relativize_path(path, root)
  expect_equal(result, path)
})

test_that(".pipeline_relativize_path handles NA and empty string", {
  root <- "/tmp/myproject"
  expect_equal(plsrri:::.pipeline_relativize_path(NA_character_, root), NA_character_)
  expect_equal(plsrri:::.pipeline_relativize_path("", root), "")
})

test_that(".pipeline_resolve_path converts relative paths to absolute", {
  root <- "/tmp/myproject"
  result <- plsrri:::.pipeline_resolve_path("data/file.txt", root)
  expect_equal(result, "/tmp/myproject/data/file.txt")
})

test_that(".pipeline_resolve_path leaves already-absolute paths unchanged", {
  root <- "/tmp/myproject"
  abs_path <- "/other/dir/file.txt"
  result <- plsrri:::.pipeline_resolve_path(abs_path, root)
  expect_equal(result, abs_path)
})

test_that(".pipeline_resolve_path handles NA and empty string", {
  root <- "/tmp/myproject"
  expect_equal(plsrri:::.pipeline_resolve_path(NA_character_, root), NA_character_)
  expect_equal(plsrri:::.pipeline_resolve_path("", root), "")
})

test_that(".pipeline_relativize_path and .pipeline_resolve_path are inverses", {
  root <- "/tmp/myproject"
  original <- "/tmp/myproject/sub-01/func/file.nii.gz"
  rel <- plsrri:::.pipeline_relativize_path(original, root)
  restored <- plsrri:::.pipeline_resolve_path(rel, root)
  expect_equal(normalizePath(restored, winslash = "/", mustWork = FALSE),
               normalizePath(original, winslash = "/", mustWork = FALSE))
})

# --- .pipeline_relativize_df and .pipeline_resolve_df ---

test_that(".pipeline_relativize_df relativizes specified columns", {
  root <- "/tmp/myproject"
  df <- data.frame(
    path = "/tmp/myproject/data/file.txt",
    other = "unchanged",
    stringsAsFactors = FALSE
  )
  result <- plsrri:::.pipeline_relativize_df(df, root, cols = "path")
  expect_equal(result$path, "data/file.txt")
  expect_equal(result$other, "unchanged")
})

test_that(".pipeline_relativize_df ignores columns not in df", {
  root <- "/tmp/myproject"
  df <- data.frame(a = "/tmp/myproject/x.txt", stringsAsFactors = FALSE)
  # 'nonexistent' col should be silently ignored
  result <- plsrri:::.pipeline_relativize_df(df, root, cols = c("a", "nonexistent"))
  expect_equal(result$a, "x.txt")
})

test_that(".pipeline_resolve_df resolves specified columns", {
  root <- "/tmp/myproject"
  df <- data.frame(
    path = "data/file.txt",
    other = "unchanged",
    stringsAsFactors = FALSE
  )
  result <- plsrri:::.pipeline_resolve_df(df, root, cols = "path")
  expect_equal(result$path, "/tmp/myproject/data/file.txt")
  expect_equal(result$other, "unchanged")
})
