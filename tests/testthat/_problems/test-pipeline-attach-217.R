# Extracted from test-pipeline-attach.R:217

# prequel ----------------------------------------------------------------------
.make_attach_root <- function(n_subjects = 2,
                              labels = c("face", "scene"),
                              use_relative_paths = TRUE,
                              write_nifti = FALSE) {
  tmp <- tempfile("attach-test-")
  dir.create(tmp, recursive = TRUE)
  root <- file.path(tmp, "out")

  mask_file <- file.path(tmp, "mask.nii.gz")
  if (write_nifti) {
    mask_arr <- array(0, dim = c(2, 2, 2))
    mask_arr[1, 1, 1] <- 1
    mask_arr[2, 1, 1] <- 1
    RNifti::writeNifti(mask_arr, mask_file)
  } else {
    file.create(mask_file)
  }

  subjects <- sprintf("%02d", seq_len(n_subjects))
  work_ids <- sprintf("w%04d", seq_len(n_subjects))

  for (i in seq_len(n_subjects)) {
    wdir <- file.path(root, "firstlevel", "work", work_ids[i])
    dir.create(wdir, recursive = TRUE, showWarnings = FALSE)

    files <- character(length(labels))
    for (j in seq_along(labels)) {
      f <- file.path(wdir, paste0(labels[j], ".nii.gz"))
      if (write_nifti) {
        arr <- array(runif(8), dim = c(2, 2, 2))
        RNifti::writeNifti(arr, f)
      } else {
        file.create(f)
      }
      files[j] <- if (use_relative_paths) {
        paste0("firstlevel/work/", work_ids[i], "/", labels[j], ".nii.gz")
      } else {
        f
      }
    }

    manifest <- data.frame(
      work_id = work_ids[i],
      subject = subjects[i],
      group = "all",
      task = "toy",
      type = "estimates",
      label = labels,
      statistic = "estimate",
      file = files,
      mask_file = mask_file,
      stringsAsFactors = FALSE
    )
    utils::write.table(manifest, file.path(wdir, "maps.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE)
  }

  plan_work_dir <- if (use_relative_paths) {
    paste0("firstlevel/work/", work_ids)
  } else {
    file.path(root, "firstlevel", "work", work_ids)
  }

  plan <- data.frame(
    subject = subjects,
    group = "all",
    task = "toy",
    work_id = work_ids,
    scan_count = 1L,
    work_dir = plan_work_dir,
    fit_file = file.path(plan_work_dir, "fit.rds"),
    manifest_file = file.path(plan_work_dir, "maps.tsv"),
    stringsAsFactors = FALSE
  )
  utils::write.table(plan, file.path(root, "firstlevel", "work_plan.tsv"),
                     sep = "\t", quote = FALSE, row.names = FALSE)

  list(root = root, tmp = tmp, mask_file = mask_file,
       subjects = subjects, labels = labels)
}

# test -------------------------------------------------------------------------
skip_if_not_installed("RNifti")
skip_if_not_installed("neuroim2")
info <- .make_attach_root(write_nifti = TRUE)
plan <- pipeline_load_analysis_plan(info$root)
pls_opts <- list(method = "task", nperm = 0L, nboot = 0L)
spec <- pipeline_build_pls_spec_from_ui(plan, pls_opts)
expect_s3_class(spec, "pls_spec")
expect_equal(spec$num_cond, 2L)
expect_equal(length(spec$datamat_lst), 1L)
expect_equal(spec$num_perm, 0L)
