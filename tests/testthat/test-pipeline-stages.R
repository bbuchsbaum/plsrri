# Synthetic tests for staged plscli helpers.

test_that("pipeline_pls_plan folds basis-style first-level maps into lagged manifest", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("RNifti")

  tmp <- tempfile("plscli-basis-")
  dir.create(tmp, recursive = TRUE)
  out_root <- file.path(tmp, "out")
  dir.create(out_root, recursive = TRUE)

  mask_arr <- array(0, dim = c(2, 2, 2))
  mask_arr[1, 1, 1] <- 1
  mask_arr[2, 1, 1] <- 1
  mask_file <- file.path(tmp, "mask.nii.gz")
  RNifti::writeNifti(mask_arr, mask_file)

  make_map <- function(value, file) {
    arr <- array(0, dim = c(2, 2, 2))
    arr[1, 1, 1] <- value
    arr[2, 1, 1] <- value + 0.25
    RNifti::writeNifti(arr, file)
  }

  work_plan <- data.frame(
    subject = c("01", "02"),
    group = c("all", "all"),
    task = c("toy", "toy"),
    work_id = c("w0001", "w0002"),
    scan_count = c(1L, 1L),
    work_dir = file.path(out_root, "firstlevel", "work", c("w0001", "w0002")),
    fit_file = file.path(out_root, "firstlevel", "work", c("w0001", "w0002"), "fit.rds"),
    manifest_file = file.path(out_root, "firstlevel", "work", c("w0001", "w0002"), "maps.tsv"),
    stringsAsFactors = FALSE
  )

  labels <- c("face_t0", "face_t1", "scene_t0", "scene_t1")
  subject_offsets <- c("01" = 0, "02" = 10)

  for (i in seq_len(nrow(work_plan))) {
    dir.create(work_plan$work_dir[[i]], recursive = TRUE, showWarnings = FALSE)
    subj <- work_plan$subject[[i]]
    manifest <- do.call(rbind, lapply(seq_along(labels), function(j) {
      file <- file.path(work_plan$work_dir[[i]], paste0(labels[[j]], ".nii.gz"))
      make_map(j + subject_offsets[[subj]], file)
      data.frame(
        work_id = work_plan$work_id[[i]],
        subject = subj,
        group = "all",
        task = "toy",
        type = "estimates",
        label = labels[[j]],
        statistic = "estimate",
        file = file,
        mask_file = mask_file,
        stringsAsFactors = FALSE
      )
    }))
    utils::write.table(manifest, work_plan$manifest_file[[i]], sep = "\t", quote = FALSE, row.names = FALSE)
  }

  utils::write.table(work_plan, file.path(out_root, "firstlevel", "work_plan.tsv"),
                     sep = "\t", quote = FALSE, row.names = FALSE)

  spec_path <- file.path(tmp, "plscli.yml")
  writeLines(yaml::as.yaml(list(
    dataset = list(
      bids_dir = tmp,
      task = "toy"
    ),
    design = list(
      formula = "onset ~ hrf(condition, basis = 'fir')"
    ),
    first_level = list(
      output = list(
        type = "estimates",
        statistics = "estimate"
      )
    ),
    pls = list(
      method = "task",
      input = list(
        type = "estimates",
        statistic = "estimate",
        basis_pattern = "^(.*)_t([0-9]+)$",
        condition_group = 1L,
        basis_group = 2L,
        basis_order = c("0", "1")
      )
    ),
    outputs = list(
      root = out_root
    )
  )), spec_path)

  planned <- pipeline_pls_plan(spec_path)
  expect_true("lag" %in% names(planned))
  expect_equal(sort(unique(planned$condition)), c("face", "scene"))
  expect_equal(sort(unique(planned$lag)), c(0L, 1L))
})

test_that("pipeline_pls_run works from synthetic basis-style first-level maps", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("RNifti")
  skip_if_not_installed("neuroim2")

  tmp <- tempfile("plscli-run-")
  dir.create(tmp, recursive = TRUE)
  out_root <- file.path(tmp, "out")
  dir.create(out_root, recursive = TRUE)

  mask_arr <- array(0, dim = c(2, 2, 2))
  mask_arr[1, 1, 1] <- 1
  mask_arr[2, 1, 1] <- 1
  mask_file <- file.path(tmp, "mask.nii.gz")
  RNifti::writeNifti(mask_arr, mask_file)

  write_map <- function(value, file) {
    arr <- array(0, dim = c(2, 2, 2))
    arr[1, 1, 1] <- value
    arr[2, 1, 1] <- value + 0.5
    RNifti::writeNifti(arr, file)
  }

  plan <- data.frame(
    subject = c("01", "02"),
    group = c("all", "all"),
    task = c("toy", "toy"),
    work_id = c("w0001", "w0002"),
    scan_count = c(1L, 1L),
    work_dir = file.path(out_root, "firstlevel", "work", c("w0001", "w0002")),
    fit_file = file.path(out_root, "firstlevel", "work", c("w0001", "w0002"), "fit.rds"),
    manifest_file = file.path(out_root, "firstlevel", "work", c("w0001", "w0002"), "maps.tsv"),
    stringsAsFactors = FALSE
  )

  labels <- c("face_t0", "face_t1", "scene_t0", "scene_t1")
  values <- list(
    "01" = c(3, 2, -2, -3),
    "02" = c(2.5, 1.5, -1.5, -2.5)
  )

  for (i in seq_len(nrow(plan))) {
    dir.create(plan$work_dir[[i]], recursive = TRUE, showWarnings = FALSE)
    subj <- plan$subject[[i]]
    manifest <- do.call(rbind, lapply(seq_along(labels), function(j) {
      file <- file.path(plan$work_dir[[i]], paste0(labels[[j]], ".nii.gz"))
      write_map(values[[subj]][[j]], file)
      data.frame(
        work_id = plan$work_id[[i]],
        subject = subj,
        group = "all",
        task = "toy",
        type = "estimates",
        label = labels[[j]],
        statistic = "estimate",
        file = file,
        mask_file = mask_file,
        stringsAsFactors = FALSE
      )
    }))
    utils::write.table(manifest, plan$manifest_file[[i]], sep = "\t", quote = FALSE, row.names = FALSE)
  }

  utils::write.table(plan, file.path(out_root, "firstlevel", "work_plan.tsv"),
                     sep = "\t", quote = FALSE, row.names = FALSE)

  spec_path <- file.path(tmp, "plscli.yml")
  writeLines(yaml::as.yaml(list(
    dataset = list(
      bids_dir = tmp,
      task = "toy"
    ),
    design = list(
      formula = "onset ~ hrf(condition, basis = 'fir')"
    ),
    first_level = list(
      output = list(
        type = "estimates",
        statistics = "estimate"
      )
    ),
    pls = list(
      method = "task",
      nperm = 0L,
      nboot = 0L,
      input = list(
        type = "estimates",
        statistic = "estimate",
        basis_pattern = "^(.*)_t([0-9]+)$",
        condition_group = 1L,
        basis_group = 2L,
        basis_order = c("0", "1")
      )
    ),
    outputs = list(
      root = out_root
    )
  )), spec_path)

  pipeline_pls_plan(spec_path)
  result <- pipeline_pls_run(spec_path)

  expect_s3_class(result, "pls_result")
  expect_equal(nrow(result$u), 4L)
  expect_equal(result$num_cond, 2L)
  expect_true(file.exists(file.path(out_root, "pls", "pls_result.rds")))
})
