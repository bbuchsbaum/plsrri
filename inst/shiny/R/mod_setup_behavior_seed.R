# Setup module behavior + seed handlers
# Extracted from mod_setup.R to keep setup_server readable and maintainable.

setup_register_behavior_seed_handlers <- function(input, output, session, local_rv) {
  # Behavior matrix upload (Behavior/Multiblock PLS)
  parse_uploaded_numeric_matrix <- function(file_path, file_name) {
    mat <- NULL

    tryCatch({
      if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        df <- utils::read.csv(file_path, check.names = FALSE)

        if (ncol(df) > 1 && !is.numeric(df[[1]]) && all(vapply(df[-1], is.numeric, logical(1)))) {
          rownames(df) <- df[[1]]
          df <- df[-1]
        }

        mat <- as.matrix(df)
        suppressWarnings(mode(mat) <- "numeric")
      } else if (grepl("\\.rds$", file_name, ignore.case = TRUE)) {
        mat <- readRDS(file_path)
      } else if (grepl("\\.rda$", file_name, ignore.case = TRUE)) {
        env <- new.env(parent = emptyenv())
        load(file_path, envir = env)
        obj_name <- ls(env)[1]
        mat <- get(obj_name, envir = env)
      }

      if (is.data.frame(mat)) mat <- as.matrix(mat)
      if (!is.matrix(mat)) return(NULL)

      storage.mode(mat) <- "numeric"
      mat
    }, error = function(e) {
      NULL
    })
  }

  observeEvent(input$behav_file, {
    req(input$behav_file)

    file_path <- input$behav_file$datapath[1]
    file_name <- input$behav_file$name[1]

    mat <- parse_uploaded_numeric_matrix(file_path, file_name)
    if (is.null(mat)) {
      local_rv$behav_loaded <- FALSE
      local_rv$behav_data <- NULL
      showNotification("Behavior file could not be parsed as a numeric matrix", type = "error")
      return()
    }

    local_rv$behav_data <- mat
    local_rv$behav_loaded <- TRUE
  })

  output$behav_info <- renderUI({
    if (!isTRUE(local_rv$behav_loaded) || is.null(local_rv$behav_data)) return(NULL)

    mat <- local_rv$behav_data
    nms <- colnames(mat)
    if (is.null(nms)) {
      nms <- paste0("measure_", seq_len(ncol(mat)))
    }

    div(
      class = "small text-muted mt-2",
      icon("check-circle", class = "text-success"),
      sprintf(" %d rows x %d columns (%s)", nrow(mat), ncol(mat), paste(head(nms, 4), collapse = ", ")),
      if (length(nms) > 4) " \u2026" else NULL
    )
  })

  # Mask upload (Seed PLS)
  observeEvent(input$mask_file, {
    req(input$mask_file)

    if (!requireNamespace("neuroim2", quietly = TRUE)) {
      showNotification("Package 'neuroim2' is required to read NIfTI masks", type = "error")
      return()
    }

    tryCatch({
      mask <- neuroim2::read_vol(input$mask_file$datapath[1])
      local_rv$mask <- mask
    }, error = function(e) {
      local_rv$mask <- NULL
      showNotification(paste("Error reading mask:", e$message), type = "error")
    })
  })

  output$mask_info <- renderUI({
    if (is.null(local_rv$mask)) return(NULL)

    mask_arr <- as.array(local_rv$mask)
    n_vox <- sum(mask_arr > 0)
    dims <- dim(mask_arr)[1:3]

    mismatch <- FALSE
    if (isTRUE(local_rv$data_loaded) && !is.null(local_rv$data_matrices)) {
      n_cols <- vapply(local_rv$data_matrices, ncol, integer(1))
      ok <- n_cols == n_vox | (n_vox > 0 && (n_cols %% n_vox) == 0L)
      mismatch <- !all(ok)
    }

    tagList(
      p(
        class = "small mb-1",
        icon("check-circle", class = "text-success"),
        sprintf("Mask loaded: %s voxels (%dx%dx%d)", format(n_vox, big.mark = ","), dims[1], dims[2], dims[3])
      ),
      if (mismatch) {
        p(
          class = "text-warning small mb-0",
          icon("exclamation-triangle"),
          "Mask voxel count does not match the number of columns in your data matrix."
        )
      }
    )
  })

  # Seed atlas + ROI list (Seed PLS)
  seed_atlas <- reactive({
    req(identical(input$method, "seed"))
    req(identical(as.character(input$seed_source)[1], "atlas"))

    if (!requireNamespace("neuroatlas", quietly = TRUE)) return(NULL)
    if (!requireNamespace("neuroim2", quietly = TRUE)) return(NULL)

    outspace <- if (!is.null(local_rv$mask)) neuroim2::space(local_rv$mask) else NULL

    load_seed_atlas(
      atlas = input$seed_atlas,
      outspace = outspace,
      parcels = input$schaefer_parcels,
      networks = input$schaefer_networks,
      resolution = input$schaefer_resolution
    )
  })

  observeEvent(seed_atlas(), {
    atlas <- seed_atlas()
    if (is.null(atlas) || !requireNamespace("neuroatlas", quietly = TRUE)) return()

    meta <- tryCatch(neuroatlas::roi_metadata(atlas), error = function(e) NULL)
    if (is.null(meta) || nrow(meta) == 0) return()

    label_col <- if ("label_full" %in% names(meta)) "label_full" else "label"
    choices <- meta[[label_col]]

    selected <- intersect(isolate(input$seed_rois), choices)
    updateSelectizeInput(session, "seed_rois", choices = choices, selected = selected, server = TRUE)
  })

  output$seed_info <- renderUI({
    if (!identical(input$method, "seed")) return(NULL)
    rois <- input$seed_rois
    if (is.null(rois)) rois <- character(0)
    n <- length(rois)
    if (n == 0) return(NULL)

    div(
      class = "small text-muted mt-2",
      icon("check-circle", class = "text-success"),
      sprintf(" %d seed ROI%s selected", n, if (n == 1) "" else "s")
    )
  })

  # Seed mask (binary)
  observeEvent(input$seed_mask_file, {
    req(input$seed_mask_file)

    if (!requireNamespace("neuroim2", quietly = TRUE)) {
      showNotification("Package 'neuroim2' is required to read NIfTI seed masks", type = "error")
      return()
    }

    tryCatch({
      seed_mask <- neuroim2::read_vol(input$seed_mask_file$datapath[1])
      local_rv$seed_mask <- seed_mask
    }, error = function(e) {
      local_rv$seed_mask <- NULL
      showNotification(paste("Error reading seed mask:", e$message), type = "error")
    })
  })

  output$seed_mask_info <- renderUI({
    if (is.null(local_rv$seed_mask)) return(NULL)

    arr <- as.array(local_rv$seed_mask)
    dims <- dim(arr)[1:3]
    n_vox <- sum(arr > 0)

    tagList(
      p(
        class = "small text-muted mt-2 mb-0",
        icon("check-circle", class = "text-success"),
        sprintf("Seed mask: %s voxels (%dx%dx%d)", format(n_vox, big.mark = ","), dims[1], dims[2], dims[3])
      )
    )
  })

  # Custom labeled atlas (integer labels)
  observeEvent(input$seed_atlas_file, {
    req(input$seed_atlas_file)

    if (!requireNamespace("neuroim2", quietly = TRUE)) {
      showNotification("Package 'neuroim2' is required to read NIfTI atlases", type = "error")
      return()
    }

    tryCatch({
      atlas <- neuroim2::read_vol(input$seed_atlas_file$datapath[1])
      local_rv$seed_custom_atlas <- atlas

      atlas_arr <- as.array(atlas)
      if (!is.null(local_rv$mask)) {
        mask_arr <- as.array(local_rv$mask)
        if (identical(dim(mask_arr)[1:3], dim(atlas_arr)[1:3])) {
          atlas_arr <- atlas_arr[mask_arr > 0]
        }
      }

      ids <- sort(unique(as.integer(atlas_arr)))
      ids <- ids[ids != 0 & !is.na(ids)]

      local_rv$seed_custom_ids <- ids
      updateSelectizeInput(session, "seed_label_ids", choices = as.character(ids), selected = character(0), server = TRUE)
    }, error = function(e) {
      local_rv$seed_custom_atlas <- NULL
      local_rv$seed_custom_ids <- NULL
      showNotification(paste("Error reading seed atlas:", e$message), type = "error")
    })
  })

  output$seed_atlas_file_info <- renderUI({
    if (is.null(local_rv$seed_custom_atlas)) return(NULL)

    arr <- as.array(local_rv$seed_custom_atlas)
    dims <- dim(arr)[1:3]
    ids <- local_rv$seed_custom_ids
    n_ids <- if (is.null(ids)) 0L else length(ids)

    p(
      class = "small text-muted mt-2 mb-0",
      icon("check-circle", class = "text-success"),
      sprintf("Seed atlas: %dx%dx%d with %d non-zero labels", dims[1], dims[2], dims[3], n_ids)
    )
  })

  seed_atlas
}

