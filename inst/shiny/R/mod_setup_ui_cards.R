# Setup module UI building blocks
# Extracted from mod_setup.R to keep setup_ui readable and maintainable.

setup_ui_data_source_card <- function(ns) {
  data_source_choice_names <- list(
    tagList(
      "BIDS directory",
      help_popover(
        "BIDS directory",
        p("Browse to the root of a BIDS dataset (the folder containing ", code("dataset_description.json"), ")."),
        tags$ul(
          tags$li("Optionally filter by task, space, or filename regex."),
          tags$li("If participants.tsv contains group labels, you can group participants during import.")
        )
      )
    ),
    tagList(
      "Manifest (NIfTI lags)",
      help_popover(
        "Manifest (NIfTI lags)",
        p("Use a manifest file that maps each observation to a NIfTI file on disk."),
        tags$ul(
          tags$li("Accepted formats: ", code(".csv"), ", ", code(".tsv"), ", ", code(".rds"), "."),
          tags$li("Required columns (minimum): ", code("subject"), ", ", code("condition"), ", ", code("file"), "."),
          tags$li("If the NIfTI has a 4th dimension, it is treated as lag volumes (time since onset).")
        ),
        p("You must also upload a brain mask to define voxel columns.")
      )
    ),
    tagList(
      "Manual matrices",
      help_popover(
        "Manual matrices",
        p("Upload one data matrix per group (CSV/RDS/RDA)."),
        tags$ul(
          tags$li("Rows should be observations stacked as subject \u00d7 condition."),
          tags$li("For balanced designs: ", code("nrow = n_subj * n_cond"), "."),
          tags$li("Columns are features/voxels (optionally lags concatenated).")
        )
      )
    ),
    tagList(
      "Load saved spec",
      help_popover(
        "Load saved spec",
        p("Upload a previously saved ", code("pls_spec"), " object (RDS/RDA) to restore data + settings.")
      )
    )
  )

  panel_card(
    title = "Data Source",
    status = tags$span(`data-test` = "setup-data-status", uiOutput(ns("data_status"), inline = TRUE)),

    div(
      `data-test` = "setup-data-source",
      radioButtons(
        ns("data_source"),
        label = NULL,
        choiceValues = c("bids", "manifest", "manual", "load"),
        choiceNames = data_source_choice_names,
        selected = "manual"
      )
    ),

    # Conditional inputs based on source
    conditionalPanel(
      condition = "input['data_source'] == 'bids'",
      ns = ns,
      div(
        class = "mt-3",
        shinyFiles::shinyDirButton(
          ns("bids_dir"),
          label = "Browse BIDS Directory",
          title = "Select BIDS Directory",
          class = "btn btn-outline-primary btn-sm",
          `data-test` = "setup-bids-btn"
        ),
        p(
          class = "text-muted small mt-2 mb-1",
          "Select the BIDS dataset root (the folder containing dataset_description.json)."
        ),
        uiOutput(ns("bids_path_display")),
        div(
          class = "mt-3",
          textInput(
            ns("bids_task"),
            label_with_help(
              "Task (optional filter)",
              "Task filter",
              p("Optional. Restrict to a single BIDS task label (e.g., ", code("stroop"), "). Leave blank to include all tasks.")
            ),
            value = ""
          ),
          textInput(
            ns("bids_space"),
            label_with_help(
              "Space (optional filter)",
              "Space filter",
              p("Optional. Restrict to a particular spatial normalization space (default matches fMRIPrep: ", code("MNI152NLin2009cAsym"), ").")
            ),
            value = "MNI152NLin2009cAsym"
          ),
          textInput(
            ns("bids_file_regex"),
            label_with_help(
              "File regex (matches filename)",
              "Filename regex",
              p("Regular expression matched against candidate NIfTI filenames. Default matches ", code(".nii"), " and ", code(".nii.gz"), ".")
            ),
            value = ".*\\.nii(\\.gz)?$"
          ),
          textInput(
            ns("bids_condition_keys"),
            label_with_help(
              "Condition key(s) (comma-separated)",
              "Condition keys",
              p("Comma-separated metadata keys used to define conditions (e.g., ", code("cond"), " or ", code("desc,run"), ").")
            ),
            value = "cond",
            placeholder = "e.g., cond or desc,run"
          ),
          textInput(
            ns("bids_volumes"),
            label_with_help(
              "Lag volumes (optional, 1-based)",
              "Lag volumes",
              p("Optional. Select 4D volumes to treat as lags. Use 1-based indexing (e.g., ", code("1:8"), "). Leave blank for all volumes.")
            ),
            value = "",
            placeholder = "e.g., 1:8"
          ),
          selectInput(
            ns("bids_mask_method"),
            label_with_help(
              "Mask method",
              "Mask method",
              p("How to construct a brain mask when importing from BIDS."),
              tags$ul(
                tags$li(strong("Intersection"), ": voxels present for all subjects (more conservative)."),
                tags$li(strong("Union"), ": voxels present for any subject (more inclusive)."),
                tags$li(strong("First subject"), ": use the first subject's mask.")
              )
            ),
            choices = c(
              "Intersection" = "intersection",
              "Union" = "union",
              "First subject" = "first"
            ),
            selected = "intersection"
          ),
          fileInput(
            ns("bids_mask_file"),
            label_with_help(
              "Upload Brain Mask (optional)",
              "Custom mask",
              p("Optionally provide your own NIfTI mask. If provided, it overrides automatic mask construction.")
            ),
            accept = c(".nii", ".nii.gz")
          ),
          tags$details(
            class = "mt-2",
            tags$summary(
              "Grouping (optional)",
              help_popover(
                "Grouping participants",
                p("If your BIDS ", code("participants.tsv"), " has group labels, you can split subjects into groups at import time."),
                p("Select the grouping column and one or more values. Leave empty to include all participants.")
              )
            ),
            div(
              class = "mt-2",
              selectInput(
                ns("bids_group_col"),
                "participants.tsv column",
                choices = c("all"),
                selected = "all"
              ),
              selectizeInput(
                ns("bids_group_values"),
                "Group value(s)",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Leave empty to include all participants")
              )
            )
          ),
          actionButton(
            ns("btn_build_bids"),
            "Build from BIDS",
            class = "btn-primary btn-sm mt-2"
          ),
          uiOutput(ns("bids_info")),
          tags$details(
            class = "mt-3",
            tags$summary(
              "In-App First-Level Pipeline",
              help_popover(
                "Workstation first-level mode",
                p("Use the staged pipeline to run first-level GLM estimation in the app, then continue into PLS."),
                p("This is intended for workstation-scale analyses. For HPC or larger studies, export YAML and use the CLI.")
              )
            ),
            div(
              class = "mt-3",
              checkboxInput(
                ns("bids_use_pipeline"),
                "Run first-level GLM in app",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input['bids_use_pipeline']",
                ns = ns,
                tagList(
                  textInput(
                    ns("bids_design_formula"),
                    label_with_help(
                      "First-level design formula",
                      "Design formula",
                      p("R formula passed to ", code("fmrireg::fmri_lm()"), "."),
                      p("Examples: ", code("onset ~ hrf(condition, basis = 'spmg1')"), " or ", code("onset ~ hrf(condition, basis = 'fir')"))
                    ),
                    value = "onset ~ hrf(condition, basis = 'spmg1')"
                  ),
                  selectInput(
                    ns("bids_pipeline_output_type"),
                    label_with_help(
                      "First-level output type",
                      "Output type",
                      p("Which first-level outputs should feed the downstream multivariate stage.")
                    ),
                    choices = c("Estimates" = "estimates", "Contrasts" = "contrasts", "F-statistics" = "F"),
                    selected = "estimates"
                  ),
                  textInput(
                    ns("bids_pipeline_statistic"),
                    label_with_help(
                      "First-level statistic",
                      "Output statistic",
                      p("Statistic to export from first-level results, for example ", code("estimate"), " or ", code("t"), ".")
                    ),
                    value = "estimate"
                  ),
                  textInput(
                    ns("bids_basis_pattern"),
                    label_with_help(
                      "Basis label pattern (optional)",
                      "Basis pattern",
                      p("Regex used to fold basis-expanded first-level labels into lag-style PLS features."),
                      p("Leave blank for single-HRF designs. Typical FIR/tent example: ", code("^(.*)_t([0-9]+)$"))
                    ),
                    value = ""
                  ),
                  textInput(
                    ns("bids_basis_order"),
                    label_with_help(
                      "Basis order (comma-separated, optional)",
                      "Basis order",
                      p("Explicit lag order for basis labels, for example ", code("0,1,2,3"), ". Required when using FIR/tent outputs.")
                    ),
                    value = ""
                  ),
                  radioButtons(
                    ns("bids_analyze_mode"),
                    label_with_help(
                      "Analyze mode",
                      "Analyze mode",
                      p("Choose whether Analyze runs first-level + PLS, or only prepares first-level outputs for later reuse.")
                    ),
                    choiceValues = c("end_to_end", "firstlevel_only"),
                    choiceNames = list(
                      tagList(icon("play"), " End-to-End"),
                      tagList(icon("layer-group"), " First-Level Only")
                    ),
                    selected = "end_to_end",
                    inline = TRUE
                  ),
                  shinyFiles::shinyDirButton(
                    ns("bids_output_root_dir"),
                    label = "Choose Output Root (optional)",
                    title = "Select Pipeline Output Directory",
                    class = "btn btn-outline-secondary btn-sm"
                  ),
                  uiOutput(ns("bids_output_root_display"))
                )
              )
            )
          )
        )
      )
    ),

    conditionalPanel(
      condition = "input['data_source'] == 'manifest'",
      ns = ns,
      div(
        class = "mt-3",
        shinyFiles::shinyFilesButton(
          ns("manifest_file"),
          label = "Browse Manifest File",
          title = "Select manifest (.csv/.tsv/.rds)",
          multiple = FALSE,
          class = "btn btn-outline-primary btn-sm",
          `data-test` = "setup-manifest-btn"
        ),
        p(
          class = "text-muted small mt-2 mb-1",
          "Manifest should include at least: subject, condition, file. ",
          "NIfTI dim4 is interpreted as lags (time since onset)."
        ),
        uiOutput(ns("manifest_path_display")),
        fileInput(
          ns("manifest_mask_file"),
          "Upload Brain Mask (NIfTI)",
          accept = c(".nii", ".nii.gz")
        ),
        uiOutput(ns("manifest_mask_info")),
        uiOutput(ns("manifest_info"))
      )
    ),

    conditionalPanel(
      condition = "input['data_source'] == 'manual'",
      ns = ns,
      div(
        class = "mt-3",
        div(
          `data-test` = "setup-file-upload",
          fileInput(
            ns("data_files"),
            label_with_help(
              "Upload Data Matrices (CSV/RDS)",
              "Upload matrices",
              p("Upload one file per group. Accepted: ", code(".csv"), ", ", code(".rds"), ", ", code(".rda"), "."),
              tags$ul(
                tags$li("CSV should contain only numeric values (an optional first column of row names is OK)."),
                tags$li("RDS/RDA should contain an R matrix (or data.frame coercible to matrix)."),
                tags$li("Rows must match subjects \u00d7 conditions for balanced designs.")
              )
            ),
            accept = c(".csv", ".rds", ".rda"),
            multiple = TRUE
          )
        ),
        uiOutput(ns("manual_data_info"))
      )
    ),

    conditionalPanel(
      condition = "input['data_source'] == 'load'",
      ns = ns,
      div(
        class = "mt-3",
        fileInput(
          ns("spec_file"),
          label_with_help(
            "Upload Saved Specification (RDS)",
            "Saved spec",
            p("Upload a saved ", code("pls_spec"), " object (RDS/RDA). This restores your data source, design, and analysis settings.")
          ),
          accept = c(".rds", ".rda")
        )
      )
    )
  )
}

setup_ui_study_design_card <- function(ns) {
  panel_card(
    title = "Study Design",
    status = tags$span(`data-test` = "setup-design-status", uiOutput(ns("design_status"), inline = TRUE)),

    div(
      class = "mb-3",
      tags$label(
        class = "form-label",
        label_with_help(
          "Groups",
          "Groups",
          p("Define group labels and subject counts for your design."),
          p("For manual matrices, subject counts are used to interpret rows as subject \u00d7 condition."),
          p("For BIDS/manifest imports, these fields may be filled automatically.")
        )
      ),
      div(
        class = "d-flex align-items-center gap-2 mb-2",
        actionButton(ns("add_group"), "", icon = icon("plus"), class = "btn-sm btn-outline-primary"),
        actionButton(ns("remove_group"), "", icon = icon("minus"), class = "btn-sm btn-outline-secondary")
      ),
      uiOutput(ns("group_inputs"))
    ),

    div(
      `data-test` = "setup-num-conditions",
      numericInput(
        ns("num_conditions"),
        label_with_help(
          "Number of Conditions",
          "Conditions",
          p("Number of experimental conditions (or event types) per subject."),
          p("For balanced designs, each group matrix should have ", code("n_subj * n_cond"), " rows.")
        ),
        value = 2,
        min = 1,
        max = 20,
        step = 1
      )
    ),

    uiOutput(ns("design_summary"))
  )
}

setup_ui_analysis_method_card <- function(ns) {
  panel_card(
    title = "Analysis Method",

    div(
      `data-test` = "setup-method",
      radioButtons(
        ns("method"),
        label = NULL,
        choiceValues = c("task", "behavior", "seed", "multiblock"),
        choiceNames = list(
          tagList(
            "Task PLS",
            help_popover(
              "Task PLS",
              p("Compares conditions to find brain patterns that differ across task conditions."),
              p("Use when you have an experimental design with distinct conditions.")
            )
          ),
          tagList(
            "Behavior PLS",
            help_popover(
              "Behavior PLS",
              p("Relates brain data to external measures (RT, accuracy, questionnaires)."),
              p("Requires uploading a behavior matrix with one row per observation.")
            )
          ),
          tagList(
            "Seed PLS",
            help_popover(
              "Seed PLS",
              p("Seed-based connectivity: derives a behavior matrix from one or more ROIs and runs Behavior PLS."),
              p("Requires a brain mask and a seed definition (atlas, mask, or labeled atlas).")
            )
          ),
          tagList(
            "Multiblock PLS",
            help_popover(
              "Multiblock PLS",
              p("Combines task effects with behavioral correlations for richer models."),
              p("Requires a behavior matrix.")
            )
          )
        ),
        selected = "task"
      )
    ),

    # Method description
    uiOutput(ns("method_description"))
  )
}

setup_ui_behav_seed_card <- function(ns) {
  conditionalPanel(
    condition = "input['method'] != 'task'",
    ns = ns,
    panel_card(
      title = "Behavior / Seed Data",
      status = tags$span(`data-test` = "setup-behav-status", uiOutput(ns("behav_status"), inline = TRUE)),

      # Behavior (uploaded matrix)
      conditionalPanel(
        condition = "input['method'] == 'behavior' || input['method'] == 'multiblock'",
        ns = ns,
        div(
          class = "mt-2",
          fileInput(
            ns("behav_file"),
            label_with_help(
              "Upload Behavior Matrix (CSV/RDS/RDA)",
              "Behavior matrix",
              p("One row per observation (subject \u00d7 condition), columns are behavioral variables."),
              p("The number of rows must match the total number of observations in your design.")
            ),
            accept = c(".csv", ".rds", ".rda")
          ),
          uiOutput(ns("behav_info")),
          selectInput(
            ns("cormode"),
            label_with_help(
              "Correlation Mode",
              "Correlation mode",
              tags$ul(
                tags$li(strong("Pearson"), ": standard correlation (scale-invariant)."),
                tags$li(strong("Covariance"), ": retains original scaling."),
                tags$li(strong("Cosine angle"), ": correlation without mean-centering."),
                tags$li(strong("Dot product"), ": raw dot product.")
              )
            ),
            choices = c(
              "Pearson" = "0",
              "Covariance" = "2",
              "Cosine angle" = "4",
              "Dot product" = "6"
            ),
            selected = "0"
          )
        )
      ),

      # Seed PLS (atlas-based)
      conditionalPanel(
        condition = "input['method'] == 'seed'",
        ns = ns,
        div(
          class = "mt-2",
          p(
            class = "text-muted small mb-2",
            "Seed PLS is Behavior PLS where the behavior matrix is derived from one or more ROIs. ",
            "Each seed is the mean ROI value per observation (subject \u00d7 condition)."
          ),
          fileInput(
            ns("mask_file"),
            label_with_help(
              "Upload Brain Mask (NIfTI)",
              "Brain mask",
              p("A binary mask defining which voxels/features are included. Required for Seed PLS (and recommended generally).")
            ),
            accept = c(".nii", ".nii.gz")
          ),
          uiOutput(ns("mask_info")),
          selectInput(
            ns("seed_source"),
            label_with_help(
              "Seed Definition",
              "Seed definition",
              p("Choose how seed ROIs are defined: atlas-based selection, a binary seed mask, or integer-labeled atlas.")
            ),
            choices = c(
              "Atlas ROI selection" = "atlas",
              "Seed mask (binary NIfTI)" = "mask",
              "Custom labeled atlas (integer NIfTI)" = "custom"
            ),
            selected = "atlas"
          ),

          # Atlas ROI selection (neuroatlas)
          conditionalPanel(
            condition = "input['seed_source'] == 'atlas'",
            ns = ns,
            div(
              class = "mt-2",
              selectInput(
                ns("seed_atlas"),
                "Atlas",
                choices = c(
                  "Schaefer (cortex)" = "schaefer",
                  "Glasser (cortex)" = "glasser",
                  "ASEG (subcortical)" = "aseg"
                ),
                selected = "schaefer"
              ),
              conditionalPanel(
                condition = "input['seed_atlas'] == 'schaefer'",
                ns = ns,
                div(
                  class = "row",
                  div(
                    class = "col-4",
                    selectInput(
                      ns("schaefer_parcels"),
                      "Parcels",
                      choices = c("100", "200", "300", "400", "500", "600", "700", "800", "900", "1000"),
                      selected = "200"
                    )
                  ),
                  div(
                    class = "col-4",
                    selectInput(
                      ns("schaefer_networks"),
                      "Networks",
                      choices = c("7", "17"),
                      selected = "17"
                    )
                  ),
                  div(
                    class = "col-4",
                    selectInput(
                      ns("schaefer_resolution"),
                      "Resolution",
                      choices = c("2", "1"),
                      selected = "2"
                    )
                  )
                )
              ),
              selectizeInput(
                ns("seed_rois"),
                "Seed ROI(s)",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select one or more ROIs")
              ),
              uiOutput(ns("seed_info"))
            )
          ),

          # Seed mask (binary)
          conditionalPanel(
            condition = "input['seed_source'] == 'mask'",
            ns = ns,
            div(
              class = "mt-2",
              fileInput(
                ns("seed_mask_file"),
                "Upload Seed Mask (0/1 NIfTI)",
                accept = c(".nii", ".nii.gz")
              ),
              textInput(
                ns("seed_mask_name"),
                "Seed name",
                value = "seed"
              ),
              uiOutput(ns("seed_mask_info"))
            )
          ),

          # Custom labeled atlas (integer)
          conditionalPanel(
            condition = "input['seed_source'] == 'custom'",
            ns = ns,
            div(
              class = "mt-2",
              fileInput(
                ns("seed_atlas_file"),
                "Upload Seed Atlas (integer labels: 1,2,3,...)",
                accept = c(".nii", ".nii.gz")
              ),
              uiOutput(ns("seed_atlas_file_info")),
              checkboxInput(
                ns("seed_use_all_labels"),
                "Use all non-zero labels",
                value = TRUE
              ),
              conditionalPanel(
                condition = "!input['seed_use_all_labels']",
                ns = ns,
                selectizeInput(
                  ns("seed_label_ids"),
                  "Seed label id(s)",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(placeholder = "e.g., 1, 2, 3")
                )
              )
            )
          ),
          textInput(
            ns("seed_lags"),
            label_with_help(
              "Seed lags (optional)",
              "Seed lags",
              p("Optional labels for lagged seed signals (e.g., ", code("0:8"), "). Leave blank for a single seed value per observation.")
            ),
            value = "",
            placeholder = "e.g., 0:8 (lag labels)"
          ),
          selectInput(
            ns("cormode_seed"),
            label_with_help(
              "Correlation Mode",
              "Correlation mode",
              p("How to relate seed signals to brain data (same options as Behavior PLS).")
            ),
            choices = c(
              "Pearson" = "0",
              "Covariance" = "2",
              "Cosine angle" = "4",
              "Dot product" = "6"
            ),
            selected = "0"
          )
        )
      )
    )
  )
}

setup_ui_resampling_card <- function(ns) {
  panel_card(
    title = "Resampling",

    div(
      class = "row",
      div(
        class = "col-6",
        `data-test` = "setup-num-perm",
        numericInput(
          ns("num_perm"),
          label_with_help(
            "Permutations",
            "Permutation test",
            p("Number of permutation samples for significance testing of singular values."),
            p("Set to 0 to skip permutation testing. Typical values: 500\u20132000.")
          ),
          value = 1000,
          min = 0,
          max = 10000,
          step = 100
        )
      ),
      div(
        class = "col-6",
        `data-test` = "setup-num-boot",
        numericInput(
          ns("num_boot"),
          label_with_help(
            "Bootstrap Samples",
            "Bootstrap",
            p("Number of bootstrap samples used to estimate standard errors and confidence intervals."),
            p("Set to 0 to skip bootstrap. Typical values: 200\u20131000.")
          ),
          value = 500,
          min = 0,
          max = 5000,
          step = 100
        )
      )
    ),

    numericInput(
      ns("confidence"),
      label_with_help(
        "Confidence Level (%)",
        "Confidence level",
        p("Percent confidence for reported intervals (e.g., 95%).")
      ),
      value = 95,
      min = 80,
      max = 99,
      step = 1
    ),

    advanced_options(
      id = ns("advanced_resampling"),
      title = label_with_help(
        "Advanced options",
        "Advanced options",
        p("Optional settings affecting resampling and preprocessing. Defaults match the MATLAB toolbox for common cases.")
      ),
      selectInput(
        ns("boot_type"),
        label_with_help(
          "Bootstrap Type",
          "Bootstrap type",
          tags$ul(
            tags$li(strong("Stratified"), ": resample within group/condition blocks (recommended)."),
            tags$li(strong("Non-stratified"), ": resample observations globally.")
          )
        ),
        choices = c(
          "Stratified" = "strat",
          "Non-stratified" = "nonstrat"
        ),
        selected = "strat"
      ),
      numericInput(
        ns("num_split"),
        label_with_help(
          "Split-half Iterations",
          "Split-half validation",
          p("Number of split-half iterations per outer permutation. Set to 0 to disable.")
        ),
        value = 0,
        min = 0,
        max = 1000
      ),
      selectInput(
        ns("meancentering"),
        label_with_help(
          "Mean Centering",
          "Mean-centering",
          p("Controls how condition means are centered prior to SVD."),
          tags$ul(
            tags$li(strong("Grand mean (0)"), ": subtract within-group grand mean."),
            tags$li(strong("Within group (1)"), ": subtract grand condition mean across groups."),
            tags$li(strong("Within condition (2)"), ": subtract global grand mean."),
            tags$li(strong("Within group & condition (3)"), ": remove main effects; keep interaction.")
          )
        ),
        choices = c(
          "Grand mean" = "0",
          "Within group" = "1",
          "Within condition" = "2",
          "Within group & condition" = "3"
        ),
        selected = "0"
      )
    )
  )
}

setup_ui_continue_row <- function(ns) {
  div(
    class = "d-flex justify-content-between align-items-start mt-4 gap-3",
    div(
      class = "d-flex gap-2",
      shinyFiles::shinyFilesButton(
        ns("pipeline_yaml_file"),
        label = "Load YAML",
        title = "Select pipeline YAML",
        multiple = FALSE,
        class = "btn btn-outline-secondary"
      ),
      downloadButton(
        ns("download_yaml"),
        "Export YAML",
        class = "btn btn-outline-secondary"
      ),
      actionButton(
        ns("btn_show_cli"),
        "CLI Commands",
        icon = icon("terminal"),
        class = "btn-outline-secondary"
      ),
      uiOutput(ns("recent_pipeline_ui"))
    ),
    div(
      class = "d-flex align-items-start gap-3",
      div(
        `data-test` = "setup-validation",
        uiOutput(ns("validation_messages"))
      ),
      actionButton(
        ns("btn_continue"),
        "Continue",
        icon = icon("arrow-right"),
        class = "btn-primary pls-btn-primary",
        `data-test` = "setup-continue-btn"
      )
    )
  )
}
