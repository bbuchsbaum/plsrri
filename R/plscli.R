#' `plscli` Command-Line Entry Point
#'
#' @description
#' A staged command-line interface for BIDS discovery, first-level estimation,
#' PLS execution, and report rendering.
#'
#' @name plscli
NULL

.plscli_command_specs <- function() {
  list(
    template = list(
      summary = "Write an example YAML specification",
      requires_spec = FALSE,
      options = list(
        out = "value"
      )
    ),
    validate = list(
      summary = "Validate the pipeline specification",
      requires_spec = TRUE,
      options = list(
        spec = "value"
      )
    ),
    discover = list(
      summary = "Build a BIDS discovery manifest",
      requires_spec = TRUE,
      options = list(
        spec = "value"
      )
    ),
    "firstlevel-plan" = list(
      summary = "Create first-level work units",
      requires_spec = TRUE,
      options = list(
        spec = "value"
      )
    ),
    "firstlevel-run" = list(
      summary = "Run first-level estimation",
      requires_spec = TRUE,
      options = list(
        spec = "value",
        "work-id" = "value"
      )
    ),
    "pls-plan" = list(
      summary = "Build a PLS manifest from first-level outputs",
      requires_spec = TRUE,
      options = list(
        spec = "value"
      )
    ),
    "pls-run" = list(
      summary = "Run PLS from the planned manifest",
      requires_spec = TRUE,
      options = list(
        spec = "value"
      )
    ),
    report = list(
      summary = "Render a Quarto report from a spec, artifact root, or result",
      requires_spec = FALSE,
      options = list(
        spec = "value",
        input = "value",
        output = "value",
        format = "value",
        title = "value",
        author = "value",
        open = "flag"
      )
    ),
    summarize = list(
      summary = "Summarize stage outputs",
      requires_spec = TRUE,
      options = list(
        spec = "value"
      )
    ),
    run = list(
      summary = "Run the staged workflow end to end",
      requires_spec = TRUE,
      options = list(
        spec = "value",
        "work-id" = "value"
      )
    )
  )
}

.plscli_option_type <- function(command, key) {
  specs <- .plscli_command_specs()
  global <- c(help = "flag", json = "flag")
  if (key %in% names(global)) {
    return(global[[key]])
  }
  if (!command %in% names(specs)) {
    return(NULL)
  }
  specs[[command]]$options[[key]] %||% NULL
}

.plscli_usage <- function(command = NULL) {
  specs <- .plscli_command_specs()

  if (is.null(command)) {
    lines <- c(
      "Usage:",
      "  plscli <command> [options]",
      "  plscli help [command]",
      "",
      "Commands:"
    )
    for (name in names(specs)) {
      lines <- c(lines, sprintf("  %-15s %s", name, specs[[name]]$summary))
    }
    lines <- c(
      lines,
      "",
      "Global options:",
      "  --help           Show global or command-specific help",
      "  --json           Emit machine-readable JSON to stdout",
      "",
      "Run `plscli help <command>` for command-specific options."
    )
    return(paste(lines, collapse = "\n"))
  }

  if (!command %in% names(specs)) {
    return(paste(
      sprintf("Unknown command: %s", command),
      "",
      .plscli_usage(NULL),
      sep = "\n"
    ))
  }

  spec <- specs[[command]]
  command_usage <- sprintf("Usage:\n  plscli %s", command)
  if (isTRUE(spec$requires_spec)) {
    command_usage <- paste0(command_usage, " --spec path/to/spec.yml")
  }
  command_usage <- paste0(command_usage, " [options]")

  lines <- c(
    command_usage,
    "",
    spec$summary,
    "",
    "Options:"
  )

  if (isTRUE(spec$requires_spec)) {
    lines <- c(lines, "  --spec PATH      YAML specification path")
  }

  if (identical(command, "template")) {
    lines <- c(lines, "  --out PATH       Output path for the generated template")
  }
  if (identical(command, "firstlevel-run")) {
    lines <- c(lines, "  --work-id ID     Optional first-level work unit id")
  }
  if (identical(command, "run")) {
    lines <- c(lines, "  --work-id ID     Optional first-level work unit id")
  }
  if (identical(command, "report")) {
    lines <- c(
      lines,
      "  --input PATH     Report input: spec, artifact root, or pls_result.rds",
      "  --output PATH    Output file path for the rendered report",
      "  --format FMT     Report format: html, pdf, or docx",
      "  --title TEXT     Report title",
      "  --author TEXT    Report author",
      "  --open           Open the rendered report when interactive",
      "  --no-open        Disable report opening explicitly"
    )
  }

  lines <- c(
    lines,
    "  --help           Show help for this command",
    "  --json           Emit machine-readable JSON to stdout"
  )

  paste(lines, collapse = "\n")
}

.plscli_parse_args <- function(args) {
  specs <- .plscli_command_specs()

  if (!length(args)) {
    return(list(command = "help", topic = NULL, options = list()))
  }

  first <- args[[1]]
  if (first %in% c("-h", "--help")) {
    return(list(command = "help", topic = NULL, options = list()))
  }
  if (identical(first, "help")) {
    topic <- if (length(args) >= 2L) args[[2]] else NULL
    return(list(command = "help", topic = topic, options = list()))
  }

  command <- first
  rest <- args[-1]
  options <- list()
  i <- 1L

  while (i <= length(rest)) {
    arg <- rest[[i]]
    if (arg %in% c("-h", "--help")) {
      return(list(command = "help", topic = command, options = list()))
    }
    if (!startsWith(arg, "--")) {
      stop("Unexpected positional argument: ", arg, call. = FALSE)
    }

    key_value <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- key_value[[1]]
    explicit_value <- if (length(key_value) > 1L) paste(key_value[-1], collapse = "=") else NULL

    is_negated <- startsWith(key, "no-")
    canonical_key <- if (is_negated) substring(key, 4L) else key
    opt_type <- .plscli_option_type(command, canonical_key)
    if (is.null(opt_type)) {
      stop("Unknown option for command '", command, "': --", key, call. = FALSE)
    }
    if (is_negated && !identical(opt_type, "flag")) {
      stop("Option does not support --no- prefix: --", canonical_key, call. = FALSE)
    }

    if (identical(opt_type, "flag")) {
      if (!is.null(explicit_value)) {
        value <- tolower(explicit_value)
        if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
          stop("Flag option expects a boolean value: --", canonical_key, call. = FALSE)
        }
        options[[canonical_key]] <- value %in% c("true", "1", "yes")
      } else if (!is_negated && i < length(rest) && !startsWith(rest[[i + 1L]], "--")) {
        value <- tolower(rest[[i + 1L]])
        if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
          stop("Flag option expects a boolean value: --", canonical_key, call. = FALSE)
        }
        options[[canonical_key]] <- value %in% c("true", "1", "yes")
        i <- i + 2L
        next
      } else {
        options[[canonical_key]] <- !is_negated
      }
      i <- i + 1L
      next
    }

    if (!is.null(explicit_value)) {
      options[[canonical_key]] <- explicit_value
      i <- i + 1L
      next
    }

    if (i == length(rest)) {
      stop("Option requires a value: --", canonical_key, call. = FALSE)
    }

    options[[canonical_key]] <- rest[[i + 1L]]
    i <- i + 2L
  }

  if (!command %in% names(specs)) {
    return(list(command = command, options = options))
  }

  list(command = command, options = options)
}

.plscli_message <- function(...) {
  cat(..., "\n", file = stderr(), sep = "")
}

.plscli_print_table <- function(x) {
  if (inherits(x, "pls_result")) {
    print(summary(x))
    return(invisible(x))
  }
  if (is.data.frame(x)) {
    print(utils::head(x, 20L), row.names = FALSE)
    return(invisible(x))
  }
  print(x)
  invisible(x)
}

.plscli_result_payload <- function(command, result) {
  if (identical(command, "template")) {
    return(list(command = command, output = normalizePath(result, winslash = "/", mustWork = FALSE)))
  }
  if (identical(command, "validate")) {
    return(list(
      command = command,
      valid = TRUE,
      spec_path = result$.spec_path %||% NA_character_,
      output_root = .pipeline_output_root(result),
      task = .pipeline_as_chr(.pipeline_nested(result, c("dataset", "task"), character(0))),
      method = .pipeline_nested(result, c("pls", "method"), NA_character_)
    ))
  }
  if (identical(command, "report") && is.character(result) && length(result) == 1L) {
    return(list(command = command, output = normalizePath(result, winslash = "/", mustWork = FALSE)))
  }
  if (inherits(result, "pls_result")) {
    return(list(
      command = command,
      class = class(result),
      n_lv = n_lv(result),
      n_features = n_features(result),
      singular_values = unname(result$s)
    ))
  }
  if (is.data.frame(result) || is.list(result) || length(result) <= 1L) {
    return(result)
  }
  as.list(result)
}

.plscli_emit_result <- function(command, result, json = FALSE) {
  if (isTRUE(json)) {
    payload <- .plscli_result_payload(command, result)
    cat(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
    return(invisible(result))
  }

  if (identical(command, "template")) {
    cat("Wrote template:", result, "\n")
    return(invisible(result))
  }
  if (identical(command, "validate")) {
    cat("Specification is valid:", result$.spec_path %||% "<in-memory spec>", "\n")
    cat("Output root:", .pipeline_output_root(result), "\n")
    return(invisible(result))
  }
  if (identical(command, "report") && is.character(result) && length(result) == 1L) {
    cat("Rendered report:", result, "\n")
    return(invisible(result))
  }

  .plscli_print_table(result)
}

.plscli_exit_status <- function(command, error) {
  message <- conditionMessage(error)
  if (identical(command, "validate") && grepl("^Pipeline specification validation failed:", message)) {
    return(1L)
  }
  2L
}

.plscli_dispatch <- function(command, opts) {
  if (!command %in% names(.plscli_command_specs())) {
    stop("Unknown plscli command: ", command, call. = FALSE)
  }

  if (identical(command, "template")) {
    out <- opts$out %||% file.path(getwd(), "plscli.yml")
    write_pipeline_template(out)
    return(out)
  }

  if (identical(command, "report")) {
    report_input <- opts$input %||% opts$spec
    if (is.null(report_input) || !nzchar(report_input)) {
      stop("--input or --spec is required for command 'report'", call. = FALSE)
    }

    return(render_report(
      report_input,
      output_file = opts$output,
      output_format = opts$format %||% "html",
      title = opts$title %||% "PLS Pipeline Report",
      author = opts$author %||% Sys.info()[["user"]],
      open = isTRUE(opts$open)
    ))
  }

  spec_path <- opts$spec
  if (is.null(spec_path) || !nzchar(spec_path)) {
    stop("--spec is required for command '", command, "'", call. = FALSE)
  }

  switch(
    command,
    validate = pipeline_validate(spec_path),
    discover = pipeline_discover(spec_path),
    "firstlevel-plan" = pipeline_firstlevel_plan(spec_path),
    "firstlevel-run" = pipeline_firstlevel_run(spec_path, work_id = opts$`work-id`),
    "pls-plan" = pipeline_pls_plan(spec_path),
    "pls-run" = pipeline_pls_run(spec_path),
    summarize = pipeline_summarize(spec_path),
    run = {
      pipeline_validate(spec_path)
      pipeline_discover(spec_path)
      pipeline_firstlevel_plan(spec_path)
      pipeline_firstlevel_run(spec_path, work_id = opts$`work-id`)
      pipeline_pls_plan(spec_path)
      pipeline_pls_run(spec_path)
    }
  )
}

.plsrri_exec_dir <- function() {
  exec_dir <- system.file("exec", package = "plsrri")
  if (nzchar(exec_dir) && dir.exists(exec_dir)) {
    return(exec_dir)
  }

  source_dir <- file.path(getwd(), "exec")
  if (dir.exists(source_dir) && file.exists(file.path(getwd(), "DESCRIPTION"))) {
    return(normalizePath(source_dir, winslash = "/", mustWork = TRUE))
  }

  stop("Could not locate plsrri CLI wrappers under exec/", call. = FALSE)
}

#' Install `plsrri` Command Wrappers
#'
#' @param dest_dir Destination directory for installed commands.
#' @param overwrite Overwrite existing command wrappers if `TRUE`.
#' @param commands Optional character vector of command names to install.
#'
#' @return Invisibly returns the installed wrapper paths.
#' @export
install_cli <- function(dest_dir = "~/.local/bin", overwrite = FALSE, commands = NULL) {
  command_map <- c(plscli = "plscli")
  if (is.null(commands)) {
    commands <- names(command_map)
  }

  unknown <- setdiff(commands, names(command_map))
  if (length(unknown) > 0L) {
    stop("Unknown command(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  exec_dir <- .plsrri_exec_dir()
  dest_dir <- normalizePath(path.expand(dest_dir), winslash = "/", mustWork = FALSE)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  installed <- character(length(commands))
  names(installed) <- commands

  for (command in commands) {
    src <- file.path(exec_dir, command_map[[command]])
    if (!file.exists(src)) {
      stop("Wrapper does not exist: ", src, call. = FALSE)
    }

    dest <- file.path(dest_dir, command)
    if (file.exists(dest) && !isTRUE(overwrite)) {
      stop("Refusing to overwrite existing command: ", dest, call. = FALSE)
    }

    ok <- file.copy(src, dest, overwrite = isTRUE(overwrite), copy.mode = TRUE)
    if (!isTRUE(ok)) {
      stop("Failed to install command: ", command, call. = FALSE)
    }
    Sys.chmod(dest, mode = "755")
    installed[[command]] <- normalizePath(dest, winslash = "/", mustWork = TRUE)
  }

  path_entries <- normalizePath(path.expand(strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]),
    winslash = "/",
    mustWork = FALSE
  )
  if (!dest_dir %in% path_entries) {
    message("Directory is not on PATH: ", dest_dir)
  }

  invisible(unname(installed))
}

#' Run the `plscli` Dispatcher
#'
#' @param args Character vector of command-line arguments.
#'
#' @return Invisibly returns an integer exit status.
#' @export
plscli_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- tryCatch(
    .plscli_parse_args(args),
    error = function(e) {
      .plscli_message("plscli: ", conditionMessage(e))
      structure(list(error = e), class = "plscli_parse_error")
    }
  )

  if (inherits(parsed, "plscli_parse_error")) {
    return(invisible(2L))
  }

  if (identical(parsed$command, "help")) {
    cat(.plscli_usage(parsed$topic), "\n")
    return(invisible(0L))
  }

  command <- parsed$command
  opts <- parsed$options %||% list()

  result <- tryCatch(
    .plscli_dispatch(command, opts),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    .plscli_message("plscli: ", conditionMessage(result))
    return(invisible(.plscli_exit_status(command, result)))
  }

  .plscli_emit_result(command, result, json = isTRUE(opts$json))
  invisible(0L)
}
