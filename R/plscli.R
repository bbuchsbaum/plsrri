#' `plscli` Command-Line Entry Point
#'
#' @description
#' A lightweight staged CLI for BIDS discovery, first-level estimation, and PLS.
#'
#' @name plscli
NULL

.plscli_usage <- function() {
  paste(
    "Usage: plscli <command> --spec path/to/spec.yml [options]",
    "",
    "Commands:",
    "  template         Write an example YAML specification",
    "  validate         Validate the pipeline specification",
    "  discover         Build a BIDS discovery manifest",
    "  firstlevel-plan  Create first-level work units",
    "  firstlevel-run   Run first-level estimation",
    "  pls-plan         Build a PLS manifest from first-level outputs",
    "  pls-run          Run PLS from the planned manifest",
    "  report           Render a Quarto report from a spec, artifact root, or result",
    "  summarize        Summarize stage outputs",
    "  run              Convenience wrapper for all stages",
    "",
    "Options:",
    "  --spec PATH      YAML specification path",
    "  --input PATH     Generic report input: spec, artifact root, or pls_result.rds",
    "  --work-id ID     Optional first-level work unit id",
    "  --out PATH       Output path for `template`",
    "  --output PATH    Output file path for `report`",
    "  --format FMT     Report format: html, pdf, or docx",
    "  --title TEXT     Report title",
    "  --author TEXT    Report author",
    "  --open BOOL      Open the rendered report when interactive",
    "  --help           Show this message",
    sep = "\n"
  )
}

.plscli_parse_args <- function(args) {
  if (!length(args) || any(args %in% c("-h", "--help"))) {
    return(list(command = "help"))
  }

  command <- args[[1]]
  opts <- list()
  i <- 2L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (!startsWith(arg, "--")) {
      stop("Unexpected positional argument: ", arg, call. = FALSE)
    }
    key <- substring(arg, 3L)
    if (key %in% c("help")) {
      opts[[key]] <- TRUE
      i <- i + 1L
      next
    }
    if (i == length(args)) {
      stop("Option requires a value: ", arg, call. = FALSE)
    }
    opts[[key]] <- args[[i + 1L]]
    i <- i + 2L
  }

  list(command = command, options = opts)
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

#' Run the `plscli` Dispatcher
#'
#' @param args Character vector of command-line arguments.
#'
#' @return Invisibly returns the stage result.
#' @export
plscli_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- .plscli_parse_args(args)
  if (identical(parsed$command, "help")) {
    cat(.plscli_usage(), "\n")
    return(invisible(NULL))
  }

  command <- parsed$command
  opts <- parsed$options %||% list()

  if (identical(command, "template")) {
    out <- opts$out %||% file.path(getwd(), "plscli.yml")
    write_pipeline_template(out)
    cat("Wrote template:", out, "\n")
    return(invisible(out))
  }

  if (identical(command, "report")) {
    report_input <- opts$input %||% opts$spec
    if (is.null(report_input) || !nzchar(report_input)) {
      stop("--input or --spec is required for command 'report'", call. = FALSE)
    }
    result <- render_report(
      report_input,
      output_file = opts$output,
      output_format = opts$format %||% "html",
      title = opts$title %||% "PLS Pipeline Report",
      author = opts$author %||% Sys.info()[["user"]],
      open = tolower(opts$open %||% "false") %in% c("1", "true", "yes")
    )
    .plscli_print_table(result)
    return(invisible(result))
  }

  spec_path <- opts$spec
  if (is.null(spec_path) || !nzchar(spec_path)) {
    stop("--spec is required for command '", command, "'", call. = FALSE)
  }

  result <- switch(
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
    },
    stop("Unknown plscli command: ", command, call. = FALSE)
  )

  .plscli_print_table(result)
  invisible(result)
}
