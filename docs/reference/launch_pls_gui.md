# Launch PLS GUI

Launches the interactive Shiny GUI for PLS neuroimaging analysis.

## Usage

``` r
launch_pls_gui(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly returns the Shiny app object when the app closes.

## Examples

``` r
if (FALSE) { # \dontrun{
launch_pls_gui()
launch_pls_gui(port = 3838)
launch_pls_gui(launch.browser = FALSE)
} # }
```
