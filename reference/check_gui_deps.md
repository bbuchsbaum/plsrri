# Check GUI Dependencies

Checks if all required packages for the Shiny GUI are installed.

## Usage

``` r
check_gui_deps()
```

## Value

Logical indicating whether all required GUI dependencies are installed.

## Examples

``` r
check_gui_deps()
#> 
#> ── PLS GUI Dependency Check ──
#> 
#> ── Required packages 
#> ✔ shiny
#> ✔ bslib
#> ✔ bsicons
#> ✔ shinyjs
#> ✔ shinyFiles
#> ✔ R6
#> 
#> ── Suggested packages (for async execution) 
#> ✔ future
#> ✔ promises
```
