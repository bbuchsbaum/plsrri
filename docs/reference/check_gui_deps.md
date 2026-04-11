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
#> -- PLS GUI Dependency Check --
#> 
#> -- Required packages 
#> v shiny
#> v bslib
#> x bsicons - NOT INSTALLED
#> x shinyjs - NOT INSTALLED
#> x shinyFiles - NOT INSTALLED
#> v R6
#> 
#> -- Suggested packages (for async execution) 
#> v future
#> v promises
#> i Install missing packages with:
```
