# Map PLS Method Integer to Canonical String Name

Single source of truth for integer-to-name mapping. All code paths that
convert method integers to strings should call this function.

## Usage

``` r
pls_method_int_to_name(method)
```

## Arguments

- method:

  Integer method code (1-6).

## Value

Character string: one of `"task"`, `"task_nonrotated"`, `"behavior"`,
`"multiblock"`, `"behavior_nonrotated"`, `"multiblock_nonrotated"`.
