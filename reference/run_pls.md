# Run PLS from a Prepared or Saved Input

Run PLS from a Prepared or Saved Input

## Usage

``` r
run_pls(
  x,
  pls_options = NULL,
  input_type = NULL,
  statistic = NULL,
  remap = NULL,
  progress = FALSE,
  ...
)
```

## Arguments

- x:

  A
  [`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md),
  pipeline YAML/list, first-level preparation object, artifact root, or
  analysis plan.

- pls_options:

  Optional PLS option overrides passed to
  [`prepare_pls`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md)
  when `x` is not already a
  [`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md).

- input_type:

  Optional first-level output type for artifact-root inputs.

- statistic:

  Optional first-level statistic for artifact-root inputs.

- remap:

  Optional alternative root for resolving moved relative paths.

- progress:

  Logical; passed to
  [`run`](https://bbuchsbaum.github.io/plsrri/reference/run.md) when
  running an in-memory
  [`pls_spec`](https://bbuchsbaum.github.io/plsrri/reference/pls_spec.md).

- ...:

  Additional arguments forwarded to
  [`run`](https://bbuchsbaum.github.io/plsrri/reference/run.md).

## Value

A fitted PLS result object.
