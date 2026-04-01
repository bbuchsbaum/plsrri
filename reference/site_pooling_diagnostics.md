# Site-Pooling Diagnostics for Multisite PLS

Optional post-fit diagnostics for pooled multisite analyses. When
observation- or subject-level site labels are available, these summaries
quantify how stable a pooled solution is across sites.

## Usage

``` r
site_pooling_diagnostics(x, site = NULL, spec = NULL, progress = FALSE)
```

## Arguments

- x:

  A fitted `pls_result`.

- site:

  Optional site labels. May have length equal to the number of
  observations or the number of subjects. If omitted, `x$site` is used.

- spec:

  Optional `pls_spec` used to fit `x`. This is required for
  site-specific and leave-one-site-out reruns when `x` does not already
  carry enough input context.

- progress:

  Logical; emit informational messages during reruns.

## Value

A named list containing pooled observation metadata, sitewise score
summaries, sitewise score correlations, site-specific salience
similarity, and leave-one-site-out rerun summaries.
