# Add Site Labels

Adds subject- or observation-level site labels for multisite pooling
diagnostics. The labels are stored on the specification and, when
present, behavior PLS fits will attach post-fit site-stability
summaries.

## Usage

``` r
add_site_labels(spec, labels)
```

## Arguments

- spec:

  A `pls_spec` object.

- labels:

  Character vector of site labels. May have length equal to the number
  of subjects or observations implied by the current specification.

## Value

Updated `pls_spec` object.
