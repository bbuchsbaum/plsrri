# Get Confidence Intervals

Extracts bootstrap confidence intervals for saliences, correlations, or
brain scores.

## Usage

``` r
confidence(x, what = "salience", lv = NULL)
```

## Arguments

- x:

  A `pls_result` object

- what:

  What to get CIs for: "salience", "correlation", or "brain_scores"

- lv:

  Latent variable index or vector of indices (NULL = all)

## Value

List with lower and upper bounds
