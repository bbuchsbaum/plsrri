# Check for Low Variability

Checks if behavior data has low variability which could cause numerical
issues (division by zero in correlation).

## Usage

``` r
pls_is_low_variability(behavdata, threshold = 1e-10)
```

## Arguments

- behavdata:

  Behavior data matrix

- threshold:

  Minimum standard deviation allowed

## Value

Logical, TRUE if low variability detected
