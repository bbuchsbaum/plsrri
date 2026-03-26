# Generate All Bootstrap Samples (Helper)

Generates all possible bootstrap samples of size n from n items that
have at least min_unique unique elements.

## Usage

``` r
generate_boot_samples(n, min_unique)
```

## Arguments

- n:

  Sample size

- min_unique:

  Minimum unique elements required

## Value

Matrix where each row is a bootstrap sample
