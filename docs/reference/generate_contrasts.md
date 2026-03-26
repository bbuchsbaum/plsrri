# Generate Common Contrasts

Generates common contrast matrices for PLS analysis.

## Usage

``` r
generate_contrasts(num_groups, num_cond, type = "helmert")
```

## Arguments

- num_groups:

  Number of groups

- num_cond:

  Number of conditions

- type:

  Type of contrast:

  - "helmert": Helmert contrasts

  - "deviation": Deviation from mean

  - "treatment": Treatment vs baseline

  - "polynomial": Polynomial trends

## Value

Contrast matrix
