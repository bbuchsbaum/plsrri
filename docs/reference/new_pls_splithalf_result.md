# Create a Split-Half Validation Result Object

Create a Split-Half Validation Result Object

## Usage

``` r
new_pls_splithalf_result(
  num_outer_perm,
  num_split,
  orig_ucorr = NULL,
  orig_vcorr = NULL,
  ucorr_prob = NULL,
  vcorr_prob = NULL,
  ucorr_ul = NULL,
  ucorr_ll = NULL,
  vcorr_ul = NULL,
  vcorr_ll = NULL
)
```

## Arguments

- num_outer_perm:

  Number of outer permutations

- num_split:

  Number of splits

- orig_ucorr:

  Original u (brain) correlation

- orig_vcorr:

  Original v (design/behavior) correlation

- ucorr_prob:

  Probability for u correlation

- vcorr_prob:

  Probability for v correlation

- ucorr_ul:

  Upper limit for u correlation

- ucorr_ll:

  Lower limit for u correlation

- vcorr_ul:

  Upper limit for v correlation

- vcorr_ll:

  Lower limit for v correlation

## Value

A `pls_splithalf_result` object
