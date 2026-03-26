# Create a Permutation Result Object

Create a Permutation Result Object

## Usage

``` r
new_pls_perm_result(
  num_perm,
  sp,
  sprob,
  permsamp = NULL,
  Tpermsamp = NULL,
  Bpermsamp = NULL
)
```

## Arguments

- num_perm:

  Number of permutations

- sp:

  Count of permuted singular values \>= observed

- sprob:

  Probability (sp / num_perm)

- permsamp:

  Permutation sample matrix

- Tpermsamp:

  Task permutation samples (multiblock)

- Bpermsamp:

  Behavior permutation samples (multiblock)

## Value

A `pls_perm_result` object
