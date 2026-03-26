# Procrustes Rotation Matrix (MATLAB rri_bootprocrust)

Computes the orthogonal rotation matrix that aligns `bootlv` to
`origlv`. Ported from MATLAB `rri_bootprocrust.m`.

## Usage

``` r
pls_bootprocrust(origlv, bootlv)
```

## Arguments

- origlv:

  Original LV matrix

- bootlv:

  Bootstrap/permuted LV matrix

## Value

Rotation matrix
