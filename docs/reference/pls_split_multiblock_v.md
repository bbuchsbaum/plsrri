# Extract Task/Behavior Scores Separately (Multiblock)

For multiblock PLS, extracts the task block and behavior block
components of the design loadings (v) and scores separately.

## Usage

``` r
pls_split_multiblock_v(v, num_groups, num_cond, n_behav, bscan)
```

## Arguments

- v:

  Design loadings from SVD

- num_groups:

  Number of groups

- num_cond:

  Number of conditions

- n_behav:

  Number of behavior measures

- bscan:

  Conditions used in behavior block

## Value

List with task_v, behav_v components
