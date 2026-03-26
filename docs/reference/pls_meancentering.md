# Apply Mean-Centering to Data Matrix

Applies one of four mean-centering types to a data matrix organized by
groups and conditions.

## Usage

``` r
pls_meancentering(datamat_lst, num_subj_lst, num_cond, meancentering_type = 0L)
```

## Arguments

- datamat_lst:

  List of data matrices (one per group)

- num_subj_lst:

  Subjects per group. For balanced designs, a numeric vector (or scalar)
  giving the number of subjects in each group. For SSB designs, a list
  of integer vectors, one per group, each of length `num_cond` giving
  subjects per condition.

- num_cond:

  Number of conditions

- meancentering_type:

  Mean-centering type (0-3):

  0

  :   Remove group condition means from condition means within each
      group. Highlights condition effects modulated by group membership.

  1

  :   Remove grand condition means from each group condition mean.
      Highlights group differences, removes overall condition
      differences.

  2

  :   Remove grand mean over all subjects and conditions. Shows full
      spectrum of condition and group effects.

  3

  :   Remove all main effects (condition and group means). Pure group by
      condition interaction.

## Value

List with:

- centered:

  List of centered data matrices by group

- grand_mean:

  Grand mean (type 1-3) or NULL (type 0)

- group_mean:

  Group means (type 3) or NULL

- cond_mean:

  Condition means by group (type 3) or NULL
