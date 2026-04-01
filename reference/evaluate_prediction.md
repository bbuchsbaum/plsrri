# Evaluate Predictive Performance with Nested Cross-Validation

Evaluate Predictive Performance with Nested Cross-Validation

## Usage

``` r
evaluate_prediction(
  spec,
  outcome,
  task = c("classification", "regression"),
  components = NULL,
  resampling = NULL,
  metrics = NULL,
  primary_metric = NULL,
  seed = NULL,
  progress = TRUE,
  num_perm = 0L,
  num_boot = 0L,
  clim = 95
)
```

## Arguments

- spec:

  A `pls_spec` object.

- outcome:

  Subject-level outcome vector ordered by group, then subject.

- task:

  `"classification"` or `"regression"`.

- components:

  Optional integer vector of candidate LV counts. When NULL, each outer
  split tunes over `1:min(5, available_components)`.

- resampling:

  Optional list controlling nested CV. Supported fields are
  `outer_folds`, `outer_repeats`, `inner_folds`, `inner_repeats`,
  `stratify`, plus shorthand `folds`/`repeats`.

- metrics:

  Optional character vector of metric names.

- primary_metric:

  Optional metric used for inner-loop selection.

- seed:

  Optional integer seed for reproducible split generation.

- progress:

  Logical; show a progress bar.

- num_perm:

  Optional number of subject-level label permutations for predictive
  inference.

- num_boot:

  Optional number of subject-level bootstrap resamples for predictive
  confidence intervals.

- clim:

  Confidence level for bootstrap intervals.

## Value

A `predict_cv_result` list with out-of-fold predictions, per-fold
metrics, split membership, and tuning traces.
