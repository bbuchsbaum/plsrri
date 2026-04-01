# Predictive Cross-Validation for PLS

Leakage-safe predictive evaluation for fitted PLS pipelines. The
decomposition is refit inside every training fold and held-out subjects
are projected with training-only artifacts via
[`project_scores()`](https://bbuchsbaum.github.io/plsrri/reference/project_scores.md).
