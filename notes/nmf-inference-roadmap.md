# Future Work: Non-Negative Decompositions and Generic Inference

This note records a future extension path for brain maps whose natural scale is
non-negative, such as AUC, accuracy, probability, reliability, or other bounded
performance maps. These maps can already be passed to the current PLS engines,
but the decomposition remains signed SVD/PLS. A true non-negative analysis would
need a method-specific factorization and a more generic inference layer.

## Motivation

For AUC-like maps, negative saliences can be awkward to interpret because the
input features are not signed effects. Standard PLS answers a covariance or
contrast question, so signed brain saliences are appropriate there. A
non-negative decomposition would answer a different question: whether maps can
be represented as additive spatial components, and how those components relate
to groups, conditions, or continuous covariates.

This should not be treated as a drop-in replacement for PLS. It should be a new
method family with explicit semantics.

## Candidate Method Family

Possible method names:

- `nmf_task`: unsupervised non-negative spatial components with group/condition
  score summaries.
- `nmf_behavior`: non-negative components whose subject scores are related to
  behavior/covariates after factorization.
- `nn_behavior`: supervised non-negative model that uses behavior/covariate
  information during fitting.
- `nn_multiblock`: joint non-negative model for task/group structure plus
  behavior/covariate structure.

The first implementation should probably be `nmf_task` or `nmf_behavior`.
Those are simpler and make fewer claims than a fully supervised constrained
model.

## Recommended First Implementation

Start with an unsupervised NMF over the stacked data matrix:

```text
X >= 0
X ~= S %*% t(W)
```

where:

- `X` is observations by features.
- `W` is features by components and must be non-negative.
- `S` is observations by components and must be non-negative.
- component importance can be reconstruction gain, explained Frobenius norm, or
  another clearly documented criterion.

After fitting, group, condition, and behavior effects can be tested on `S`.
This keeps non-negative spatial components separate from signed experimental
contrasts.

For designs like:

```text
brain map ~ group * trait
```

the first pass should fit NMF to the non-negative maps, then model component
scores with:

```text
component score ~ group * trait
```

This is not PLS, but it is often the more interpretable route for AUC maps.

## MvaMethod Integration

The existing `MvaMethod` protocol is the right extension point. An NMF method
should return `new_mva_result()` with:

- `feature_weights`: non-negative feature/component weights `W`.
- `scores_feature`: non-negative observation/component scores `S`.
- `importance`: method-specific component strengths.
- `design_weights`: optional summaries of group, condition, or covariate
  associations with component scores.
- `extra`: NMF-specific fields such as reconstruction error, convergence
  diagnostics, rank, seed, normalization, and objective.

The method should implement or override:

- `fit(spec, progress = TRUE)`
- `bootstrap_fit(spec, boot_order)`
- `permutation_fit(spec, perm_order)`
- `align_to_reference(boot_decomp, ref_decomp)`
- `project_scores(result, spec, type = "feature")`

The alignment hook is essential because NMF components are unordered and
scale-indeterminate.

## Generic Inference Refactor

The current production inference path is still mostly PLS-specific. It assumes
singular values, PLS saliences, PLS bootstrap ratios, and PLS-specific
permutation statistics. A generic inference frame should factor out the common
resampling loop while letting each method define the statistic and alignment.

Proposed generic pieces:

```text
generate_resamples(spec, scheme, n)
fit_resample(method, spec, order)
align_resample(method, resample_decomp, observed_decomp)
summarize_resamples(method, observed_decomp, aligned_decomps)
```

The method should provide:

- `importance_stat(decomp)`: values used for component-level permutation tests.
- `feature_stat(decomp)`: feature/component matrix used for stability.
- `score_stat(decomp)`: optional observation/component score summaries.
- `match_components(candidate, reference)`: component assignment plus any
  sign/scale handling.
- `null_compare(observed, null)`: tail rule for p-values.
- `bootstrap_summary(observed, samples)`: intervals, ratios, or stability
  frequencies.

For PLS, this generic layer would wrap the existing behavior rather than
replace the optimized MATLAB-parity paths immediately.

## Resampling Schemes

The resampling generator should be shared across methods, but scheme choice is
method- and design-dependent.

Candidate schemes:

- subject bootstrap within group
- subject permutation within or across group
- condition permutation for task designs
- behavior/covariate permutation for supervised behavior methods
- site-stratified bootstrap/permutation

The scheme must be stored in the result so downstream reports can state the
actual null hypothesis.

## NMF-Specific Inference

Permutation testing for NMF should not blindly reuse PLS singular-value logic.
Possible statistics:

- reconstruction improvement over a null model
- component-wise explained non-negative variance
- group or covariate association strength in component scores
- held-out reconstruction error
- held-out prediction from component scores

Bootstrap summaries should focus on stability:

- component reproducibility after matching
- feature weight intervals or selection frequencies
- score intervals for group/condition summaries
- consensus components across resamples

Bootstrap ratios analogous to PLS BSRs may be misleading for non-negative
weights because the null and boundary behavior differ. Prefer intervals,
selection frequencies, or stability scores unless a defensible ratio is defined.

## Component Matching

NMF components have label switching and arbitrary scale. Matching should happen
before any bootstrap summary:

1. Normalize each component to a fixed convention, such as unit L2 norm or unit
   sum over features.
2. Compute component similarity to the reference, such as cosine similarity.
3. Solve the assignment problem to match bootstrap components to reference
   components.
4. Drop or mark unmatched components below a similarity threshold.
5. Store matching diagnostics.

Unlike PLS, no sign flip should be applied to non-negative feature weights.

## Open Design Questions

- Should the first NMF method decompose raw `X`, centered `X`, or shifted
  deviations such as `AUC - 0.5` plus a non-negative offset?
- How should bounded inputs be transformed, if at all?
- Which NMF backend should be used: base R implementation, `NMF`, `RcppML`, or a
  small internal multiplicative-update solver?
- Should supervised non-negative models be in scope, or should supervision be
  handled by post-factorization models of component scores?
- What minimum output contract is needed so existing plotting and reporting
  functions remain useful?

## Suggested Milestones

1. Add a generic resampling result shape for `mva_result` that is not tied to
   `pls_perm_result` or `pls_boot_result`.
2. Implement a small generic bootstrap/permutation driver for `MvaMethod`.
3. Add `NmfMethod` with unsupervised non-negative factorization and deterministic
   seeding.
4. Add component matching and bootstrap stability summaries.
5. Add a score-level group/covariate inference helper for NMF scores.
6. Write a vignette using non-negative AUC maps and a `group * trait` score
   model.
7. Only after the unsupervised path is stable, consider supervised
   non-negative behavior or multiblock methods.
