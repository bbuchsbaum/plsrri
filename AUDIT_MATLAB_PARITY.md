# plsrri MATLAB Parity Audit (McIntosh Lab PLS)

This repo vendors the original MATLAB implementation under `PLS/` and
provides an R package implementation under `R/`.

This document summarizes **function-level parity**, **known gaps**, and
the **highest‑impact algorithmic differences** between the two
codebases.

## Scope

- **MATLAB reference**: `PLS/plscmd/` (computational core) and
  `PLS/plsgui/` (GUI).
- **R implementation**: `R/` (+ `src/` for optional Rcpp helpers).
- Focus here is computational equivalence for:
  - **Task PLS** (methods 1–2)
  - **Behavior PLS** (methods 3–5)
  - **Multiblock PLS** (methods 4 & 6)
  - **Permutation / bootstrap inference**

## High-level parity map

| MATLAB (PLS/plscmd) | R (plsrri) | Status | Notes |
|----|----|---:|----|
| `pls_analysis.m` | `R/pls_analysis.R` ([`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)) | **Mostly** | Core analysis + resampling; see gaps below |
| `rri_get_covcor.m` | `R/pls_covariance.R` ([`pls_get_covcor()`](https://bbuchsbaum.github.io/plsrri/reference/pls_get_covcor.md)) | **Yes** | Includes MATLAB-style `stacked_smeanmat` (opt-in via `compute_smeanmat`) |
| `rri_task_mean.m` | `R/pls_meancentering.R` ([`pls_task_mean()`](https://bbuchsbaum.github.io/plsrri/reference/pls_task_mean.md)) | **Yes** | Condition means (balanced) |
| `ssb_rri_task_mean.m` | `R/pls_meancentering.R` ([`pls_task_mean_ssb()`](https://bbuchsbaum.github.io/plsrri/reference/pls_task_mean_ssb.md)) | **Yes** | Wired end-to-end via `num_subj_lst` list support |
| `rri_xcor.m` | `R/pls_xcor.R` ([`pls_xcor()`](https://bbuchsbaum.github.io/plsrri/reference/pls_xcor.md)) | **Yes** | Pearson/cov/cosine/dot modes |
| `rri_corr_maps.m` | `R/pls_covariance.R` ([`pls_corr_maps()`](https://bbuchsbaum.github.io/plsrri/reference/pls_corr_maps.md)) | **Yes** | Uses [`pls_xcor()`](https://bbuchsbaum.github.io/plsrri/reference/pls_xcor.md) per condition |
| `rri_corr_maps_notall.m` | `R/pls_covariance.R` ([`pls_corr_maps_notall()`](https://bbuchsbaum.github.io/plsrri/reference/pls_corr_maps_notall.md)) | **Yes** | bscan subset support |
| `rri_get_behavscores.m` | `R/pls_scores.R` ([`pls_get_behavscores()`](https://bbuchsbaum.github.io/plsrri/reference/pls_get_behavscores.md)) | **Yes** | Implemented for MATLAB‑parity scoring/loading outputs |
| `rri_perm_order.m` | `R/resample_perm.R` ([`pls_perm_order()`](https://bbuchsbaum.github.io/plsrri/reference/pls_perm_order.md)) | **Yes** | Task/block permutation order |
| `rri_randperm_notall.m` | `R/utils.R` ([`pls_randperm_notall()`](https://bbuchsbaum.github.io/plsrri/reference/pls_randperm_notall.md)) | **Yes** | Multiblock behavior-row permutation only |
| `rri_boot_order.m` | `R/resample_boot.R` ([`pls_boot_order()`](https://bbuchsbaum.github.io/plsrri/reference/pls_boot_order.md)) | **Yes** | Includes bscan mapping + sequential sample option |
| `rri_bootprocrust.m` | `R/utils.R` ([`pls_bootprocrust()`](https://bbuchsbaum.github.io/plsrri/reference/pls_bootprocrust.md)) | **Yes** | Orthogonal Procrustes rotation matrix |
| `misssvd.m` | `R/pls_svd.R` ([`misssvd()`](https://bbuchsbaum.github.io/plsrri/reference/misssvd.md)) | **Mostly** | NA/NaN handling matches; Inf handling differs from MATLAB (see gaps) |
| `rri_distrib.m` | `R/statistics.R` ([`pls_distrib_ci()`](https://bbuchsbaum.github.io/plsrri/reference/pls_distrib_ci.md), [`pls_percentile()`](https://bbuchsbaum.github.io/plsrri/reference/pls_percentile.md)) | **Yes** | Percentile + adjusted CI pipeline replicated |
| Split-half (`splithalf_*`) | `R/resample_splithalf.R` ([`pls_splithalf_test()`](https://bbuchsbaum.github.io/plsrri/reference/pls_splithalf_test.md)) | **Yes** | Outer permutation + split correlation distributions + LV-wise p-values |

## Key algorithmic mismatches addressed

Recent work in the R codebase focuses on bringing **method 3–6 scoring**
and **inference resampling** closer to MATLAB:

- **Behavior PLS scoring**:
  - MATLAB uses `rri_get_behavscores()` to produce `usc`, `vsc`, and
    `lvcorrs`.
  - R now mirrors this via
    [`pls_get_behavscores()`](https://bbuchsbaum.github.io/plsrri/reference/pls_get_behavscores.md)
    and uses it in
    [`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)
    for **methods 3 & 5**.
- **Multiblock scoring and bookkeeping**:
  - MATLAB returns concatenated **task + behavior** score blocks and
    stores per-block components (`TBv`, `TBusc`, `TBvsc`).
  - R now computes and stores these fields for **methods 4 & 6**,
    matching MATLAB’s row ordering and block concatenation.
- **Multiblock `v` row splitting**:
  - MATLAB stacks `v` rows *per group* as:
    `[task rows] then [behavior rows]`.
  - R’s
    [`pls_split_multiblock_v()`](https://bbuchsbaum.github.io/plsrri/reference/pls_split_multiblock_v.md)
    now follows this per-group interleaving instead of assuming all task
    rows precede all behavior rows.
- **Permutation tests**:
  - MATLAB uses different shuffles by method:
    - (1–2): permute datamat via `rri_perm_order`
    - (3–5): permute **behavior rows only**
    - (4–6): permute task block via `rri_perm_order` and permute **bscan
      rows only** via `rri_randperm_notall`
  - R now follows this logic and uses **Procrustes alignment**
    (`rri_bootprocrust`) for SVD-based methods to keep LV
    correspondence.
  - For **method 4** (rotated multiblock), R now applies the same **SSQ
    rescaling** step used by MATLAB during permutation significance
    testing.
- **Bootstrap test (BSR)**:
  - MATLAB computes bootstrap SEs on **scaled** saliences (`u * s`)
    after Procrustes alignment.
  - R now computes `compare_u` (BSR) and `u_se` using MATLAB‑style
    accumulation and denominators, and uses bscan-aware bootstrap orders
    for multiblock.

## Known legacy MATLAB edge-case bug (documented; not ported)

- **`Inf` handling in `missnk_*` helpers**:
  - In vendored `PLS/plscmd`, missing-data helper stack
    (`missnk_rri_xcor` + `misssum`/`missnk_mean`) relies on `isnan(...)`
    checks internally, so `Inf` values are not consistently treated as
    missing.
  - Result: with `Inf` inputs, MATLAB/Octave can propagate non-finite
    values in correlation outputs.
  - `plsrri` deliberately treats **all non-finite values**
    (`NA`/`NaN`/`Inf`) as missing in
    [`pls_xcor()`](https://bbuchsbaum.github.io/plsrri/reference/pls_xcor.md)
    missing-data mode, preserving finite outputs where data support
    exists.
  - This is covered by Octave parity tests: we preserve MATLAB agreement
    on `NaN` cases, while intentionally not porting the
    `Inf`-propagation behavior.
- **Permutation probability normalization in `pls_analysis.m`**:
  - MATLAB/Octave computes `perm_result.sprob` as `sp / (num_perm + 1)`
    in regular permutation mode.
  - `plsrri` uses `sp / num_perm` for regular permutation tests (the
    explicit number of sampled permutations).
  - We treat MATLAB’s `+1` normalization as legacy behavior and keep the
    R implementation on the direct `num_perm` denominator.
- **Trailing near-null LV instability in rotated multiblock permutation
  counts**:
  - For method 4 with tiny trailing singular values, MATLAB and R can
    disagree on the final LV count (`sp`) while agreeing on
    non-negligible LVs.
  - Octave parity tests therefore assert strict equality on stable LVs
    and treat the numerically null tail LV as non-inferential.

## Remaining parity gaps (most important)

The core **non-GUI MATLAB parity gaps** tracked above have been
addressed:

1.  **Task brain-score CIs (`stacked_smeanmat` / `usc2`)**: implemented
    and wired into bootstrap CIs.
2.  **Bootstrap CI distribution logic (`rri_distrib.m`)**: implemented
    (`*_adj` + `prop`).
3.  **Split-half validation**: updated to match MATLAB’s
    outer-permutation / split distribution logic.
4.  **SSB designs (`ssb_*`)**: `num_subj_lst` list support is wired
    end-to-end for analysis + perm + bootstrap.
5.  **Missing-data variants (`missnk_*`)**: missing-aware
    cross-correlation is implemented (missnk-style) and used throughout.

Remaining parity work is now mostly in the **GUI layer** (`PLS/plsgui/`
vs `inst/shiny/`) and in adding “golden” MATLAB fixtures for automated
cross-language regression testing.

## Recommendations (next steps)

If the goal is a **faithful + idiomatic + fast** R implementation:

1.  Implement `stacked_smeanmat` in
    [`pls_get_covcor()`](https://bbuchsbaum.github.io/plsrri/reference/pls_get_covcor.md)
    and port `rri_distrib.m` semantics to fill the CI fields already
    present in `pls_boot_result`.
2.  Bring
    [`pls_splithalf_test()`](https://bbuchsbaum.github.io/plsrri/reference/pls_splithalf_test.md)
    to MATLAB parity (outer permutation + split-half correlation
    distributions).
3.  Wire ssb support end-to-end (allow `num_subj_lst` as
    list-of-integers and route to ssb equivalents).
4.  Consolidate resampling loops to use existing Rcpp helpers in `src/`
    (or remove them if not used).
5.  Add optional “MATLAB golden” tests:
    - Export a small MATLAB `.mat` fixture set and compare R results via
      `R.matlab` when available.
