# plsrri 0.1.0

- Added deterministic Octave-backed parity tests for core MATLAB `PLS/plscmd` helpers and non-rotated end-to-end analysis methods (`2`, `5`, `6`).
- Added deterministic method-4 Octave parity coverage using fixed resampling matrices (`permsamp`/`Tpermsamp` and `bootsamp`/`bootsamp_4beh`).
- Added deterministic methods-3/5 Octave parity coverage with fixed `permsamp`/`bootsamp` paths, including permutation/bootstrap output checks.
- Added deterministic method-1 Octave parity coverage with fixed `permsamp`/`bootsamp` paths, including inference-output checks.
- Added deterministic methods-2/6 Octave parity coverage with fixed permutation/bootstrap order matrices, including inference-output checks.
- Added support for caller-supplied permutation/bootstrap order matrices in `pls_analysis()` and wired pass-through to inference internals.
- Added generated Octave split-half fixture coverage under `tests/testthat/fixtures/matlab/splithalf_result.rds`.
- Documented intentional correctness divergence for missing-data handling:
  `plsrri` treats all non-finite values (`NA`/`NaN`/`Inf`) as missing in `pls_xcor()` missing-data mode, while legacy MATLAB `missnk_*` utilities can propagate non-finite output with `Inf` inputs.
