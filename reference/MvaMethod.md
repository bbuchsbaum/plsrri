# MvaMethod R6 Class

Base class for multivariate analysis methods. Subclass this to implement
new methods (e.g., CPCA, CCA, ICA). See `PlsMethod` for an example.

## Public fields

- `name`:

  Short identifier (e.g., "pls_task", "cpca")

- `label`:

  Human-readable label (e.g., "Task PLS", "Constrained PCA")

- `family`:

  Method family grouping variants (e.g., "pls", "cpca")

- `capabilities`:

  Named list declaring what this method needs and produces

## Methods

### Public methods

- [`MvaMethod$new()`](#method-MvaMethod-new)

- [`MvaMethod$fit()`](#method-MvaMethod-fit)

- [`MvaMethod$permutation_fit()`](#method-MvaMethod-permutation_fit)

- [`MvaMethod$bootstrap_fit()`](#method-MvaMethod-bootstrap_fit)

- [`MvaMethod$project_scores()`](#method-MvaMethod-project_scores)

- [`MvaMethod$align_to_reference()`](#method-MvaMethod-align_to_reference)

- [`MvaMethod$validate_spec()`](#method-MvaMethod-validate_spec)

- [`MvaMethod$configure()`](#method-MvaMethod-configure)

- [`MvaMethod$ui_config()`](#method-MvaMethod-ui_config)

- [`MvaMethod$clone()`](#method-MvaMethod-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MvaMethod instance

#### Usage

    MvaMethod$new(name, label, family, capabilities = list())

#### Arguments

- `name`:

  Short identifier

- `label`:

  Human-readable label

- `family`:

  Method family

- `capabilities`:

  Named list of capability flags

------------------------------------------------------------------------

### Method `fit()`

Fit the method to data. Must return an `mva_decomposition`.

#### Usage

    MvaMethod$fit(spec, progress = TRUE)

#### Arguments

- `spec`:

  An `mva_spec` or `pls_spec` object

- `progress`:

  Logical, show progress

#### Returns

An `mva_decomposition` object

------------------------------------------------------------------------

### Method `permutation_fit()`

Fit on permuted data for significance testing. Default: permute rows of
data matrices, then call fit().

#### Usage

    MvaMethod$permutation_fit(spec, perm_order)

#### Arguments

- `spec`:

  An `mva_spec` or `pls_spec` object

- `perm_order`:

  Integer vector of permuted row indices

#### Returns

An `mva_decomposition` object

------------------------------------------------------------------------

### Method `bootstrap_fit()`

Fit on bootstrapped data for stability estimation. Default: resample
rows, then call fit().

#### Usage

    MvaMethod$bootstrap_fit(spec, boot_order)

#### Arguments

- `spec`:

  An `mva_spec` or `pls_spec` object

- `boot_order`:

  Integer vector of bootstrap row indices

#### Returns

An `mva_decomposition` object

------------------------------------------------------------------------

### Method [`project_scores()`](https://bbuchsbaum.github.io/plsrri/reference/project_scores.md)

Project held-out observations into the fitted score space. Subclasses
should use only training-fit artifacts stored on `result`.

#### Usage

    MvaMethod$project_scores(result, spec, type = "feature", progress = FALSE)

#### Arguments

- `result`:

  A fitted `mva_result`

- `spec`:

  Held-out `mva_spec` or `pls_spec`

- `type`:

  Score space to project into

- `progress`:

  Logical, show progress

#### Returns

Matrix of held-out scores, or a named list when `type = "both"`

------------------------------------------------------------------------

### Method `align_to_reference()`

Align bootstrap decomposition to reference (e.g., Procrustes). Default:
sign-flip columns to maximize agreement.

#### Usage

    MvaMethod$align_to_reference(boot_decomp, ref_decomp)

#### Arguments

- `boot_decomp`:

  An `mva_decomposition` from bootstrap

- `ref_decomp`:

  The reference `mva_decomposition`

#### Returns

Aligned `mva_decomposition`

------------------------------------------------------------------------

### Method `validate_spec()`

Validate that a spec has everything this method needs.

#### Usage

    MvaMethod$validate_spec(spec)

#### Arguments

- `spec`:

  An `mva_spec` or `pls_spec` object

#### Returns

TRUE invisibly, or stops with error

------------------------------------------------------------------------

### Method [`configure()`](https://bbuchsbaum.github.io/plsrri/reference/configure.md)

Apply method-specific configuration to a spec. Override to handle
method-specific parameters.

#### Usage

    MvaMethod$configure(spec, ...)

#### Arguments

- `spec`:

  An `mva_spec` or `pls_spec` object

- `...`:

  Method-specific parameters

#### Returns

Modified spec

------------------------------------------------------------------------

### Method `ui_config()`

UI metadata for the Shiny app. Override to customize.

#### Usage

    MvaMethod$ui_config()

#### Returns

Named list with analysis_stages, explore_panels, inspector_metrics

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MvaMethod$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
