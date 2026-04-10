# Pipeline YAML Specification

``` r
library(plsrri)
library(yaml)
```

The YAML file is the durable contract for the non-GUI pipeline. It is
what
[`prepare_firstlevel()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md),
[`prepare_pls()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md),
[`run_pls()`](https://bbuchsbaum.github.io/plsrri/reference/run_pls.md),
`plscli`, and the Shiny import/export path all share.

This article is a reference for that contract. It focuses on structure,
required fields, and common variants. For the scripted workflow itself,
see
[`vignette("scripted-workflows")`](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md).

The intended starting point is a scaffold, not a blank file:

``` r
write_pipeline_template("study.yml")
```

``` bash
plscli template --out study.yml
```

## What sections does the spec contain?

The scaffold written by
[`write_pipeline_template()`](https://bbuchsbaum.github.io/plsrri/reference/write_pipeline_template.md)
gives the intended shape.

``` r
required_sections
#> [1] "dataset"     "design"      "first_level" "pls"         "execution"  
#> [6] "outputs"
```

Those sections have distinct roles:

- `dataset`: where the BIDS data live and how subjects/tasks are
  selected
- `design`: how first-level regressors are built
- `first_level`: what first-level outputs to write
- `pls`: how those outputs are mapped into a PLS analysis
- `execution`: local versus array execution settings
- `outputs`: where artifacts are written

## What is the smallest valid spec?

The minimum viable spec is deliberately small. You need:

- a BIDS directory
- at least one task label
- a first-level design formula
- a PLS method
- an output root

``` r
cat(as.yaml(minimal_spec))
#> dataset:
#>   bids_dir: /tmp/RtmpVd2Ll3/plsrri-yaml-2764337e4a08/bids
#>   task: stroop
#> design:
#>   formula: onset ~ hrf(condition, basis = 'spmg1')
#> first_level:
#>   output:
#>     type: estimates
#>     statistics: estimate
#> pls:
#>   method: task
#>   nperm: 0
#>   nboot: 0
#> outputs:
#>   root: /tmp/RtmpVd2Ll3/plsrri-yaml-2764337e4a08/out
```

That small object already validates and picks up defaults for
`execution.mode`, `execution.parallelism`, `first_level.strategy`, and
`pls.input`.

## How should you read the top-level sections?

### `dataset`

`dataset` answers: what study is this, and which task/run space should
be used?

Common fields:

- `bids_dir`
- `task`
- `space`
- `group_column`
- optional subject/session/run filters

``` r
dataset:
  bids_dir: /path/to/bids
  task: stroop
  space: MNI152NLin2009cAsym
  group_column: group
```

### `design`

`design` defines the first-level model, not the PLS contrast. The key
field is `formula`.

``` r
design:
  formula: onset ~ hrf(condition, basis = 'spmg1')
  block: ~ run
```

That formula is where you choose single-df HRFs versus basis expansions
such as FIR or tent-style models.

### `first_level`

`first_level` controls how the GLM stage is run and what maps are
written.

``` r
first_level:
  strategy: runwise
  nchunks: 1
  output:
    type: estimates
    statistics: [estimate]
```

The important distinction is:

- `type = estimates`: write condition-level beta-like maps
- `type = contrasts`: write named contrast maps
- `type = F`: write F-statistic outputs

### `pls`

`pls` tells the second stage how to interpret first-level outputs.

``` r
pls:
  method: task
  input:
    type: estimates
    statistic: estimate
  nperm: 1000
  nboot: 500
```

The `method` field maps onto supported `plsrri` methods such as:

- `task`
- `task_nonrotated`
- `behavior`
- `behavior_nonrotated`
- `multiblock`
- `multiblock_nonrotated`

### `execution`

`execution` controls how staged commands are scheduled, not the
statistical analysis itself.

``` r
execution:
  mode: array
  parallelism: 8
```

### `outputs`

`outputs.root` is the artifact root that the CLI, scripted R API, report
layer, and Shiny attach mode all reuse.

``` r
outputs:
  root: plscli-out
```

## How do basis-expanded first-level outputs fit into the spec?

The YAML needs two pieces of information when first-level output labels
encode basis functions such as FIR bins or tent functions:

1.  how first-level labels are written
2.  how PLS should fold those labels back into a basis-aware manifest

``` r
cat(as.yaml(basis_spec))
#> dataset:
#>   bids_dir: /tmp/RtmpVd2Ll3/plsrri-yaml-2764337e4a08/bids
#>   task: stroop
#> design:
#>   formula: onset ~ hrf(condition, basis = 'fir', K = 4)
#> first_level:
#>   output:
#>     type: estimates
#>     statistics: estimate
#>     basis_pattern: ^(.*)_bin([0-9]+)$
#>     basis_order:
#>     - bin1
#>     - bin2
#>     - bin3
#>     - bin4
#> pls:
#>   method: task
#>   input:
#>     type: estimates
#>     statistic: estimate
#>     basis_pattern: ^(.*)_bin([0-9]+)$
#>     condition_group: 1
#>     basis_group: 2
#>     basis_order:
#>     - '1'
#>     - '2'
#>     - '3'
#>     - '4'
#>   nperm: 0
#>   nboot: 0
#> outputs:
#>   root: /tmp/RtmpVd2Ll3/plsrri-yaml-2764337e4a08/fir-out
```

The important point is that basis handling belongs in both places:

- `design.formula` determines what the first-level model estimates
- `pls.input.*` tells the PLS stage how to reinterpret those
  basis-labelled maps

## How does the spec map to CLI stages?

The YAML is consumed incrementally. Not every stage needs every section.

| CLI stage                | Main sections used                            |
|--------------------------|-----------------------------------------------|
| `plscli validate`        | all                                           |
| `plscli discover`        | `dataset`, `outputs`                          |
| `plscli firstlevel-plan` | `dataset`, `design`, `first_level`, `outputs` |
| `plscli firstlevel-run`  | first-level plan artifacts plus `execution`   |
| `plscli pls-plan`        | `pls`, `outputs`                              |
| `plscli pls-run`         | `pls`, planned manifests, `outputs`           |
| `plscli report`          | `outputs` or an existing artifact root        |

That split is why the same YAML can drive:

- a local end-to-end run
- an HPC array workflow
- a Shiny-exported pipeline configuration

## What should you treat as stable?

The stable contract is:

- the YAML structure described here
- the artifact root under `outputs.root`
- the public helpers:
  - [`write_pipeline_template()`](https://bbuchsbaum.github.io/plsrri/reference/write_pipeline_template.md)
  - [`read_pipeline_spec()`](https://bbuchsbaum.github.io/plsrri/reference/read_pipeline_spec.md)
  - [`validate_pipeline_spec()`](https://bbuchsbaum.github.io/plsrri/reference/validate_pipeline_spec.md)
  - [`prepare_firstlevel()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md)
  - [`prepare_pls()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md)
  - [`run_pls()`](https://bbuchsbaum.github.io/plsrri/reference/run_pls.md)
  - [`render_pls_report()`](https://bbuchsbaum.github.io/plsrri/reference/render_pls_report.md)

## Where should you go next?

Use
[`vignette("scripted-workflows")`](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md)
for the staged R and CLI workflow, and
[`?write_pipeline_template`](https://bbuchsbaum.github.io/plsrri/reference/write_pipeline_template.md)
/
[`?read_pipeline_spec`](https://bbuchsbaum.github.io/plsrri/reference/read_pipeline_spec.md)
for the corresponding help pages.
