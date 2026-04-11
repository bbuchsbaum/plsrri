# Command Line Interface

``` r
library(plsrri)
```

`plsrri` ships an installable `plscli` wrapper for staged non-GUI
workflows. The command is designed for the same pipeline contract that
powers
[`prepare_firstlevel()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_firstlevel.md),
[`prepare_pls()`](https://bbuchsbaum.github.io/plsrri/reference/prepare_pls.md),
[`run_pls()`](https://bbuchsbaum.github.io/plsrri/reference/run_pls.md),
and
[`render_pls_report()`](https://bbuchsbaum.github.io/plsrri/reference/render_pls_report.md).

This article is about the shell interface itself: installation, help,
command shapes, machine-readable output, and exit behavior. For the YAML
schema, see
[`vignette("pipeline-yaml-spec")`](https://bbuchsbaum.github.io/plsrri/articles/pipeline-yaml-spec.md).
For a broader end-to-end analysis example, see
[`vignette("scripted-workflows")`](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md).

## How do you install the `plscli` wrapper?

Install the package, then copy the wrapper into a directory on `PATH`.

``` r
plsrri::install_cli("~/.local/bin", overwrite = TRUE)
```

If needed, add that directory to your shell startup file:

``` bash
export PATH="$HOME/.local/bin:$PATH"
```

Then check the command:

``` bash
plscli --help
plscli help report
```

When working from a source checkout, the wrapper can also bootstrap the
local package via `pkgload` or `devtools` if `plsrri` is not yet
installed.

## What commands does `plscli` expose?

The command surface is intentionally small and stage-oriented:

- `template`
- `validate`
- `discover`
- `firstlevel-plan`
- `firstlevel-run`
- `pls-plan`
- `pls-run`
- `report`
- `summarize`
- `run`

The usual starting point is a scaffolded YAML file:

``` bash
plscli template --out study.yml
```

Then validate the specification before launching expensive work:

``` bash
plscli validate --spec study.yml
```

## What does a typical staged workflow look like?

For a workstation or scheduler-driven run, the stage boundaries are
explicit:

``` bash
plscli discover --spec study.yml
plscli firstlevel-plan --spec study.yml
plscli firstlevel-run --spec study.yml --work-id w0007
plscli pls-plan --spec study.yml
plscli pls-run --spec study.yml
plscli report --input study.yml --format html
```

For array execution, the main difference is the first-level worker id:

``` bash
plscli firstlevel-plan --spec study.yml
plscli firstlevel-run --spec study.yml --work-id "${SLURM_ARRAY_TASK_ID}"
plscli pls-plan --spec study.yml
plscli pls-run --spec study.yml
```

The `report` command accepts either the YAML spec, an artifact root, or
a saved `pls_result.rds` path:

``` bash
plscli report --input /path/to/plscli-out --format pdf --output report.pdf
```

## How do options and help behave?

Commands support both `--key value` and `--key=value` forms:

``` bash
plscli report --input=study.yml --format=html
```

Command-specific help is available in either style:

``` bash
plscli help firstlevel-run
plscli firstlevel-run --help
```

Unknown options are treated as usage errors rather than being ignored.
Boolean flags use `--flag`, and `report` also accepts `--no-open` when
you want to disable browser launching explicitly.

## Can the CLI emit machine-readable output?

Yes. Add `--json` when you want a stable stdout payload for scripts or
CI:

``` bash
plscli template --out study.yml --json
plscli validate --spec study.yml --json
plscli report --input study.yml --format html --json
```

Human-facing diagnostics still go to stderr where practical.

## What exit codes should you expect?

The CLI returns:

- `0` for success
- `1` for domain failures such as an invalid pipeline spec
- `2` for usage or runtime errors

That makes it safe to call from shell scripts, schedulers, and CI
wrappers:

``` bash
plscli validate --spec study.yml --json
status=$?
if [ "$status" -ne 0 ]; then
  echo "validation failed with status $status" >&2
  exit "$status"
fi
```

## Where should you go next?

- [`vignette("pipeline-yaml-spec")`](https://bbuchsbaum.github.io/plsrri/articles/pipeline-yaml-spec.md)
  for the YAML contract
- [`vignette("scripted-workflows")`](https://bbuchsbaum.github.io/plsrri/articles/scripted-workflows.md)
  for the broader saved-artifact workflow
- [`?install_cli`](https://bbuchsbaum.github.io/plsrri/reference/install_cli.md)
- [`?plscli_main`](https://bbuchsbaum.github.io/plsrri/reference/plscli_main.md)
