# Tests

`tests/testthat.R` loads `testthat` and `redcapmissing`, then runs the package tests with `test_check("redcapmissing")`. Keep `tests/testthat/` flat: filenames identify the owning function, class, structural columns, validation check, or report component.

## Package workflow

The test suite follows the public workflow established by the package:

```text
all_instruments()
        |
build_explicit_schedule()
        |
plan_from_data() or plan_explicit() -> redcapmissing_plan
        |
run_plan() -> redcapmissing
        |
get_summary() or get_missing()
        |
flexify(), flex_event_instruments(), or flex_html()
```

`build_extended_schedule()` supplies an optional schedule to `plan_from_data()`. `build_explicit_schedule()` supplies a project-aware exact schedule to `plan_explicit()`, which derives its instrument scope from that schedule. `registry()` describes the validation checks that `run_plan()` reports.

## Package behavior and test ownership

Authored roxygen comments in `R/` and `vignettes/redcapmissing.Rmd` are the package-facing sources for user-visible behavior. Tests provide executable evidence for those contracts. This file owns test placement, fixture ownership, evidence layers, and validation commands; it does not restate function contracts.

If a test establishes user-visible behavior that is absent from package documentation, add it to the owning roxygen topic or the shipped vignette. Do not create a second behavior ledger in `tests/`.

| Package code or value | Owning test files |
|---|---|
| `all_instruments`, `build_explicit_schedule`, `build_extended_schedule`, `plan_from_data`, `plan_explicit`, `run_plan`, `get_summary`, `get_missing`, `registry`, `flexify`, `flex_event_instruments`, and `flex_html`; their formals and S3 registrations; absence of `find_missing` and `flex_event_forms` | `test-public-api.R` |
| `redcapmissing_error_*`, `redcapmissing_warning_empty_arm_extension`, and `redcapmissing_warning_undesignated_extension` inheritance | `test-conditions.R` |
| `registry()`, `redcapmissing_registry`, and registry printing | `test-registry.R` |
| `all_instruments()` | `test-all-instruments.R` |
| `build_extended_schedule()` | `test-build-extended-schedule.R` |
| `build_explicit_schedule()` | `test-build-explicit-schedule.R` |
| Shared `redcapmissing_plan` representation, validation, project labels, supported `rcon` classes, structure reads, and zero record exports | `test-redcapmissing-plan.R` |
| Observed and `extended_schedule` target selection | `test-plan-from-data.R` |
| `explicit_schedule`, absent records, omissions, and zero-target plans | `test-plan-explicit.R` |
| `record_id`, `redcap_event_name`, `redcap_repeat_instrument`, `redcap_repeat_instance`, schedule schemas, and normalized duplicate keys | `test-schema-normalization.R` |
| `structure_fingerprint` determinism and project-structure changes | `test-plan-structure-fingerprint.R` |
| `assessible_targets` identity, ordering, six-column schema, cardinality, and `target_source` | `test-plan-assessible-targets.R` |
| `run_plan()` result class and component order, twelve diagnostics stages, privacy exclusions, `details` and `progress` controls, and progress cleanup | `test-run-plan.R` |
| Full/shared comparison strata, transitions, scope, settings, compatibility, serialization, and reconciliation | `test-compare-reports.R` |
| Comparison event/instrument denominators, formatter agreement, threshold boundaries, labels, and optional dependencies | `test-flex-comparison.R` |
| Plan validation, newer `data` snapshots, frozen response scope, structural columns, and project-surface reads | `test-run-plan-data-scope.R` |
| `event-row-started`, `repeat-instance-row-started`, absent targets, repeat context, and downstream `"not reached"` results | `test-run-plan-target-gates.R` |
| Detection fields, checkbox children, and `instrument-started` | `test-run-plan-instrument-started.R` |
| `required_fields`, `ignore_fields`, `exclude_types`, response missingness, and `field-complete` | `test-run-plan-field-complete.R` |
| Same-event, cross-event, repeated-source, and vectorized REDCap branching logic | `test-redcap-branching-logic.R` |
| `details`, dispositions, reasons, `branch_satisfied`, `value_summary`, `verification_applied`, and compact/detailed equivalence | `test-run-plan-details.R` |
| Typed `target_results`, `summary`, `missing`, `verification`, `diagnostics`, URLs, and result reconciliation | `test-run-plan-report-components.R` |
| Verification schema, storage, contexts, and timestamps | `test-run-plan-verification-input.R` |
| `verified_user`, `"VERIFIED"`, latest timestamps, and tied rows | `test-run-plan-verification-contexts.R` |
| Eligible overrides, audit counts, and invariance of targets and upstream gates | `test-run-plan-verification-overrides.R` |
| `get_summary()`, `get_missing()`, shared filters, ordering, empty schemas, and labels | `test-report-accessors.R` |
| `flexify()`, `flex_event_instruments()`, `flex_html()`, and their optional dependencies | `test-flexify.R`, `test-flex-event-instruments.R`, and `test-flex-html.R` |
| Credential-free `redcapAPI::offlineConnection()` workflow | `test-redcapapi-offline-workflow.R` |
| `.onAttach()`, `.startup_build_message()`, and `.startup_get_version()` | `test-startup.R` |

Put a regression test in the file that owns the affected package behavior. Keep one authoritative assertion unless a second test protects a different boundary, such as the synthetic package suite and the `redcapAPI::offlineConnection()` workflow.

## Shared fixtures

- `helper-redcap-fixtures.R` owns `redcap_api_connection_fixture()` and `meta_row()`.
- `helper-plan-fixtures.R` owns shared classic and longitudinal `redcapmissing_plan` inputs.
- `helper-schedule-fixtures.R` owns instrument-inventory and schedule connection fixtures.
- `helper-run-plan-fixtures.R` owns shared `run_plan()` connections, `data`, explicit schedules, and repeating-event inputs.
- `helper-verification-fixtures.R` owns `run_plan_verified_row()`.
- `helper-report-fixtures.R` owns typed empty report components and complete synthetic `redcapmissing` objects.

Keep a fixture local when only one test file calls it. Each helper must return a fresh synthetic object. Do not add `setup*.R`, `teardown*.R`, shared mutable objects, tokens, production metadata, or live REDCap requests.

## Evidence layers

- Deterministic package tests call exported functions and assert documented functions, classes, structural columns, validation checks, report components, conditions, schemas, and ordering.
- Internal domain tests directly exercise normalization, `structure_fingerprint`, REDCap branching logic, target gates, and report construction where those contracts carry independent risk.
- `test-redcapapi-offline-workflow.R` exercises the public workflow against `redcapAPI::offlineConnection()` without credentials or network access.
- Presentation tests use `skip_if_not_installed()` for optional packages and test `flextable` or HTML results only when their dependencies are installed.
- `tools/audit-plan-run-boundary.R` verifies the PR 60 planning and assessment source boundary; it does not replace behavior tests.
- `tools/benchmark-plan-run.R` runs deterministic synthetic workloads and checks result equivalence before reporting time or allocation evidence; it is not a unit-test timing threshold. Its `high-cardinality` family defaults to the U2 (100 records, 600 instruments, 2 fields per instrument) and U10 (100, 600, 10) maintainer baselines. Opt-in scenarios cover shared and per-instrument branching, first/last/absent instrument-start responses, failure density, compact versus detailed reports, disabled/empty/sparse/dense verification, dependency width, unrelated data width, and progress. Timed expressions never run under `Rprofmem()`; requested allocation profiles are separate executions.

## Commands

Run an owning file while changing its domain:

```sh
Rscript -e "devtools::load_all('.', quiet = TRUE, export_all = TRUE); testthat::test_file('tests/testthat/test-run-plan-field-complete.R')"
```

Run the complete development suite:

```sh
Rscript -e "devtools::test()"
```

Run the planning and assessment source-boundary audit:

```sh
Rscript tools/audit-plan-run-boundary.R
```

Run the `branching` and `detail-allocation` benchmark workloads:

```sh
Rscript -e "Sys.setenv(REDCAPMISSING_BENCH_TIER = 'branching,detail-allocation'); source('tools/benchmark-plan-run.R')"
```

Run the Phase 1 high-cardinality U2 and U10 baselines:

```sh
Rscript -e "Sys.setenv(REDCAPMISSING_BENCH_TIER = 'high-cardinality', REDCAPMISSING_BENCH_SCENARIO = 'U2,U10'); source('tools/benchmark-plan-run.R')"
```

Use `REDCAPMISSING_BENCH_SCENARIO=all` to run every high-cardinality control, or supply a comma-separated subset from the scenario names printed by an invalid selection. Counts and iteration controls remain available through the documented `REDCAPMISSING_BENCH_*` variables.

Raw iteration rows, raw stage rows, summaries, exact sanitized reports and hashes, the Git commit, and session information can be saved to an untracked RDS artifact. Keep machine-specific artifacts outside the repository, for example:

```sh
Rscript -e "dir.create('../redcapmissing-benchmarks', showWarnings = FALSE); Sys.setenv(REDCAPMISSING_BENCH_TIER = 'high-cardinality', REDCAPMISSING_BENCH_SCENARIO = 'U2,U10', REDCAPMISSING_BENCH_OUTPUT = '../redcapmissing-benchmarks/phase1-baseline.rds'); source('tools/benchmark-plan-run.R')"
```

Compare a later run with that artifact by setting `REDCAPMISSING_BENCH_BASELINE` to its path. The comparison requires the same workload descriptor and an `identical()` sanitized report after replacing only `diagnostics$elapsed_seconds` with typed zeroes; all other report values, types, classes, attributes, and ordering remain part of the comparison.

Complete the built-package gate:

```sh
Rscript -e "devtools::check()"
```
