# redcapmissing 7.0.0

## 2026-09-04

- Added `export_data_quality()` to retrieve complete Data Resolution Workflow
  history through Vanderbilt's Data Quality API module, optionally restricted
  by record IDs, and return a table ready for `run_plan(verified = ...)`.
- `run_plan()` now accepts native classic event IDs and nonrepeating instance
  placeholders, silently ignores evidence outside the plan and entries without
  reviewers, and prevents a missing latest status from reviving an older
  verification. Input/reviewer audit counts cover supplied history; latest and
  applied evidence counts cover the plan.
- Documented the export-to-assessment workflow in help and the README, with a
  runnable synthetic before/after example in the main vignette.

## 2026-09-01

- Added REDCap `year(date)` support to branching-logic evaluation, including
  cross-event field references and valid YMD, MDY, and DMY date/datetime
  values. Blank or invalid dates leave the branch unsatisfied.
- Added `build_explicit_schedule()` to discover a project's record-ID field,
  validate filtered mapping specifications against REDCap structure, and cross
  them with unique cohort IDs into the four-column schedule consumed by
  `plan_explicit()`. Independently built cohort schedules compose with
  `dplyr::bind_rows()`.
- Removed the separate `instruments` argument from `plan_explicit()` without a
  compatibility path. The explicit schedule is now the complete assessment
  declaration: the plan derives unique instruments in normalized schedule
  first-appearance order, and a zero-row schedule produces `character()`
  instrument scope.
- Fixed `all_instruments()` and project snapshot construction so surrounding
  whitespace in optional instrument, event, and arm display labels is trimmed,
  blank labels use existing fallbacks, and strict raw structural identifiers
  remain unchanged.

## 2026-08-12

- Set the package startup banner's release label to `run_plan() workflow` for
  the 7.0.0 release.

## 2026-08-08

- Improved `run_plan()` performance for high-cardinality plans by reusing
  target groups, branch parsing and evaluation results, response masks, and
  metadata lookups; limiting per-instrument branch dependencies; projecting
  validated runtime data; and materializing field outcomes in one compact pass.
  Paired synthetic 100-record by 600-instrument benchmarks showed lower median
  runtime overall with no reproducible regression across core blank, shared,
  and instrument-specific branching workloads while producing identical
  reports.

## 2026-07-27

- Standardized `assessible_targets` as the only name for the plan target table
  across code, diagnostics, documentation, and errors. Field selection and
  branching now use the clearer reasons `"no fields remain after field policy"`
  and `"no fields apply after branching logic"`.
- Added `all_instruments()` for the ordered REDCap instrument inventory and
  `build_extended_schedule()` for deterministic three-column extensions over
  every allowable crossing of a requested instrument subset. Classic projects
  retain native eventless crossings; wholly undesignated longitudinal
  instruments warn and contribute no extension row.
- Scoped `run_plan()` response-column preflight to instruments represented by
  frozen `assessible_targets`, plus checkbox and branching dependencies.
  Selected instruments with no targets no longer require their exclusive
  response fields at runtime.
- Limited the documented and validated `rcon` contract to redcapAPI classes
  `redcapApiConnection` and `redcapOfflineConnection`.
- Reduced `registry()` to the five columns used by package outputs and
  presentation: `validation_order`, `validation_level`, `validation_check`,
  `flex_label`, and `description`. Each `description` is the concise pass
  condition for an assessed check and drives the printed `condition` column.
- Added a credential-free synthetic offline plan-to-report example and corrected
  the documented accessor attribute name to `redcapmissing_labels`.
- Made verification evidence fail closed for classic projects. Every
  `verified$event_id` must be missing or blank; a nonmissing value now errors
  before username or status filtering and cannot authorize an override.

## 2026-07-25

- Replaced `find_missing()` with a plan and run workflow. `plan_from_data()`
  stores observed crossings plus `extended_schedule` crossings in
  `assessible_targets`; `plan_explicit()` uses `explicit_schedule`; `run_plan()`
  evaluates the resulting plan.
- Removed the `forms`, `events`, `records`, `instances`, and `ignore_ids` scope system.
  Removed the compatibility wrapper. Migration requires `plan_from_data()`,
  `plan_explicit()`, and `run_plan()`. Reports, accessors, registry values, and formatters
  use `instrument`, including `flex_event_instruments()`.
- Added SHA-256 project structure fingerprints. Record, event, repeat instrument, and repeat
  instance identifiers are validated and converted to the storage documented by the plan
  functions. Plans contain project identity, selected instruments, targets, and the
  fingerprint. Source records and live REDCap connections remain outside plans.
- Added `event-row-started`, `repeat-instance-row-started`, `instrument-started`, and
  `field-complete`. `required_fields`, `exclude_types`, and `ignore_fields` affect
  `field-complete`. A target with no fields remaining after policy receives status `"not applicable"`.
- Replaced the previous data quality history input with the nine required `verified`
  columns and latest timestamp selection. An exact `"VERIFIED"` row can change an assessed
  failed `field-complete` result, and the report stores verification counts.
- Changed reports to store `plan`, `target_results`, `summary`, `missing`, `verification`,
  `diagnostics`, and `details`. With `details = TRUE`, `details$value_summary` stores
  assessed ordinary field values as character and selected checkbox child column names.
- Added an error for ambiguous cross event branching when a referenced event has multiple
  source rows with positive repeat instances and no source row with a missing repeat
  instance. A unique source row with a missing repeat instance takes precedence; one
  source row with a positive repeat instance can supply the value.

# redcapmissing 6.1.2

## 2026-07-25

- Required verified data quality exceptions to include `status_id`, `res_id`, and `ts`. Each
  issue uses its latest resolution by timestamp and numeric resolution ID tie break. The
  resolution username, resolution `current_query_status`, and issue `query_status` must
  match the requested verifier and `"VERIFIED"`. Historical resolutions are ineligible.
- Accepted an entirely missing `instance` column with any R storage type. A classic project
  export may supply one positive internal `event_id`; it becomes the blank classic event
  context. Malformed and conflicting internal event IDs raise errors.

# redcapmissing 6.1.1

## 2026-07-24

- Fixed `find_missing()` validation of `redcapAPI::exportDataQuality()` when the repeat
  instrument and repeat instance dimensions do not apply. An entirely missing
  `repeat_instrument` column accepts any R storage type. `instance` placeholders are
  ignored when REDCap project structure shows that neither the event nor instrument
  repeats.

# redcapmissing 6.1.0

## 2026-07-24

- Added optional `verified` and `verified_user` inputs to `find_missing()`. An exact
  `"VERIFIED"` REDCap data quality row changes a matching failed `field-complete` result
  after eligibility, startedness, branching logic, field selection, and requested scope have
  been resolved.
- Added `diagnostics$verification` counts for input, matching username, verified, and
  applied rows. Supplied data quality rows remain outside the report.

# redcapmissing 6.0.3

## 2026-07-24

- Kept requested forms in `find_missing()` reports when `required_fields`, `exclude_types`,
  and `ignore_fields` leave zero fields. These forms retain `form-started` results and add
  zero assessed `field-complete` summaries for each context that reaches field assessment.

# redcapmissing 6.0.2

## 2026-07-22

- Added `diagnostics$stage_timings` and `diagnostics$form_workload` for processing time and
  field workload by report and form.
- Clarified that `events` can request assessment of an event with zero exported rows.

# redcapmissing 6.0.1

## 2026-07-22

- Updated `flex_event_forms()` so each form row displays `Form Incomplete` as `N/D (%)`.

# redcapmissing 6.0.0

## 2026-07-22

- Added `get_summary()` as the summary accessor and gave `get_missing()` stable schemas with
  event, repeat instrument, and repeat instance context. Both accessors validate
  `validation_check`, `events`, and `forms` with exact letter case.
- Added `flexify()` to map each input column from a full accessor result or compatible
  subset to one flextable column. It applies event and form labels and removes the two
  repeat columns when both are blank.
- Removed `tidy()`, `tidy.redcapmissing()`, `flex()`, the `flex.redcapmissing` S3
  registration, and the `generics` dependency. Use `get_summary()` for summaries and
  `flexify()` for tables from `get_summary()` or `get_missing()`.

# redcapmissing 5.2.1

## 2026-07-22

- Aligned help, README, and vignette descriptions with record eligibility, empty assessment,
  `flex()` filter intersection, exact context denominators, and `missing_threshold`
  headings.

# redcapmissing 5.2.0

## 2026-07-20

- Added `Form Not Started` and a configurable `Form >10% Missing` column to
  `flex_event_forms()`. Both report exact record, event, form, and repeat opportunities as
  N/D (%). `missing_threshold` changes the threshold.
- At `missing_threshold = 1`, the column is labeled `Form = 100% Missing` and counts
  contexts with 100% effective missingness. Results use each record context after branching
  logic.
- Added validation of the report values required by these metrics. Reports created before
  redcapmissing 5.2.0 require regeneration with `find_missing()`.
- Replaced the Ember Tag startup message with a branch spectrum radar mark showing the
  package name, installed version, and release name across multiple lines.

# redcapmissing 5.1.0

## 2026-07-20

- Added `get_missing()` as the focused accessor for failed validation rows. It returns nine
  columns, accepts one or more `validation_check` values, and preserves report row order.

# redcapmissing 5.0.2

## 2026-07-18

- Updated `report$missing` to replace redundant `validation_label` and `value_summary`
  columns with a final `url` column that links failed record and form contexts to REDCap
  Data Entry when connection metadata is available.

# redcapmissing 5.0.1

## 2026-07-18

- Redesigned `find_missing(progress = TRUE)` as one CLI status line using the Native Cool
  palette. It displays completed, active, and pending forms plus form and total processing
  percentages.

# redcapmissing 5.0.0

## 2026-07-10

- Breaking change: removed complete rollup validation checks and the validation check type
  column from registry, summary, missing, tidy, flex, README, vignette, and generated help.
- Added `find_missing(records = ...)` eligibility for event, event and form, and event,
  form, and repeat instance record sets.
- Replaced the event record override slot with `spec$record_eligibility`, which stores every
  assessed record, event, form, and repeat instance context with its eligibility source.
- Added `spec$unused_record_specs` and one `Unused records spec.` warning for valid
  `records` entries that remain unused after form, event, and instance resolution.
- Required every nested `records` value to contain record IDs. `NULL`, empty, missing,
  whitespace, and overlap with `ignore_ids` raise errors.
- Changed `Form Incomplete` to count unique failed record contexts from row presence,
  `form-started`, and `field-complete` failures. Multiple missing fields in one record,
  form, and repeat context count once.

# redcapmissing 4.0.2

## 2026-07-08

- Fixed `flex_event_forms()` so a started form context with zero assessed `form-complete`
  rows is excluded from `Form Incomplete`. Unstarted forms and failed `form-complete`
  contexts contribute to `Form Incomplete`.
- Changed `Form Incomplete` percentages to use assessed N from the exact row context.
- Included failed `event-row-started` contexts in the `Form Incomplete` numerator shown on
  form rows.
- Added an `All` row for aggregate incomplete and assessed form opportunities. Failed
  `instance-row-started` contexts contribute to the numerator on repeat form rows.
- Changed form row denominators to use the exact event, form, and repeat context.
- Required valid `event-row-started` or `instance-row-started` denominators for displayed
  longitudinal and repeat form rows. A `Single event` row uses positive `Total N`.
- Added an error when filtering leaves zero assessable records, except when explicit
  `records` entries create expected row presence assessments.
- Regenerated the vignette HTML as UTF-8 to remove encoding artifacts.

# redcapmissing 4.0.1

## 2026-07-08

- Updated `flex_event_forms()` to show `N (started/due)` counts with percentages, renamed
  `Form Complete` to `Form Incomplete`, removed `Fields Missing`, and retained form rows
  under events with zero started rows.

# redcapmissing 4.0.0

## 2026-07-08

- Removed the `pointblank` dependency and moved `find_missing()` validation into the
  package.
- Breaking change: reports contain `summary`, `missing`, `spec`, `diagnostics`, and
  `details`. `report$details` stores full validation rows when `details = TRUE`.
- Renamed identifiers for failed rows to `validation_step` and `validation_row_id`.
- Added `progress` output through `cli::cat_line()` with form and total processing
  percentages.
- Default reports exclude full validation tables. They retain failed row identifiers, the
  empty `report$missing` schema, and blank context strings for inapplicable REDCap system
  columns.
- Restricted branching logic validation to assessed fields.

# redcapmissing 3.2.6

## 2026-07-08

- Updated the package startup banner to a single line Ember Tag style:
  `> redcapmissing {v#.#.#} ~ eye-spy`.

# redcapmissing 3.2.5

## 2026-07-07

- Fixed `flex_event_forms()` event headers so `event-row-started` counts come from the
  validation summaries returned by `tidy()` and `flex()`.
- Removed the body `Total N` row, moved total N into the N column label, displayed event and
  repeat N as passed and assessed, and labeled `field-complete` failures `Fields Missing`.
- Added an error when duplicate `event-row-started` summaries for one event disagree on
  passed or assessed counts.

# redcapmissing 3.2.4

## 2026-07-07

- Clarified `find_missing(records = ...)` documentation: omitted events remain in
  assessment, `events` can exclude them, and `event-row-started` determines whether each
  record has an exported event row.

# redcapmissing 3.2.3

## 2026-07-07

- Fixed `flex_event_forms()` output for projects whose REDCap record ID field has a custom
  name. Total N, event N, repeat instance N, form rows, and HTML output now populate.
- Expanded `flex()` and `flex_event_forms()` help with reporting rules, filter behavior, and
  workflow examples.

# redcapmissing 3.2.2

## 2026-07-07

- Added `flex_event_forms()` for reduced event and form flextables with total record N,
  `event-row-started` N, `form-complete` counts, and `field-complete` failures under each
  event.

# redcapmissing 3.2.1

## 2026-07-07

- Excluded downstream validation rows when all applicable event or repeat rows fail their
  row presence checks, preventing rows with denominator zero and blank `redcap_event_name`.

# redcapmissing 3.2.0

## 2026-07-06

- Added `validation_check` to `flex()` for filtering formatted reports by values from
  `tidy()`, such as `"field-complete"`.
- Applied `events`, `forms`, and `validation_check` filters by intersection after validation
  against values in `tidy(x)`.
- Fixed REDCap branching logic parsing across `stringr` versions for compound field
  references, multiple references within one row, and references qualified by event.

# redcapmissing 3.1.3

## 2026-07-06

- Updated validation check descriptions in `registry()`, README, help, and vignettes:
  `form-complete` is "all form fields complete", `field-complete` is "field complete", and
  `event-complete` is "all forms on event complete". `event-complete` continues to summarize
  `on-route` checks.
- Updated the README workflow to show `tidy(report)` summaries before `report$missing`,
  followed by the failed rows.

# redcapmissing 3.1.2

## 2026-07-06

- Redesigned the README validation flow diagram with plain language labels and a simpler
  `event-complete` flow.

# redcapmissing 3.1.1

## 2026-07-06

- Fixed downstream summaries when every expected event or repeat row fails its row presence
  check. The result excludes blank event contexts from passing `form-started` or
  `event-complete` rows.

# redcapmissing 3.1.0

## 2026-07-06

- Added `records` to `find_missing()` for record eligibility by event. Each populated list
  entry sets the record IDs for its named event; omitted and empty entries use denominators
  derived from the export.
- Added `report$eligible_records` to store the event to ID overrides used during validation.

# redcapmissing 3.0.1

## 2026-07-06

- Simplified the README validation flow diagram so the validation levels read left to right
  and multiple event and form contexts visibly contribute to the shared event level detour
  summary.

# redcapmissing 3.0.0

## 2026-07-06

- Changed `tidy()` to return a raw validation summary with context columns first, omitted
  `form_label`, and included repeat context columns when repeat contexts exist.
- Changed `flex()` to display labeled event, form, repeat context, validation check, and
  pass and failure counts. Repeat columns appear when the selected rows contain repeat
  contexts.
- Added raw `events` and `forms` filters to `flex()` that apply before display labels.

# redcapmissing 2.0.1

## 2026-07-06

- Clarified GitHub installation and made the opening README example run during rendering.
- Moved optional reporting helpers out of the minimal workflow and centered the package logo
  clear of the header.

# redcapmissing 2.0.0

## 2026-07-06

- Added the event level `event-complete` detour validation check, which reports whether
  every `on-route` check passed for a requested record and event context.
- Updated `registry()` to show one row per validation check, display
  `event:form / event:form:instance` as one registry level, and print concise descriptions.
- Replaced `validation_level` values `row`, `form`, and `field` with `event:form`,
  `event:form:instance`, and `event`.
- Added `event_complete_checks` and `event_complete_failures` report components and
  documented the event summary in tidy, flex, README, vignette, and roxygen topics.

# redcapmissing 1.0.3

## 2026-07-06

- Clarified the README validation flow diagram by displaying `event-row-started` and
  `instance-row-started` as sibling row context checks.

# redcapmissing 1.0.2

## 2026-07-06

- Revised the README validation flow diagram to show `detour` checks as reporting offshoots.
- Labeled the main `on-route` transitions as paths followed after a pass.

# redcapmissing 1.0.1

## 2026-07-06

- Moved validation metadata columns in `tidy()` and `flex()` after the REDCap event and
  repeat system columns so context columns appear first.

# redcapmissing 1.0.0

## 2026-07-02

- Reorganized report rows around `validation_level`, `validation_check_type`,
  `validation_check`, and `validation_passed`.
- Added `registry()` as the validation check registry, with a classed tibble return value
  and grouped `cli` print method.
- Renamed validation checks to these exact values: `event-row-started`,
  `instance-row-started`, `form-started`, `form-complete`, and `field-complete`.
- Removed former validation scope names and report components such as `event_row_exists_*`,
  `repeat_instance_row_exists_*`, and `fields_complete_*`.
- Applied downstream gating to failed `on-route` checks. A failed `form-complete` check
  remains a `detour`, and field assessment continues.
- Updated README, vignette, roxygen documentation, generated help, and tests for the 1.0.0
  validation check names.

# redcapmissing 0.9.0

## 2026-07-02

- Changed the required `find_missing()` argument from `form` to `forms` and supported one
  report across multiple requested REDCap forms.
- Added `events` and `instances` lists for each form, including partial named lists, scalar
  repeat instance counts, exact repeat instance vectors, and one warning when an omitted
  repeating form uses instance 1.
- Added an explicit form column to validation rows, summaries, tidy output, and formatted
  output for combined reports.

# redcapmissing 0.8.0

## 2026-07-01

- Updated report summaries to use positive validation terminology for event rows, repeat
  instance rows, form startedness, form completeness, and field completeness.
- Changed `tidy.redcapmissing()` to return the focused public summary columns: form
  metadata, validation label, REDCap event and repeat context, assessed, passed, failed,
  pass rate, and fail rate.
- Added strict instrument label validation through `rcon$instruments()` so tidy summaries
  can include REDCap form labels.
- Renamed report helper tables to positive validation check and failure components.

# redcapmissing 0.7.1

## 2026-07-01

- Replaced the startup banner with a quieter three line console splash that shows the
  package name, release name, version, and release update.

# redcapmissing 0.7.0

## 2026-07-01

- Replaced `summary.redcapmissing()` with `tidy.redcapmissing()` and exported `tidy()` for
  focused validation summary tibbles.
- Removed support for `summary(report)` on a `redcapmissing` object.

# redcapmissing 0.6.1

## 2026-07-01

- Added a suppressible startup banner that reports the installed package version and current
  release metadata.

# redcapmissing 0.6.0

## 2026-07-01

- Renamed the `find_missing()` selected event and repeat instance arguments to `events` and
  `instances`, and aligned the corresponding returned report fields with those names.

# redcapmissing 0.5.1

## 2026-07-01

- Added direct `flex()` support for `"summary.redcapmissing"` objects returned by
  `summary(report)`, with expanded generated documentation for accepted inputs and optional
  reporting package requirements.

# redcapmissing 0.5.0

## 2026-06-30

- Grouped `find_missing()` validation summaries by event and repeat instance so `n`,
  `n_passed`, `n_failed`, `f_passed`, and `f_failed` describe each exact context for
  instruments used across events or repeating structures.
- Added an `any_field_missing` summary that reports whether an evaluable record context has
  at least one missing expected field. The existing field summary remains available.
- Added context columns to `report$agent$validation_set` and displayed that context beside
  assessed, passed, and failed counts in `flex()`.

# redcapmissing 0.4.0

## 2026-06-30

- Removed the `redcap_missing_report()` compatibility function. `find_missing()` became the
  sole exported report creation function.

# redcapmissing 0.3.0

## 2026-06-30

- Renamed the primary report creation function to `find_missing()`.
- Deprecated `redcap_missing_report()` while keeping it as an exported compatibility wrapper
  for this release.
- Moved `flex()` and `flex_html()` into their own source files for clearer package
  structure.

# redcapmissing 0.2.0

## 2026-06-30

- Changed the report return object to use the `"redcapmissing"` S3 class.
- Added `summary.redcapmissing()` so `summary(report)` returns the unmodified validation
  summary tibble.
- Added `flex()` and `flex_html()` for flextable and HTML summary output.
- Removed the previous `redcap_missing_summary()` helper.

# redcapmissing 0.1.12

## 2026-06-11

- Shortened the README opening to describe `redcapmissing` as an R package for working with
  REDCap.

# redcapmissing 0.1.11

## 2026-06-11

- Centered the inverted tree motif in the package logo and retained its background palette.
- Updated the README opening to describe `redcapmissing` as an R package for REDCap record
  exports.

# redcapmissing 0.1.10

## 2026-06-11

- Restored the README acknowledgement structure for dependency references.

# redcapmissing 0.1.9

## 2026-06-11

- Completed `.Rbuildignore` and `.gitignore` with additional package and local workspace
  exclusions.
- Restored concise README source wording in `README.Rmd` so the generated `README.md`
  preserves the intended public package language.

# redcapmissing 0.1.8

## 2026-06-10

- Added README and vignette guidance for `redcapAPI::exportRecordsTyped()`, including its
  cast arguments for missingness reports.

# redcapmissing 0.1.7

## 2026-06-09

- Moved the README logo beside the package overview so it clears the title rule.
- Rendered the vignette HTML beside its source `.Rmd` file for repository browsing.

# redcapmissing 0.1.6

## 2026-06-09

- Fixed repeat instance logic to use the repeat status at each requested event when an
  instrument has no repeat at some events and repeats at others.
- Added tests for those mixed structures, including `desired_events` subsets and default
  repeat instance behavior.
- Expanded vignette and README guidance for instruments whose repeat status differs by
  event.

# redcapmissing 0.1.5

## 2026-06-09

- Added a `desired_events` argument to `redcap_missing_report()` for subsetting multiple
  event form assessment to selected REDCap events.
- Added tests and public documentation for selected event assessment behavior.

# redcapmissing 0.1.4

## 2026-06-09

- Expanded README and vignette guidance for the `redcap_missing_report()` return object.
- Documented `report$missing`, the helper tables for each scope, and
  `redcap_missing_summary()`.

# redcapmissing 0.1.3

## 2026-06-09

- Expanded README and vignette documentation for the four missingness scopes and expected
  repeat context behavior.
- Documented the origins of redcapAPI and its current VUMC Biostatistics stewardship.
- Updated package documentation links and citations for current redcapAPI resources.

# redcapmissing 0.1.2

## 2026-06-05

- Declared and documented the package's reliance on `redcapAPI` for REDCap connections,
  metadata, and typed exports.
- Updated package, README, and vignette documentation to describe `rcon` as a
  `redcapAPI::redcapConnection()` workflow input.
- Aligned internal blank value handling with `redcapAPI::isNAorBlank()`.

# redcapmissing 0.1.1

## 2026-06-05

- Set package author metadata in `DESCRIPTION`.
- Added the vignette, README source, package help, and package logo.
- Updated build exclusions and release preparation files.

# redcapmissing 0.1.0

## 2026-06-05

- Created the initial package structure for REDCap missingness reports that apply branching
  logic.
- Added package documentation, a vignette, and a public README.
- Added field, event, repeat instance, and whole form missingness results.
