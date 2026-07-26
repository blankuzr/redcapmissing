# redcapmissing 7.0.0

## 2026-07-25

- Replaced `find_missing()` with a plan-and-run workflow. Use
  `plan_from_data()` for Assessible targets observed in an export plus sparse
  `extended_schedule` additions, or `plan_explicit()` for a complete
  record-level `explicit_schedule`; then evaluate the frozen plan with
  `run_plan()`.
- Removed the former `forms`, `events`, `records`, `instances`, and
  `ignore_ids` scope system without a compatibility wrapper. Public report,
  accessor, registry, and formatter vocabulary now uses `instrument`
  consistently, including `flex_event_instruments()`.
- Added deterministic project-structure fingerprints and strict shared
  normalization for record, event, repeating-instrument, and repeat-instance
  identifiers. Plans retain neither source records nor live REDCap
  connections, and execution rejects malformed or structurally stale plans.
- Separated physical-row, repeat-row, instrument-started, and field-complete
  checks. Field policy now affects only field completeness, and a target with
  no assessible fields is reported as not applicable rather than complete.
- Replaced the prior data-quality history interface with an exact nine-column
  verification contract, latest-timestamp selection, effective-failure
  overrides, and explicit audit counts.
- Changed the report object to store its plan, target results, summary,
  unresolved missing rows, verification audit, fixed-stage diagnostics, and
  optional detailed results.
- Reworked plan construction and execution around native structural joins,
  integer target identities, vector field kernels, compiled branching logic,
  batched verification, and grouped report aggregation. Compact execution now
  retains failed field rows rather than materializing every passing field while
  preserving the same targets, summaries, missing rows, and audit counts.
- Defined fail-closed cross-event branching when a referenced event contains
  repeated rows: a unique regular source row takes precedence, a sole repeated
  source row is usable, and multiple repeated-only source rows error rather
  than selecting an arbitrary instance.

# redcapmissing 6.1.2

## 2026-07-25

- Hardened verified data-quality exceptions by requiring the exported
  `status_id`, `res_id`, and `ts`, collapsing each issue to its latest
  resolution by timestamp with a numeric resolution-ID tie-break, and requiring
  that latest resolution's username and `current_query_status` to match the
  requested verifier and `"VERIFIED"`. The live issue `query_status` must also
  remain exactly `"VERIFIED"`; historical resolutions never authorize an
  exception.
- Accepted entirely missing `instance` columns regardless of their R storage
  type. Classic-project exports may now supply one canonical positive internal
  `event_id`, which is normalized to the package's blank classic event context;
  malformed or conflicting internal event IDs remain errors.

# redcapmissing 6.1.1

## 2026-07-24

- Fixed `find_missing()` validation of `redcapAPI::exportDataQuality()` results
  for regular, non-repeating contexts. An entirely missing
  `repeat_instrument` column is accepted regardless of its R storage type, and
  upstream `instance` placeholders are ignored when the cached REDCap structure
  confirms that the field is not in a repeating instrument or event.

# redcapmissing 6.1.0

## 2026-07-24

- Added optional, fail-closed `verified` and `verified_user` inputs to
  `find_missing()`. Validated REDCap data-quality rows with exact status
  `"VERIFIED"` can now convert only the matching, otherwise-failing
  field-complete checks to passes without bypassing upstream eligibility,
  startedness, branching, field-selection, or requested-scope rules.
- Added `diagnostics$verification` counts for the supplied, user-matched,
  verified, and applied exception rows without retaining the supplied
  data-quality data.

# redcapmissing 6.0.3

## 2026-07-24

- Kept valid requested forms in `find_missing()` reports when required-field,
  type-exclusion, and ignored-field filtering leaves no fields to assess.
  These forms retain form-started results and now contribute explicit
  zero-assessed field-complete summaries for each event/repeat context that
  reaches field assessment, while upstream-gated contexts remain absent.

# redcapmissing 6.0.2

## 2026-07-22

- Removed field-count-dependent CLI progress overhead from `find_missing()` by
  reusing one formatting theme per run, including constellation symbols and
  styles, and skipping intermediate line assembly
  in terminals that only display the final completion line. The progress-aware
  benchmark now runs the 150-record, 10-form workload near the same speed as
  `progress = FALSE` while preserving the existing progress display.
- Added `REDCAPMISSING_BENCH_PROGRESS` to the package benchmark script so
  progress-enabled and quiet runs can be measured explicitly.
- Reworked `find_missing()` field evaluation around one report-level compiled
  plan, row-indexed record views, and shared per-form blankness matrices.
  Ordinary non-branched fields are evaluated as a vectorized block, while
  branching fields and checkboxes use specialized paths only where needed.
- Added `diagnostics$stage_timings` and `diagnostics$form_workload` so report
  and per-form processing stages can be compared alongside the amount and mix
  of field work performed.
- Expanded synthetic performance coverage to include unequal form sizes,
  branching logic, checkboxes, omitted event rows, and repeating forms while
  retaining regression evidence that each REDCap connection surface is queried
  at most once per report.
- Clarified that `events` can request assessment of events REDCap omitted from
  an export because no row was started.

# redcapmissing 6.0.1

## 2026-07-22

- Updated `flex_event_forms()` so form-row `Form Incomplete` values display
  `N/D (%)`, matching the adjacent form metrics.
- Reduced `find_missing()` runtime and allocation overhead by assembling field
  plans and validation rows in bulk instead of constructing and name-repairing
  small tibbles for every field. REDCap project-context queries remain cached
  once per report rather than multiplying across requested forms.

# redcapmissing 6.0.0

## 2026-07-22

- Added `get_summary()` as the primary summary accessor and expanded
  `get_missing()` to stable schemas that always include event, repeat
  instrument, and repeat instance context. Both accessors now share strict,
  case-sensitive `validation_check`, `events`, and `forms` filtering.
- Added `flexify()` for formatting full accessor results or compatible column
  subsets as one-to-one flextable columns, including label-aware event/form
  display and visual suppression of an entirely blank repeat-column pair.
- Removed `tidy()`, `tidy.redcapmissing()`, `flex()`, the
  `flex.redcapmissing` S3 registration, and the `generics` dependency. Use
  `get_summary()` for report summaries and pass `get_summary()` or
  `get_missing()` results to `flexify()` for flextable output.

# redcapmissing 5.2.1

## 2026-07-22

- Aligned current help, README, and vignette documentation with the existing
  record-eligibility, empty-assessment, `flex()` filter-intersection,
  exact-context denominator, and missing-threshold heading contracts.

# redcapmissing 5.2.0

## 2026-07-20

- Added `Form Not Started` and a configurable dynamic
  `Form >10% Missing` column to `flex_event_forms()`. Both report exact
  record/event/form/repeat opportunities as N/D (%), and the threshold can be
  changed with `missing_threshold`. At a threshold of 1, the column is labeled
  `Form = 100% Missing` and counts contexts with 100% effective missingness.
  Threshold results use each record's final branching-aware field assessment.
- `flex_event_forms()` now validates the report context required by these
  metrics. Reports created before `redcapmissing` 5.2.0 must be regenerated
  with `find_missing()` before using the expanded table.
- Replaced the single-line Ember Tag startup message with a multi-line
  branch-spectrum radar mark that displays only the package name, installed
  version, and release name.

# redcapmissing 5.1.0

## 2026-07-20

- Added `get_missing()` as the recommended focused accessor for failed
  validation rows. It returns nine stable user-facing columns and can filter
  by one or more canonical validation checks while preserving report row
  order.

# redcapmissing 5.0.2

## 2026-07-18

- Updated `report$missing` to replace redundant `validation_label` and
  `value_summary` columns with a final `url` column that links failed
  record/form contexts to REDCap Data Entry when connection metadata is
  available.

# redcapmissing 5.0.1

## 2026-07-18

- Redesigned `find_missing(progress = TRUE)` as a single in-place,
  color-and-symbol CLI status line using the Native Cool palette. It shows
  completed, active, and pending forms alongside current-form and overall
  processing percentages instead of printing every update on a new line.

# redcapmissing 5.0.0

## 2026-07-10

- Breaking change: removed complete-rollup validation checks and the
  validation-check type column from registry, summary, missing, tidy, flex,
  README, vignette, and generated help surfaces.
- Added context-level `find_missing(records = ...)` eligibility for event,
  event/form, and event/form/repeat-instance record sets.
- Replaced the previous event-record override slot with
  `spec$record_eligibility`, a complete table of assessed
  record/event/form/repeat-instance contexts and their eligibility source, even
  when `records` is omitted.
- Added `spec$unused_record_specs` and a single `Unused records spec.` warning
  for valid `records` entries that are not used after form, event, and instance
  resolution.
- `records` values are now strict: `NULL`, empty, missing, and blank-only IDs
  error anywhere in the nested records specification. `ignore_ids` overlapping
  IDs listed in `records` also errors and lists the conflicting IDs.
- Updated `flex_event_forms()` so `Form Incomplete` counts unique failed record
  contexts from row-started, form-started, and field-complete failures; multiple
  missing fields in one record/form/repeat context count once.

# redcapmissing 4.0.2

## 2026-07-08

- Fixed `flex_event_forms()` so started form contexts with no assessed
  `form-complete` summary row are not reported as incomplete. Unstarted forms
  and failed `form-complete` contexts still contribute to `Form Incomplete`.
- Updated `Form Incomplete` percentages to use exact-context row-started
  assessed N as the denominator instead of the event-started passed N.
- Updated `flex_event_forms()` form-row `Form Incomplete` display counts so
  failed `event-row-started` contexts also contribute to the displayed numerator
  without changing the underlying validation summaries.
- Added an `All` summary row to `flex_event_forms()` that reports aggregate
  incomplete/assessed form opportunities, and aligned repeat form rows so
  failed `instance-row-started` contexts contribute to the displayed
  `Form Incomplete` numerator.
- Updated `flex_event_forms()` form-row `Form Incomplete` denominators to use
  the exact event/form/repeat context instead of reusing the event header N.
- Hardened `flex_event_forms()` so shown longitudinal and repeat form rows
  require valid exact `event-row-started` or `instance-row-started`
  denominators. Non-longitudinal `Single event` rows use positive `Total N` as
  the display-only denominator.
- Made `find_missing()` stop when no records remain assessable after filtering
  unless explicit `records` entries create expected row-started assessments.
- Regenerated the vignette HTML with UTF-8 output to remove encoded character
  artifacts.

# redcapmissing 4.0.1

## 2026-07-08

- Updated `flex_event_forms()` to show `N (started/due)` counts with
  percentages, report `Form Incomplete` instead of `Form Complete`, remove the
  `Fields Missing` column, and retain form rows under events with zero started
  rows.

# redcapmissing 4.0.0

## 2026-07-08

- Rebuilt `find_missing()` around a native validation engine and removed
  `pointblank` from the package contract and dependency surface.
- Breaking change: default reports now contain `summary`, `missing`, `spec`,
  `diagnostics`, and `details`; heavy row-level internals are available under
  `report$details` only when `details = TRUE`.
- Renamed failed-row identifiers from pointblank-specific names to
  `validation_step` and `validation_row_id`.
- Added `progress` output support using line-based `cli::cat_line()` form and
  overall processed percentages.
- Improved performance by caching REDCap project context, compiling branching
  logic once per field plan, caching event-qualified branch lookups, and
  replacing quadratic form/event rollups with grouped reductions.
- Tightened the compact default report path so full row-level validation tables
  are not retained unless `details = TRUE`, while preserving deterministic
  failed-row identifiers, empty `report$missing` schemas, and blank context
  strings for non-applicable REDCap system columns.
- Deferred branch compilation away from form-started presence checks so invalid
  branching logic on unassessed fields does not block the report.

# redcapmissing 3.2.6

## 2026-07-08

- Updated the package startup banner to a single-line Ember Tag style:
  `> redcapmissing {v#.#.#} ~ eye-spy`.

# redcapmissing 3.2.5

## 2026-07-07

- Fixed `flex_event_forms()` event headers so event-row-started counts are
  derived from the same tidy validation summaries shown by `tidy()` and
  `flex()`.
- Updated `flex_event_forms()` output to remove the body `Total N` row, show
  total N in the N column label, display event and repeat N as
  passed/assessed, and rename field-complete failures to `Fields Missing`.
- Added an informative error when duplicate `event-row-started` summaries for
  the same event disagree on passed or assessed counts.

# redcapmissing 3.2.4

## 2026-07-07

- Clarified `find_missing(records = ...)` documentation so omitted events are
  described as still checked unless excluded with `events`, with
  `event-row-started` determining whether each record has an exported event row.

# redcapmissing 3.2.3

## 2026-07-07

- Fixed `flex_event_forms()` so reports whose REDCap record ID field is not
  named `record_id` still populate total N, event N, repeat-instance N, form
  rows, and rendered HTML output.
- Expanded `flex()` and `flex_event_forms()` help pages with clearer reporting
  semantics, filter behavior, and workflow examples.

# redcapmissing 3.2.2

## 2026-07-07

- Added `flex_event_forms()` for reduced event/form flextable reports with
  total record N, event row-started N, form-complete counts, and
  field-complete failure counts nested under each event.

# redcapmissing 3.2.1

## 2026-07-07

- Fixed longitudinal validation summaries so downstream checks gated out by
  missing event or repeat rows no longer appear as zero-denominator rows with
  blank `redcap_event_name`.

# redcapmissing 3.2.0

## 2026-07-06

- Added a `validation_check` argument to `flex()` for filtering formatted
  reports by raw validation-check values from `tidy()`, such as
  `"field-complete"`.
- Standardized all `flex()` filters so `events`, `forms`, and
  `validation_check` validate against values present in `tidy(x)`, then apply
  by intersection.
- Fixed REDCap branching logic parsing for compound field references across
  `stringr` versions, including multi-reference same-row and event-qualified
  logic.

# redcapmissing 3.1.3

## 2026-07-06

- Updated validation-check wording in `registry()`, README, help, and vignettes:
  `form-complete` is "all form fields complete", `field-complete` is
  "field complete", and `event-complete` is "all forms on event complete".
  `event-complete` behavior is unchanged and continues to summarize only
  downstream-gating on-route checks.
- Updated the README minimal workflow to show `tidy(report)` summary rows
  before `report$missing`, so readers can see every validation check that ran
  before reviewing failed rows.

# redcapmissing 3.1.2

## 2026-07-06

- Redesigned the README validation-flow diagram with clearer plain-language
  labels, simplified event-complete flow, and a more polished visual style.

# redcapmissing 3.1.1

## 2026-07-06

- Fixed downstream validation summaries when all expected event or repeat rows
  fail their upstream row-started checks. `find_missing()` no longer falls back
  to blank-event record contexts that can appear as passing form-started or
  event-complete rows.

# redcapmissing 3.1.0

## 2026-07-06

- Added a `records` argument to `find_missing()` for event-specific record
  eligibility. Non-empty `records` list entries override the record IDs assessed
  for their named REDCap event, while omitted or empty events continue to use
  the existing data-derived denominators.
- Added `report$eligible_records` so reports expose the normalized event-to-ID
  overrides used during validation.

# redcapmissing 3.0.1

## 2026-07-06

- Simplified the README validation-flow diagram so the validation levels read
  left to right and multiple event/form contexts visibly contribute to the
  shared event-level detour summary.

# redcapmissing 3.0.0

## 2026-07-06

- Changed `tidy()` to return a raw context-first validation summary without
  the previous `form_label` column and to omit repeat-context columns when a
  report has no repeat contexts.
- Changed `flex()` to display the default reporting columns as labeled event,
  form, repeat context, validation check, and pass/fail counts, with repeat
  columns omitted whenever the displayed rows do not include repeat contexts.
- Added raw `events` and `forms` filters to `flex()` so formatted reports can
  be subset before display labels are applied.

# redcapmissing 2.0.1

## 2026-07-06

- Improved README onboarding by clarifying GitHub installation, making the
  first-use example executable during README rendering, moving optional
  reporting helpers out of the minimal workflow, and centering the package logo
  to avoid header overlap.

# redcapmissing 2.0.0

## 2026-07-06

- Added the event-level `event-complete` detour validation-check, which reports
  whether all on-route checks passed for each requested record/event context.
- Updated `registry()` to show one row per validation-check, with the
  contextual `event:form / event:form:instance` level displayed as one
  registry level, and a compact meaning-focused `cli` table instead of verbose
  grouped listings.
- Replaced public `validation_level` values `row`, `form`, and `field` with
  context levels: `event:form`, `event:form:instance`, and `event`.
- Added `event_complete_checks` and `event_complete_failures` report
  components and updated tidy, flex, README, vignette, and roxygen
  documentation for the event-level validation canon.

# redcapmissing 1.0.3

## 2026-07-06

- Clarified the README validation-flow diagram so `event-row-started` and
  `instance-row-started` are shown as sibling row-context gates instead of a
  sequential pass-through.

# redcapmissing 1.0.2

## 2026-07-06

- Revised the README validation-flow diagram so `detour` checks are shown as
  reporting offshoots instead of downstream-gating pipeline steps.
- Made the README validation-flow diagram more compact and labeled main
  `on-route` transitions as pass-only paths.

# redcapmissing 1.0.1

## 2026-07-06

- Moved validation metadata columns in `tidy()` and `flex()` output after the
  REDCap event and repeat system columns for easier context-first scanning.

# redcapmissing 1.0.0

## 2026-07-02

- Made the validation canon package-facing by reorganizing report rows around
  `validation_level`, `validation_check_type`, `validation_check`, and
  `validation_passed`.
- Added `registry()` as the public validation-check registry, with a classed
  tibble return value and a grouped `cli` print method.
- Renamed validation checks to the canonical hyphenated values:
  `event-row-started`, `instance-row-started`, `form-started`,
  `form-complete`, and `field-complete`.
- Removed old validation-scope names and report components such as
  `event_row_exists_*`, `repeat_instance_row_exists_*`, and
  `fields_complete_*`.
- Enforced strict downstream gating for failed `on-route` checks while keeping
  `form-complete` as a `detour` check that does not block field assessment.
- Updated README, vignette, roxygen documentation, generated docs, and tests for
  the 1.0.0 validation canon.

# redcapmissing 0.9.0

## 2026-07-02

- Changed `find_missing()` to use required `forms` instead of `form` and to
  support one combined report across multiple requested REDCap forms.
- Added form-specific `events` and `instances` list support, including partial
  named lists, scalar repeat-instance counts, exact repeat-instance vectors, and
  one warning when omitted repeating forms default to instance 1.
- Updated validation rows, validation summaries, tidy output, and formatted
  reporting to keep form as a first-class output column in combined reports.

# redcapmissing 0.8.0

## 2026-07-01

- Updated report summaries to use positive validation terminology for event
  rows, repeat-instance rows, form startedness, form completeness, and field
  completeness.
- Changed `tidy.redcapmissing()` to return the focused public summary columns:
  form metadata, validation label, REDCap event/repeat context, assessed,
  passed, failed, pass rate, and fail rate.
- Added strict instrument-label validation through `rcon$instruments()` so
  tidy summaries can include REDCap form labels.
- Renamed report helper tables to positive validation-check and failure
  components.

# redcapmissing 0.7.1

## 2026-07-01

- Replaced the startup banner with a quieter three-line console splash that
  shows the package name, release name, version, and release update.

# redcapmissing 0.7.0

## 2026-07-01

- Replaced the `summary.redcapmissing()` interface with a `tidy.redcapmissing()`
  method and re-exported `tidy()` for focused validation-summary tibbles. This
  is a breaking change: `summary(report)` is no longer a supported
  redcapmissing API.

# redcapmissing 0.6.1

## 2026-07-01

- Added a suppressible startup banner that reports the installed package
  version and current release metadata.

# redcapmissing 0.6.0

## 2026-07-01

- Renamed the `find_missing()` selected-event and repeat-instance arguments to
  `events` and `instances`, and aligned the corresponding returned report
  fields with those names.

# redcapmissing 0.5.1

## 2026-07-01

- Added direct `flex()` support for `"summary.redcapmissing"` objects returned
  by `summary(report)`, with expanded generated documentation for accepted
  inputs and optional reporting-package requirements.

# redcapmissing 0.5.0

## 2026-06-30

- Stratified `find_missing()` validation summaries by event and repeat-instance
  context so `n`, `n_passed`, `n_failed`, `f_passed`, and `f_failed` are
  clinically meaningful for multi-event forms and forms assessed in repeating
  events or as repeating instruments.
- Added an `any_field_missing` roll-up scope that reports whether an evaluable
  record context has at least one missing expected field, while preserving the
  existing granular field-level scope.
- Added context columns to `report$agent$validation_set` and updated `flex()`
  to display the validation context alongside assessed, passed, and failed
  counts.

# redcapmissing 0.4.0

## 2026-06-30

- Removed the `redcap_missing_report()` compatibility function. `find_missing()`
  is now the only exported report-building function.

# redcapmissing 0.3.0

## 2026-06-30

- Renamed the primary report-building function to `find_missing()`.
- Deprecated `redcap_missing_report()` while keeping it as an exported
  compatibility wrapper for this release.
- Moved `flex()` and `flex_html()` into their own source files for clearer
  package structure.

# redcapmissing 0.2.0

## 2026-06-30

- Refactored the report return object to use the `"redcapmissing"` S3 class.
- Added `summary.redcapmissing()` so `summary(report)` returns the unmodified
  validation-summary tibble.
- Added `flex()` and `flex_html()` for flextable and HTML summary output.
- Removed the previous `redcap_missing_summary()` helper.

# redcapmissing 0.1.12

## 2026-06-11

- Shortened the README opening language to describe `redcapmissing` as an R package for working with REDCap, without the extra record-export phrasing.

# redcapmissing 0.1.11

## 2026-06-11

- Refreshed the package logo with a cleaner centered upside-down tree motif while preserving the existing background palette.
- Updated the README opening language to clarify that `redcapmissing` is an R package for working with REDCap record exports.

# redcapmissing 0.1.10

## 2026-06-11

- Restored the README acknowledgement structure for dependency references.

# redcapmissing 0.1.9

## 2026-06-11

- Completed `.Rbuildignore` and `.gitignore` with additional package and local-workspace exclusions.
- Restored concise README source wording in `README.Rmd` so the generated
  `README.md` preserves the intended public package language.

# redcapmissing 0.1.8

## 2026-06-10

- Added clearer README and vignette guidance for the recommended `redcapAPI::exportRecordsTyped()` call, including an explicit cast pattern for missingness reports.

# redcapmissing 0.1.7

## 2026-06-09

- Adjusted README logo placement so the package icon renders cleanly beside the package overview instead of intersecting the title rule.
- Rendered the vignette HTML output alongside the source `.Rmd` file for easier repository browsing.

# redcapmissing 0.1.6

## 2026-06-09

- Fixed mixed repeat/non-repeat event handling so repeat-instance logic follows the requested events rather than the form's global repeat status elsewhere in the project.
- Added tests for forms that repeat on some REDCap events but not others, including `desired_events` subsets and default repeat-instance behavior.
- Expanded vignette and README guidance for forms that are regular on some events and repeating instruments on others.

# redcapmissing 0.1.5

## 2026-06-09

- Added a `desired_events` argument to `redcap_missing_report()` for subsetting multi-event form assessment to selected REDCap events.
- Added tests and public documentation for selected-event assessment behavior.

# redcapmissing 0.1.4

## 2026-06-09

- Expanded README and vignette guidance for the `redcap_missing_report()` return object.
- Added clearer public-facing documentation for `report$missing`, the scope-specific helper tables, and `redcap_missing_summary()`.

# redcapmissing 0.1.3

## 2026-06-09

- Expanded README and vignette documentation for the four missingness scopes and the expected-repeat-context model.
- Added public-facing documentation acknowledging both the original redcapAPI lineage and the current VUMC Biostatistics stewardship/resources.
- Updated package documentation links and citations for current redcapAPI stewardship.

# redcapmissing 0.1.2

## 2026-06-05

- Declared and documented the package's reliance on `redcapAPI` for REDCap connections, metadata, and typed exports.
- Updated package, README, and vignette documentation to describe `rcon` as a `redcapAPI::redcapConnection()` workflow input.
- Aligned internal blank-value handling with `redcapAPI::isNAorBlank()`.

# redcapmissing 0.1.1

## 2026-06-05

- Set final package author metadata for the public package surface.
- Added and polished public package documentation, including the vignette, README source, and package logo.
- Tightened build hygiene and package workflow notes for release preparation.

# redcapmissing 0.1.0

## 2026-06-05

- Initial public package structure for branching-aware REDCap missingness reports.
- Added package-style documentation, a vignette, and a public README.
- Added support for field-level, event-level, repeat-instance, and whole-form missingness reporting.
