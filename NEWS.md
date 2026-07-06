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
- Updated validation rows, pointblank summaries, tidy output, and formatted
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
  `pointblank` validation-set tibble.
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

- Restored the README acknowledgement structure for both `redcapAPI` and `pointblank`, including a dedicated `pointblank` references section in the source `README.Rmd`.

# redcapmissing 0.1.9

## 2026-06-11

- Completed `.Rbuildignore` and `.gitignore` with additional package and local-workspace exclusions.
- Restored concise README source wording in `README.Rmd`, including direct acknowledgment of `pointblank`, so the generated `README.md` preserves the intended public package language.

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
