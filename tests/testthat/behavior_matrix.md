# `redcapmissing` 7.0.0 behavior matrix

This index links package behavior to the test files and commands that check it.
Each row names the source of executable evidence.

## Exported functions, classes, and names

| ID | Behavior checked | Evidence |
|---|---|---|
| API-01 | `plan_from_data()`, `plan_explicit()`, and `run_plan()` have the documented arguments and defaults. | `test-assessment-plan.R`, `test-run-plan.R` |
| API-02 | Validation checks are `event-row-started`, `repeat-instance-row-started`, `instrument-started`, and `field-complete`. | `test-registry.R` |
| API-03 | Report columns, filters, labels, headings, and metrics use `instrument` or `instruments`. | `test-get-summary.R`, `test-get-missing.R`, `test-flexify.R`, `test-flex-event-instruments.R` |
| API-04 | `find_missing()` and `flex_event_forms()` are absent from the exports. | `test-registry.R`, `test-flex-event-instruments.R` |
| API-05 | Errors use the argument, schema, project, schedule, plan, and verification subclasses of `redcapmissing_error`. | `test-assessment-plan.R`, `test-run-plan.R`, `test-run-plan-verified.R` |
| API-06 | An extension into an arm with zero observed records uses `redcapmissing_warning_empty_arm_extension`. | `test-assessment-plan.R` |
| API-07 | `print(registry())` displays every validation check in full and returns its input invisibly with class unchanged. | `test-registry.R` |

## Plan objects and project structure

| ID | Behavior checked | Evidence |
|---|---|---|
| PLAN-01 | Both constructors return `redcapmissing_plan` with `schema_version`, `construction`, `instruments`, `assessible_targets`, `project`, and `structure_fingerprint`. | `test-assessment-plan.R` |
| PLAN-02 | `construction` is `"from_data"` or `"explicit"`. | `test-assessment-plan.R` |
| PLAN-03 | `assessible_targets` has the documented six columns and storage types. | `test-assessment-plan.R` |
| PLAN-04 | Targets are unique on their first five columns and have stable instrument, event, record, repeat kind, and instance order. | `test-assessment-plan.R` |
| PLAN-05 | `target_source` is `"observed"`, `"extended"`, `"observed+extended"`, or `"explicit"`. | `test-assessment-plan.R` |
| PLAN-06 | Plans contain project identity and labels and exclude source records and a live connection. | `test-assessment-plan.R` |
| PLAN-07 | Plan printing reports construction, instrument count, and target count while excluding record IDs. | `test-assessment-plan.R` |
| PLAN-08 | SHA-256 fingerprints are stable when source tables have different row order and change when project structure changes. | `test-assessment-plan.R` |
| PLAN-09 | Plan validation rejects changed classes, components, schema versions, targets, sources, project values, and fingerprints. | `test-assessment-plan.R` |
| PLAN-10 | Constructors retrieve each required connection surface once and make zero record export calls. | `test-assessment-plan.R` |
| PLAN-11 | Missing project structure, including repeat configuration, raises a project error. | `test-assessment-plan.R` |
| PLAN-12 | A `redcapAPI::offlineConnection()` can construct and run a classic project plan. | `test-redcapapi-integration.R` |

## Structural values

| ID | Behavior checked | Evidence |
|---|---|---|
| NORM-01 | Record IDs accept character, factor, integer, and finite double storage and become character while preserving character leading zeros. | `test-assessment-plan.R` |
| NORM-02 | Record IDs reject missing, blank, padded, `NaN`, infinite, and absent values. | `test-assessment-plan.R` |
| NORM-03 | Longitudinal rows require known raw event names. Classic event absence, blank values, and typed missing values become `NA_character_`. | `test-assessment-plan.R` |
| NORM-04 | A repeating design requires `redcap_repeat_instrument` and `redcap_repeat_instance` together. | `test-assessment-plan.R` |
| NORM-05 | Repeat instances accept positive integers, whole number doubles, and character digits matching `[1-9][0-9]*`, then become integer. | `test-assessment-plan.R` |
| NORM-06 | Repeat instances reject leading zeros, zero, negatives, decimals, required missing values, other text, `NaN`, infinity, integer overflow, and factors with values. | `test-assessment-plan.R` |
| NORM-07 | Missing structural values become typed `NA` where the dimension is inapplicable. | `test-assessment-plan.R` |
| NORM-08 | Rows where neither the event nor instrument repeats, rows at repeating events, rows for repeating instruments, and instruments whose repeat status differs by event must match the REDCap project structure. | `test-assessment-plan.R` |
| NORM-09 | Duplicate normalized physical keys raise an error. | `test-assessment-plan.R` |
| NORM-10 | Planning uses physical row presence and requires structural columns. | `test-assessment-plan.R` |

## `plan_from_data()`

| ID | Behavior checked | Evidence |
|---|---|---|
| DATA-01 | Targets are project permitted crossings found in observed rows or `extended_schedule`. | `test-assessment-plan.R` |
| DATA-02 | Omitted, `NULL`, and correctly typed extension data frames with zero rows produce observed targets. | `test-assessment-plan.R` |
| DATA-03 | `extended_schedule` requires `instrument`, `redcap_event_name`, and `repeat_instance` in that order. Extra, reordered, or incomplete columns raise an error. | `test-assessment-plan.R` |
| DATA-04 | Duplicate rows and unknown, unselected, unmapped, or invalid crossings raise an error before target creation. | `test-assessment-plan.R` |
| DATA-05 | Each extension adds its exact instance and preserves observed targets. | `test-assessment-plan.R` |
| DATA-06 | Classic extensions expand across all observed records. Longitudinal extensions expand across records observed in the matching arm. | `test-assessment-plan.R` |
| DATA-07 | An extension into an arm with zero observed records warns once and adds zero targets. | `test-assessment-plan.R` |
| DATA-08 | `plan_from_data()` rejects planner data with zero rows. | `test-assessment-plan.R` |

## `plan_explicit()`

| ID | Behavior checked | Evidence |
|---|---|---|
| EXPL-01 | Each permitted row in `explicit_schedule` creates one exact target. | `test-assessment-plan.R` |
| EXPL-02 | Omitted and `NULL` schedules raise an error. A complete schedule data frame with zero rows creates a plan with zero targets. | `test-assessment-plan.R` |
| EXPL-03 | `explicit_schedule` requires `record_id`, `instrument`, `redcap_event_name`, and `repeat_instance` in that order. Extra, reordered, incomplete, or duplicate rows raise an error. | `test-assessment-plan.R` |
| EXPL-04 | Observed combinations require a matching schedule row to become targets. | `test-assessment-plan.R` |
| EXPL-05 | Record IDs absent from planner data remain targets. The project type and the target's `redcap_event_name`, `repeat_instrument`, and `repeat_instance` determine the first failed check during `run_plan()`. | `test-assessment-plan.R`, `test-run-plan.R` |
| EXPL-06 | A record observed in one arm raises an error when scheduled into another arm. | `test-assessment-plan.R` |
| EXPL-07 | Correctly structured planner data with zero rows supports explicit targets for absent records. | `test-assessment-plan.R` |
| EXPL-08 | An absent classic target with `repeat_instance = NA_integer_` has both physical row checks marked `"not applicable"` and fails `instrument-started`. Absent longitudinal events and absent repeat instances fail their applicable physical row checks. | `test-run-plan.R`, `test-run-plan-verified.R` |

## `run_plan()` stages and results

| ID | Behavior checked | Evidence |
|---|---|---|
| RUN-01 | `run_plan()` rejects malformed plans, changed projects, and invalid runtime structure. | `test-assessment-plan.R`, `test-run-plan.R` |
| RUN-02 | A newer export with the same project structure uses the plan targets. Added rows create zero new targets. Each absent planned row receives the gate results defined by the project type and its `redcap_event_name`, `repeat_instrument`, and `repeat_instance`. | `test-run-plan.R` |
| RUN-03 | Diagnostics record the twelve documented stages in order. | `test-run-plan.R` |
| RUN-04 | Every longitudinal target receives `event-row-started`; classic targets receive status `"not applicable"`. | `test-run-plan.R` |
| RUN-05 | A target with a positive `repeat_instance` receives `repeat-instance-row-started` after its event check passes; a target with `repeat_instance = NA_integer_` receives status `"not applicable"`. | `test-run-plan.R`, `test-run-plan-verified.R` |
| RUN-06 | Failed physical row checks give later checks status `"not reached"`; `field-complete` runs after `instrument-started` passes. | `test-run-plan.R`, `test-run-plan-verified.R` |
| RUN-07 | `target_results` has one row per target, four check status columns, field counts, source, and reason. | `test-run-plan.R`, `test-flex-event-instruments.R` |
| RUN-08 | `summary` has the documented columns and types, status and reason, and `NA_real_` rates for unassessed checks. | `test-run-plan.R`, `test-get-summary.R` |
| RUN-09 | `missing` contains effective unresolved failures with typed structural missing values. | `test-run-plan.R`, `test-run-plan-verified.R`, `test-get-missing.R` |
| RUN-10 | `verification` contains counts for verification processing. | `test-run-plan.R`, `test-run-plan-verified.R` |
| RUN-11 | `details = TRUE` stores field rows with raw and effective dispositions, verification status, branching status, reason, and `value_summary`. | `test-run-plan.R`, `test-run-plan-verified.R` |
| RUN-12 | Ordinary assessed field values are character in `details$value_summary`; checkbox values list selected exported checkbox child column names. | `test-run-plan.R` |
| RUN-13 | `details = FALSE` returns `details = NULL`. Both detail settings have equal targets, summaries, missing rows, and verification counts. | `test-run-plan.R`, `test-run-plan-verified.R` |
| RUN-14 | Report components exclude source data, supplied verification rows, API tokens, and the live connection. | `test-run-plan.R` |
| RUN-15 | Reports with all passes, all failures, zero targets, inapplicable checks, and failed gates retain the documented schemas and counts. | `test-run-plan.R` |

## Instrument detection, field selection, and branching logic

| ID | Behavior checked | Evidence |
|---|---|---|
| FIELD-01 | Instrument start detection uses data entry fields except the record ID field, `descriptive` fields, and `calc` fields. Checkbox roots use exported child columns. | `test-run-plan.R` |
| FIELD-02 | A selected instrument with zero usable detection fields raises a project error. | `test-run-plan.R` |
| FIELD-03 | Missing detection, assessment, checkbox child, branching dependency, and required metadata columns raise an error before assessment. | `test-run-plan.R` |
| FIELD-04 | Logical controls require one nonmissing logical value. | `test-run-plan.R` |
| FIELD-05 | Character controls accept `NULL` or empty vectors and otherwise require unique, nonblank, unpadded values. Explicit `exclude_types` values must occur after the `required_fields` filter; `ignore_fields` names must remain after both preceding steps. | `test-run-plan.R` |
| FIELD-06 | `ignore_fields` accepts metadata field names. Exported checkbox child names raise an argument error. | `test-run-plan.R` |
| FIELD-07 | Field selection applies `required_fields`, `exclude_types`, and `ignore_fields` in that order. | `test-run-plan.R` |
| FIELD-08 | The three field selection arguments change `field-complete` assessment and preserve targets and the first three checks. | `test-run-plan.R` |
| FIELD-09 | Zero fields after selection or branching gives `field-complete` status `"not applicable"`, the documented reason, zero counts, and `NA_real_` rates. | `test-run-plan.R` |
| FIELD-10 | Same row logic, cross event logic, and checkbox root completeness use REDCap field codes and exact target context. | `test-run-plan.R` |
| FIELD-11 | A unique cross event source row with a missing `redcap_repeat_instance` is selected before source rows with positive repeat instances. When no missing instance row exists, one positive instance row is usable and multiple positive instance rows raise a project error. | `test-run-plan.R` |
| FIELD-12 | R missing values, factor missing values, logical missing values, numeric missing values, date and time missing values, `NaN`, empty strings, and strings containing whitespace are missing responses. | `test-run-plan.R` |
| FIELD-13 | `Inf`, `-Inf`, `"NA"`, `"N/A"`, `"NULL"`, `"."`, and `"-999"` are present responses. | `test-run-plan.R` |
| FIELD-14 | Physical row presence is independent of field response missingness. | `test-assessment-plan.R`, `test-run-plan.R` |
| FIELD-15 | Unresolved rows receive a REDCap data entry URL when the required URL values are available. | `test-run-plan.R` |

## Verification

| ID | Behavior checked | Evidence |
|---|---|---|
| VER-01 | `verified` and `verified_user` are supplied together. Two `NULL` values disable verification. | `test-run-plan-verified.R` |
| VER-02 | `verified` requires the nine documented columns. Extra columns are ignored. A complete data frame with zero rows is valid. | `test-run-plan-verified.R` |
| VER-03 | Every verification row is validated before filtering by username or status. | `test-run-plan-verified.R` |
| VER-04 | Event and repeat values accept typed missing values when their dimensions are inapplicable. | `test-run-plan-verified.R` |
| VER-05 | Documented character timestamps and finite `POSIXct` values become UTC. Character timestamps lacking a zone use UTC. | `test-run-plan-verified.R` |
| VER-06 | Username and status matching is exact and sensitive to letter case. | `test-run-plan-verified.R` |
| VER-07 | The latest row for each selected username and field context is used. Identical latest ties collapse and conflicting latest ties raise an error. | `test-run-plan-verified.R` |
| VER-08 | `"VERIFIED"` changes an assessed failed `field-complete` row. Targets, gates, instrument start, passing fields, and fields removed by policy or branching keep their prior results. | `test-run-plan-verified.R` |
| VER-09 | Verification input order leaves results unchanged. Zero matching rows produce audit counts. | `test-run-plan-verified.R` |

## Accessors, formatters, and progress

| ID | Behavior checked | Evidence |
|---|---|---|
| OUT-01 | `get_summary()` returns the documented columns and types, labels, status, reason, and filtered rows. | `test-get-summary.R` |
| OUT-02 | `get_missing()` returns the documented unresolved rows and types and leaves the stored report unchanged. | `test-get-missing.R` |
| OUT-03 | Accessor filters reject empty, missing, blank, padded, unknown, and noncharacter values. | `test-get-summary.R`, `test-get-missing.R` |
| OUT-04 | `flexify()` applies labels, removes two empty repeat columns, and displays inapplicable rates as blank cells. | `test-flexify.R` |
| OUT-05 | `flex_event_instruments()` calculates event and instrument metrics from `target_results`, applies `missing_threshold`, and rejects inconsistent event checks. | `test-flex-event-instruments.R` |
| OUT-06 | `flex_html()` renders a `flextable` returned by a package formatter as HTML. | `test-flex-html.R` |
| OUT-07 | `progress` validates one logical value, preserves assessment results, and closes its display after success or error. | `test-run-plan.R` |
