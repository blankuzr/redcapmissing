# `redcapmissing` 7.0.0 behavior-to-test contract matrix

This indexes public plan-and-run behavior against named automated tests. It is a coverage map, not a substitute for test assertions.

- **Covered**: the named test directly asserts the behavior.
- **Partial**: the test asserts part of it; the missing part is stated.
- **Gap**: no named automated test directly asserts it.
- **Workflow**: evidence comes from a package-validation or repository-audit command.

References use `file -- "test_that name"`.

## Public API and vocabulary

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| API-01 | Constructor signatures/defaults are exact. | `test-assessment-plan.R` -- "plan constructors expose the exact public signatures" | **Covered** |
| API-02 | `run_plan()` has the exact ten formals/defaults. | `test-run-plan.R` -- "run_plan exposes the exact plan execution API and report schemas" | **Covered** |
| API-03 | Validation checks are exactly `event-row-started`, `repeat-instance-row-started`, `instrument-started`, and `field-complete`; levels use instrument terminology. | `test-registry.R` -- "registry exposes the plan-and-run validation taxonomy"; "context validation levels use instrument terminology" | **Covered** |
| API-04 | Public report columns, filters, labels, headers, and metrics use `instrument`/`instruments`. | `test-get-summary.R` -- "get_summary exposes the exact typed plan-and-run schema"; `test-get-missing.R` -- "get_missing exposes canonical typed structural values"; `test-flexify.R` -- "flexify uses instrument labels and current validation labels" | **Covered** |
| API-05 | `find_missing()` and `flex_event_forms()` are absent without aliases. | `test-registry.R` -- "retired monolithic and form formatter APIs are not exported"; `test-flex-event-instruments.R` -- "flex_event_instruments replaces the retired form API" | **Covered** |
| API-06 | Public failures/warnings use package subclasses for argument, schema, project, schedule, plan, verification, and empty-arm extension. | `test-assessment-plan.R` -- "extensions into an arm with no observed records warn and add no targets"; "all public condition subclasses inherit from package base classes" | **Covered** |

## Plan representation and project snapshot

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| PLAN-01 | Both constructors return `redcapmissing_plan` with the exact six components; construction is `from_data` or `explicit`. | `test-assessment-plan.R` -- "plan_from_data creates an exact compact plan from physical classic rows"; "plan_explicit freezes exact targets including records absent from data" | **Covered** |
| PLAN-02 | `assessible_targets` has the exact six columns and canonical storage. | `test-assessment-plan.R` -- "plan_from_data creates an exact compact plan from physical classic rows"; "target dimensions normalize across classic longitudinal and repeat modes" | **Covered** |
| PLAN-03 | Targets are unique on five keys and deterministically ordered by instrument, event, record, repeat kind, and instance. | `test-assessment-plan.R` -- "target dimensions normalize across classic longitudinal and repeat modes"; "plan validation rejects malformed components and target invariants" | **Covered** for representative classic, longitudinal, and repeat orderings. |
| PLAN-04 | Provenance is limited to `observed`, `extended`, `observed+extended`, and `explicit`. | `test-assessment-plan.R` -- "classic repeating instruments produce exact observed and extended instances"; "plan_explicit freezes exact targets including records absent from data"; "plan validation rejects malformed components and target invariants" | **Covered** |
| PLAN-05 | Project identity/record field/longitudinal status are canonical; the plan retains no input data or live connection. | `test-assessment-plan.R` -- "plan_from_data creates an exact compact plan from physical classic rows"; "plan project label maps are canonical complete and protected" | **Covered** |
| PLAN-06 | Plan printing is compact, reports construction/counts, and never prints record IDs. | `test-assessment-plan.R` -- "plan_from_data creates an exact compact plan from physical classic rows" | **Covered** |
| PLAN-07 | SHA-256 fingerprinting is deterministic despite source-table row order. | `test-assessment-plan.R` -- "project fingerprints are stable to table row order with explicit record identity"; "plan project label maps are canonical complete and protected" | **Covered** |
| PLAN-08 | Malformed class/components/version/targets/sources/project/fingerprint are rejected. | `test-assessment-plan.R` -- "plan validation rejects hand edits and changed project structure"; "plan validation rejects malformed components and target invariants" | **Covered** |
| PLAN-09 | Constructors cache each connection surface once and never export records. | `test-assessment-plan.R` -- "constructors require complete cached project surfaces and never export records" | **Covered** |
| PLAN-10 | Required project surfaces fail closed when missing, including repeat configuration. | `test-assessment-plan.R` -- "constructors require complete cached project surfaces and never export records"; "repeat configuration must be explicit and consistent with project status" | **Covered** |
| PLAN-11 | Project event/instrument label maps are named, unique, nonmissing, deterministic, and complete for selected instruments. | `test-assessment-plan.R` -- "plan project label maps are canonical complete and protected" | **Covered** |
| PLAN-12 | Genuine `redcapAPI::offlineConnection()` surfaces support classic plan construction and execution. | `test-redcapapi-integration.R` -- "genuine redcapAPI offline connections support the plan-and-run workflow" | **Covered** |

## Shared structural normalization

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| NORM-01 | Discover the record-ID field; accept character/factor/integer/finite double, normalize to character, and preserve character leading zeros. | `test-assessment-plan.R` -- "record identifiers normalize accepted storage and reject noncanonical values"; "numeric identifiers and fingerprints are option independent" | **Covered** |
| NORM-02 | Reject missing, blank, padded, `NaN`, infinite, or absent IDs; never trim silently. | `test-assessment-plan.R` -- "record identifiers normalize accepted storage and reject noncanonical values" | **Covered** |
| NORM-03 | Longitudinal rows require known raw events; classic event absence/blanks/typed missing normalize to `NA_character_`, while nonblank classic values error. | `test-assessment-plan.R` -- "nullable structural dimensions accept every typed NA representation"; "event missingness is contextual for classic and longitudinal planning" | **Covered** |
| NORM-04 | Any repeating design requires `redcap_repeat_instrument` and `redcap_repeat_instance` together. | `test-assessment-plan.R` -- "repeat columns and instances enforce canonical contextual missingness"; "repeat configuration must be explicit and consistent with project status" | **Covered** |
| NORM-05 | Positive integer, whole double, and canonical character instances normalize to integer. | `test-assessment-plan.R` -- "repeat columns and instances enforce canonical contextual missingness" | **Covered** |
| NORM-06 | Reject leading zeros, zero, negatives, decimals, absent required instances, text, `NaN`, infinity, overflow, and nonmissing factor instances. | `test-assessment-plan.R` -- "repeat columns and instances enforce canonical contextual missingness"; "nonmissing factor repeat instances are rejected" | **Covered** |
| NORM-07 | Structural blanks and logical/character/integer/double/factor missing normalize to typed `NA` only when inapplicable. | `test-assessment-plan.R` -- "nullable structural dimensions accept every typed NA representation" | **Covered** for classic regular data and both schedules. |
| NORM-08 | Validate regular, repeating-event, repeating-instrument, and mixed regular/repeating contexts against project structure. | `test-assessment-plan.R` -- "target dimensions normalize across classic longitudinal and repeat modes"; "schedule schemas and allowable crossings fail closed" | **Covered** |
| NORM-09 | Normalized physical keys are unique; collisions error and no row is discarded. | `test-assessment-plan.R` -- "structural values normalize strictly without silent row loss" | **Covered** |
| NORM-10 | Planning uses physical-row presence, not response content, and needs structural columns only. | `test-assessment-plan.R` -- "plan_from_data creates an exact compact plan from physical classic rows" | **Covered** |
| NORM-11 | `plan_from_data()` rejects zero rows; `plan_explicit()` accepts correctly structured zero-row data. | `test-assessment-plan.R` -- "zero-row schedules require complete ordered schemas and allowed storage" | **Covered** |

## `plan_from_data()` construction

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| DATA-01 | Targets equal allowable crossings intersected with observed crossings union extension crossings. | `test-assessment-plan.R` -- "plan_from_data unions observed and arm-scoped longitudinal extensions" | **Covered** |
| DATA-02 | Omitted, `NULL`, and correctly typed zero-row extensions are equivalent observed-only requests. | `test-assessment-plan.R` -- "zero-row schedules require complete ordered schemas and allowed storage" | **Covered** |
| DATA-03 | Extension columns are exactly `instrument`, `redcap_event_name`, `repeat_instance`, in order; incomplete zero-row, extra, and reordered schemas error. | `test-assessment-plan.R` -- "zero-row schedules require complete ordered schemas and allowed storage" | **Covered** |
| DATA-04 | Normalized duplicates and unknown/unselected/unmapped/structurally invalid crossings error before intersection. | `test-assessment-plan.R` -- "normalized schedule collisions error before target construction"; "unknown and unmapped schedule crossings fail before intersection"; "schedule schemas and allowable crossings fail closed" | **Covered** |
| DATA-05 | Missing schedule rows never remove observed targets; exact instance `2` adds only 2. | `test-assessment-plan.R` -- "classic repeating instruments produce exact observed and extended instances"; "plan_from_data unions observed and arm-scoped longitudinal extensions" | **Covered** |
| DATA-06 | Classic rows expand across all records; longitudinal rows expand only in the applicable arm. | `test-assessment-plan.R` -- "classic repeating instruments produce exact observed and extended instances"; "plan_from_data unions observed and arm-scoped longitudinal extensions" | **Covered** |
| DATA-07 | Empty-arm extension warns once with its class and adds no target. | `test-assessment-plan.R` -- "extensions into an arm with no observed records warn and add no targets" | **Covered** |
| DATA-08 | Unselected or unmapped physical rows cannot create invalid targets. | `test-assessment-plan.R` -- "unselected and unmapped physical rows never fabricate targets" | **Covered** |

## `plan_explicit()` construction

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| EXPL-01 | Targets are the exact allowable explicit crossings and never expand. | `test-assessment-plan.R` -- "plan_explicit freezes exact targets including records absent from data" | **Covered** |
| EXPL-02 | Omitted/`NULL` schedules error; a full-schema zero-row schedule assesses nothing. | `test-assessment-plan.R` -- "a typed zero-row explicit schedule assesses nothing"; "required constructor arguments and instrument vectors fail with classed errors" | **Covered** |
| EXPL-03 | Explicit columns are exactly `record_id`, `instrument`, `redcap_event_name`, `repeat_instance`, in order; extras/incomplete zero-row/duplicates error. | `test-assessment-plan.R` -- "zero-row schedules require complete ordered schemas and allowed storage"; "normalized schedule collisions error before target construction" | **Covered** |
| EXPL-04 | Observed rows and selected instruments absent from the schedule create no targets. | `test-assessment-plan.R` -- "explicit omissions exclude observed crossings and absent selected instruments" | **Covered** |
| EXPL-05 | IDs absent from data remain targets and can fail physical-row gates. | `test-assessment-plan.R` -- "plan_explicit freezes exact targets including records absent from data"; `test-run-plan.R` -- "run_plan freezes targets and gates absent physical rows" | **Covered** |
| EXPL-06 | An observed record cannot be scheduled into a contradictory arm. | `test-assessment-plan.R` -- "plan_explicit freezes exact targets including records absent from data" | **Covered** |
| EXPL-07 | Structured zero-row data may support nonempty wholly absent explicit targets. | `test-assessment-plan.R` -- "zero-row schedules require complete ordered schemas and allowed storage" | **Covered** |
| EXPL-08 | Absent regular, repeating-event, and repeating-instrument targets fail at the correct upstream gate. | `test-run-plan.R` -- "run_plan freezes targets and gates absent physical rows"; "zero targets and fully gated targets reconcile across report components"; "repeating-event gates distinguish absent events from absent instances"; `test-run-plan-verified.R` -- "verification cannot bypass a missing repeat-instance gate" | **Covered** |

## Runner lifecycle, gating, and results

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| RUN-01 | Reject malformed/incompatible plans, schema versions, projects, fingerprints, and runtime structure. | `test-assessment-plan.R` -- "plan validation rejects hand edits and changed project structure"; `test-run-plan.R` -- "run_plan rejects malformed plans, changed projects, and runtime structure" | **Covered** |
| RUN-02 | A newer same-project snapshot is allowed; targets stay frozen, new rows are ignored, absent targets fail. | `test-run-plan.R` -- "newer data snapshots cannot add targets and absent planned rows still fail" | **Covered** |
| RUN-03 | Diagnostics record the fixed twelve stages in order. | `test-run-plan.R` -- "run_plan exposes the exact plan execution API and report schemas" | **Covered** |
| RUN-04 | Every longitudinal target gets an event-row gate based on any record-event row; classic event gates are not applicable. | `test-run-plan.R` -- "run_plan exposes the exact plan execution API and report schemas"; "longitudinal event gates use any physical row in the record-event"; "zero targets and fully gated targets reconcile across report components" | **Covered** |
| RUN-05 | Repeat gates run only after event gates; regular repeat gates are not applicable. | `test-run-plan.R` -- "run_plan freezes targets and gates absent physical rows"; "repeating-event gates distinguish absent events from absent instances"; `test-run-plan-verified.R` -- "verification cannot bypass a missing repeat-instance gate" | **Covered** |
| RUN-06 | Failed event/repeat gates make downstream checks not reached; field checks run only after instrument-started passes. | `test-run-plan.R` -- "run_plan freezes targets and gates absent physical rows"; "zero targets and fully gated targets reconcile across report components"; `test-run-plan-verified.R` -- "verification cannot bypass a missing repeat-instance gate" | **Covered** |
| RUN-07 | `target_results` has one row per target, exact schema, provenance, four status columns, counts, and reason; statuses use the four public values. | `test-run-plan.R` -- "run_plan result components preserve every documented storage type"; "field details retain target provenance for same-context instruments"; "mixed target outcomes reconcile statuses summaries missing rows and provenance"; "zero targets and fully gated targets reconcile across report components"; `test-flex-event-instruments.R` -- "event-instrument formatter rejects malformed target results" | **Covered** |
| RUN-08 | `summary` has exact schema/types, status/reason, and `NA_real_` rates when unassessed. | `test-run-plan.R` -- "an empty field policy is not applicable rather than complete"; `test-get-summary.R` -- "get_summary exposes the exact typed plan-and-run schema" | **Covered** |
| RUN-09 | `missing` holds effective unresolved failures only, with typed structural missing values. | `test-get-missing.R` -- "get_missing exposes canonical typed structural values"; `test-run-plan.R` -- "zero targets and fully gated targets reconcile across report components"; "mixed target outcomes reconcile statuses summaries missing rows and provenance"; "all failing targets reconcile exactly without losing structural absence"; `test-run-plan-verified.R` -- "exact latest VERIFIED evidence overrides only a failed field check" | **Covered** |
| RUN-10 | `verification` stores audit counts, not supplied rows. | `test-run-plan.R` -- "run_plan result components preserve every documented storage type"; `test-run-plan-verified.R` -- "a complete zero-row verification template is valid and audited" | **Covered** |
| RUN-11 | Details retain raw/effective/verification disposition; compact mode omits them without changing targets, summaries, or missing rows. | `test-run-plan.R` -- "compact and detailed runs have identical assessment results"; `test-run-plan-verified.R` -- "exact latest VERIFIED evidence overrides only a failed field check" | **Covered** |
| RUN-12 | Results retain no source data, raw verification, token, or live connection. | `test-run-plan.R` -- "run_plan retains no source data, verification rows, connection, or token" | **Covered** |
| RUN-13 | All-pass, all-fail, zero-target, N/A, and fully gated reports retain exact schemas and reconcile counts. | `test-run-plan.R` -- "run_plan result components preserve every documented storage type"; "zero targets and fully gated targets reconcile across report components"; "an empty field policy is not applicable rather than complete"; "mixed target outcomes reconcile statuses summaries missing rows and provenance"; "all failing targets reconcile exactly without losing structural absence" | **Covered** |

## Instrument detection, field policy, and responses

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| FIELD-01 | Start detection uses all data-entry fields except record ID, `descriptive`, and `calc`, with checkbox-child expansion. | `test-run-plan.R` -- "instrument detection uses the complete independent field set"; "branching and checkbox roots retain REDCap completeness semantics"; "field policy changes only field-complete assessment"; "selected instruments require at least one usable start-detection field" | **Covered** |
| FIELD-02 | An instrument with no usable detection fields errors. | `test-run-plan.R` -- "selected instruments require at least one usable start-detection field" | **Covered** |
| FIELD-03 | Missing detection, field, checkbox-child, branching-dependency, or required metadata columns error before assessment. | `test-run-plan.R` -- "instrument detection requires every exported detection column"; "cross-event branching uses the matching record and event context"; "planner and runner data reject non-atomic response columns"; "checkbox metadata must define unambiguous exported children"; "malformed branching logic raises a package project condition"; "run_plan rejects incomplete metadata before field resolution" | **Covered** |
| FIELD-04 | Logical controls are single nonmissing logicals. | `test-run-plan.R` -- "run_plan validates scalar and named field-policy arguments"; "logical and character controls enforce their complete scalar/vector contracts" | **Covered** |
| FIELD-05 | Field/type vectors allow `NULL`/empty and otherwise require unique, character, nonblank, unpadded, relevant values; unknown/unused values error. | `test-run-plan.R` -- "run_plan validates scalar and named field-policy arguments"; "logical and character controls enforce their complete scalar/vector contracts"; "explicit exclusions must remain relevant after the required-only filter"; "the default descriptive exclusion is safe when the type is absent" | **Covered** |
| FIELD-06 | `ignore_fields` accepts metadata roots, not checkbox-child export names. | `test-run-plan.R` -- "run_plan validates scalar and named field-policy arguments" | **Covered** |
| FIELD-07 | Resolution order is all fields -- required-only -- type exclusions -- named exclusions. | `test-run-plan.R` -- "field policy changes only field-complete assessment"; "an empty field policy is not applicable rather than complete"; "explicit exclusions must remain relevant after the required-only filter" | **Covered** |
| FIELD-08 | Field policy changes only field-complete; excluded/ignored/optional fields may still establish instrument started. | `test-run-plan.R` -- "field policy changes only field-complete assessment"; "every field-policy argument leaves targets and upstream checks invariant" | **Covered** |
| FIELD-09 | No remaining fields yields exact N/A status/reason, zero counts, and `NA_real_` rates, not a pass. | `test-run-plan.R` -- "an empty field policy is not applicable rather than complete"; "all branch-closed fields are not applicable rather than complete" | **Covered** |
| FIELD-10 | Same-row and cross-event branching plus checkbox-root completeness are preserved. | `test-run-plan.R` -- "branching and checkbox roots retain REDCap completeness semantics"; "cross-event branching uses the matching record and event context" | **Covered** |
| FIELD-11 | Typed R missing, factor/logical/numeric/date-time missing, `NaN`, empty, and whitespace responses are missing. | `test-run-plan.R` -- "response missingness distinguishes R missing values from literal text"; "typed response missing values fail while nonfinite values remain literal" | **Covered** |
| FIELD-12 | `Inf`, `-Inf`, and literals `NA`, `N/A`, `NULL`, `.`, `-999` are nonmissing. | `test-run-plan.R` -- "response missingness distinguishes R missing values from literal text" | **Covered** |
| FIELD-13 | Physical row presence is independent of response missingness. | `test-assessment-plan.R` -- "plan_from_data creates an exact compact plan from physical classic rows" | **Covered** at planning; no separate runner gate assertion. |
| FIELD-14 | Unresolved rows receive canonical REDCap URLs when URL metadata exists. | `test-run-plan.R` -- "longitudinal unresolved rows receive canonical REDCap URLs" | **Covered** for longitudinal field failure; other contexts/missing URL not isolated. |

## Verification

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| VER-01 | `verified` and `verified_user` are mutually dependent; both `NULL` disables verification. | `test-run-plan-verified.R` -- "verification arguments are paired and require the nine-column schema" | **Covered** |
| VER-02 | The nine required columns are mandatory; extras are ignored; complete zero-row is valid and incomplete zero-row errors. | `test-run-plan-verified.R` -- "verification arguments are paired and require the nine-column schema"; "a complete zero-row verification template is valid and audited"; "verification extras are ignored and finite POSIXct timestamps normalize" | **Covered** |
| VER-03 | Validate and normalize every row before user/status filtering across every key, context, timestamp, status, and username. | `test-run-plan-verified.R` -- "all verification rows are validated before user filtering"; "verification rejects invalid identity and text columns before filtering"; "verification rejects every malformed timestamp before filtering"; "verified_user requires one nonblank unpadded character scalar"; "longitudinal verification rejects malformed event IDs before filtering"; "repeating verification rejects malformed instrument and instance keys" | **Covered** |
| VER-04 | Nullable event/repeat dimensions accept typed missing only when structurally inapplicable. | `test-run-plan-verified.R` -- "verification nullable columns normalize typed missing values"; "longitudinal regular verification requires only its event key"; "repeating-event verification requires an instance and no repeat instrument"; "repeating-instrument verification requires its instrument and instance" | **Covered** |
| VER-05 | Supported character and POSIXct timestamps normalize to UTC; timezone-less input means UTC. | `test-run-plan-verified.R` -- "mixed documented timestamp formats are normalized row by row"; "verification timestamps preserve offsets and fractional ordering"; "nonfinite POSIXct verification timestamps fail closed"; "verification extras are ignored and finite POSIXct timestamps normalize" | **Covered** |
| VER-06 | Username/status matching is exact and case-sensitive; only the selected user's latest exact context participates. | `test-run-plan-verified.R` -- "verification never changes passing fields and username matching is case sensitive"; "latest user status is order independent and non-VERIFIED does not apply"; "verification status and user matching are exact without empty-result warnings" | **Covered** |
| VER-07 | Input order is irrelevant; identical latest ties collapse; conflicting latest ties error. | `test-run-plan-verified.R` -- "latest user status is order independent and non-VERIFIED does not apply"; "identical latest verification duplicates collapse harmlessly"; "conflicting latest verification ties fail closed" | **Covered** |
| VER-08 | Exact `VERIFIED` changes only an otherwise-failing, already-assessed field-complete row. | `test-run-plan-verified.R` -- "exact latest VERIFIED evidence overrides only a failed field check" | **Covered** for a failing assessed field. |
| VER-09 | Verification cannot add targets, bypass event/repeat gates, start instruments, affect passing fields, or restore branch/policy-removed fields. | `test-run-plan-verified.R` -- "verification cannot bypass a missing repeat-instance gate"; "verification never changes passing fields and username matching is case sensitive"; "verification cannot start an instrument or restore removed fields"; "verification cannot create targets or bypass a failed event gate" | **Covered** |
| VER-10 | Zero applicable rows/users produce no warning; audit counts explain the outcome. | `test-run-plan-verified.R` -- "a complete zero-row verification template is valid and audited"; "verification status and user matching are exact without empty-result warnings" | **Covered** |

## Accessors and formatters

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| DOWN-01 | `get_summary()` has the exact typed schema, labels, status/reason, filters, and malformed-storage checks. | `test-get-summary.R` -- "get_summary exposes the exact typed plan-and-run schema"; "get_summary filters by checks, events, and instruments"; "get_summary rejects malformed stored summaries" | **Covered** |
| DOWN-02 | `get_missing()` has exact typed unresolved output, canonical missing structure, filters, and no report mutation. | `test-get-missing.R` -- "get_missing exposes canonical typed structural values"; "get_missing filters only the completed result"; "get_missing rejects retired filters and malformed storage" | **Covered** |
| DOWN-03 | Accessor filters reject empty, missing, blank, padded, unknown, and noncharacter values. | `test-get-summary.R` -- "accessor filter vectors reject missing blank and padded values"; "accessor filters enforce the complete invalid-value contract" | **Covered** |
| DOWN-04 | `flexify()` accepts final schemas, applies labels, drops absent repeat columns, and renders N/A rates blank. | `test-flexify.R` -- "flexify accepts exact summary and missing accessor schemas"; "flexify uses instrument labels and current validation labels"; "flexify drops jointly absent repeat columns without mutating input"; "flexify returns a presentation table with N/A rates blank" | **Covered** |
| DOWN-05 | `flex_event_instruments()` replaces the old API, validates inputs, and computes frozen-target metrics. | `test-flex-event-instruments.R` -- "flex_event_instruments replaces the retired form API"; "event-instrument data is computed from frozen target results"; "missing-threshold comparison is strict below one"; "event-instrument formatter validates its public threshold"; "event-instrument formatter rejects malformed target results"; "flex_event_instruments returns a flextable when dependencies exist" | **Covered** |
| DOWN-06 | `flex_html()` consumes final schemas and uses instrument terminology/N/A display. | `test-flex-html.R` -- "flex_html renders formatted summary HTML when optional packages are available"; "flex_html renders multi-instrument formatted missing rows" | **Covered** |

## Optimized execution architecture

| ID | Required invariant | Test or workflow evidence | Coverage |
|---|---|---|---|
| PERF-01 | Constructors use native structural columns for matching, provenance, duplicate detection, and ordering; delimiter-like identifiers cannot collide. | `test-assessment-plan.R` -- "native target identities do not collide on delimiter-like values"; "planning materializes moderate record expansions with exact provenance" | **Covered** |
| PERF-02 | Fingerprints distinguish typed missing values, literal sentinel-like text, and delimiter-containing sequences while remaining independent of source row order. | `test-assessment-plan.R` -- "fingerprints distinguish missing and delimiter-safe structured values" | **Covered** |
| PERF-03 | Branching plans compile once per unique expression, evaluate across records with record-specific outcomes, and fail closed for ambiguous repeated-only cross-event sources. | `test-run-plan.R` -- "run_plan evaluates shared branching plans across record vectors"; "cross-event branching uses the matching record and event context" | **Covered** |
| PERF-04 | Compact execution does not construct detailed validation rows and remains semantically identical to detailed execution. | `test-run-plan.R` -- "compact execution does not construct detailed validation rows"; "compact and detailed runs have identical assessment results" | **Covered** |
| PERF-05 | Verification normalizes timestamps in batches and groups exact contexts on native typed columns. | `test-run-plan-verified.R` -- "verification preparation batches many native field contexts"; timestamp and latest-tie tests under VER-05 through VER-07 | **Covered** |
| PERF-06 | Event/instrument formatting aggregates high-cardinality contexts in bulk and rejects inconsistent record-event gates. | `test-flex-event-instruments.R` -- "event-instrument aggregation scales by native contexts"; "event aggregation detects conflicting record-event gates" | **Covered** |
| PERF-07 | Constructor (character/numeric IDs and no/partial/full extension overlap), runner, verification-history, failure-density, branching, detailed/compact, and formatter workloads have reproducible timing and allocation evidence without test-time thresholds. | Representative tiers in `tools/benchmark-plan-run.R` | **Workflow** |
| PERF-08 | The package has one plan-native engine, no retired entry point, no new-to-old argument translation, and no callable legacy fallback. | AST definition/export/call, exact-signature, and public-body scope checks in `tools/audit-plan-run-boundary.R`; `test-registry.R` -- "retired monolithic and form formatter APIs are not exported" | **Workflow** |

## Operational, release, and validation evidence

| ID | Public behavior | Test evidence | Coverage |
|---|---|---|---|
| OPS-01 | Progress validates its scalar, is silent when disabled, preserves results, and cleans up on success/error. | `test-run-plan.R` -- "logical and character controls enforce their complete scalar/vector contracts"; "progress updates all stages and cleans up on success and error" | **Covered** |
| OPS-02 | Runner and constructors bound connection calls and fetch each surface once per call. | `test-assessment-plan.R` -- "constructors require complete cached project surfaces and never export records"; `test-run-plan.R` -- "runner reads every project structure surface once" | **Covered** |
| OPS-03 | Benchmark smoke uses construct-plan -- run-plan. | `tools/benchmark-plan-run.R` smoke execution | **Workflow** |
| OPS-04 | Version is 7.0.0 with imported `digest`; roxygen/Rd/NAMESPACE, README, vignette, package docs, and NEWS agree. | `devtools::document()`; direct README and vignette renders; `devtools::check()` vignette build/rebuild; diff audit | **Workflow** |
| OPS-05 | Historical NEWS remains; no pkgdown/CI is added; opaque fixture is retired after synthetic translation. | Repository file/diff audit | **Workflow** |
| OPS-06 | Focused tests, `devtools::test()`, `devtools::check()`, stale-term search, `git diff --check`, and leak/artifact scans pass. | Final validation transcript | **Workflow** |
| OPS-07 | The static architecture-boundary audit parses every R source file and rejects retired definitions, exports, calls, named former-pipeline helpers, legacy scope symbols in the three public workflows, signature drift, restored former-engine files, and retired validation codes. | `Rscript tools/audit-plan-run-boundary.R` | **Workflow** |

## Coverage status

All normative behavior rows marked **Covered** are directly exercised by named automated tests. Rows marked **Workflow** are established by documentation generation, artifact rendering, package checks, reproducible benchmarks, and repository audits.
