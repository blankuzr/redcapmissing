
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redcapmissing

<!-- badges: start -->

![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-339999)
![License](https://img.shields.io/badge/license-MIT-blue.svg)
<!-- badges: end -->

<!-- canonical-readme-introduction: start -->

<img src="man/figures/logo.svg" align="right" width="160"
     alt="redcapmissing hex logo" />

`redcapmissing` identifies and contextualizes missing data in REDCap
databases through four practical data-entry checks:

- **Event started** · `event-row-started`<br> Has the event been started
  for this patient?
- **Repeat instance started** · `repeat-instance-row-started`<br> Has
  the repeat instance been started for this patient?
- **Form started** · `instrument-started`<br> Has the form been started
  for this patient?
- **Field complete** · `field-complete`<br> Is the field complete for
  this patient?

Results are returned as tidy data frames, making it easy to filter,
group, and create custom summaries with familiar R tools.
`redcapmissing` also provides built-in summaries and optional detailed
results, supports comparisons with prior reports, and can incorporate
verified resolutions from REDCap's Data Resolution Workflow when the
Data Quality API module is enabled.
<!-- canonical-readme-introduction: end -->

## Installation

`redcapmissing` requires R 4.1.0 or later. Install it from GitHub:

``` r
# install.packages("pak")
pak::pak("blankuzr/redcapmissing")
```

## Prepare a live connection and records

Supported `rcon` objects inherit from `redcapApiConnection`, returned by
`redcapAPI::redcapConnection()`, or `redcapOfflineConnection`, returned
by `redcapAPI::offlineConnection()` or
`redcapAPI::readPreservedProject()`.

``` r
library(redcapmissing)

records <- redcapAPI::exportRecordsTyped(
  rcon,
  cast = list(
    radio = redcapAPI::castCode,
    dropdown = redcapAPI::castCode,
    yesno = redcapAPI::castCode,
    truefalse = redcapAPI::castCode,
    checkbox = redcapAPI::castRaw,
    system = redcapAPI::castRaw
  )
)
```

## Offline example

This credential-free example uses a longitudinal
`redcapOfflineConnection`. The `study_baseline` form is offered at
`baseline`; the `testing` and `visit` forms are offered at each of the
three test events.

### Prepare an offline connection and records

Define a small synthetic REDCap project and its observed records:

``` r
library(redcapmissing)

metadata <- tibble::tribble(
  ~field_name, ~form_name, ~field_type, ~field_label,
  ~select_choices_or_calculations,
  ~text_validation_type_or_show_slider_number, ~branching_logic,
  ~required_field,
  "record_id", "study_baseline", "text", "Record ID", "", "", "", "",
  "group", "study_baseline", "radio", "Group",
  "A, Group A | B, Group B", "", "", "y",
  "test_performed", "testing", "radio", "Test performed",
  "yes, Yes | no, No", "", "", "y",
  "test_date", "testing", "text", "Test date", "", "date_ymd",
  "[test_performed] = 'yes'", "y",
  "visit_status", "visit", "radio", "Visit status",
  "1, Status 1 | 2, Status 2 | 3, Status 3", "", "", "y",
  "visit_date", "visit", "text", "Visit date", "", "date_ymd", "", "y"
)

project_info <- tibble::tibble(
  project_id = "1",
  is_longitudinal = "1",
  has_repeating_instruments_or_events = "0"
)

events <- tibble::tribble(
  ~event_id, ~arm_num, ~unique_event_name, ~event_name,
  101L, 1L, "baseline", "Baseline",
  102L, 1L, "test_event_1", "Test event 1",
  103L, 1L, "test_event_2", "Test event 2",
  104L, 1L, "test_event_3", "Test event 3"
)

instrument_metadata <- tibble::tribble(
  ~instrument_name, ~instrument_label,
  "study_baseline", "Study baseline",
  "testing", "Testing",
  "visit", "Visit"
)

event_mapping <- tibble::tribble(
  ~arm_num, ~unique_event_name, ~form,
  1L, "baseline", "study_baseline",
  1L, "test_event_1", "testing",
  1L, "test_event_1", "visit",
  1L, "test_event_2", "testing",
  1L, "test_event_2", "visit",
  1L, "test_event_3", "testing",
  1L, "test_event_3", "visit"
)

rcon <- suppressWarnings(redcapAPI::offlineConnection(
  meta_data = metadata,
  project_info = project_info,
  arms = tibble::tibble(arm_num = 1L, name = "Arm 1"),
  events = events,
  instruments = instrument_metadata,
  mapping = event_mapping,
  repeat_instrument = redcapAPI::REDCAP_REPEAT_INSTRUMENT_STRUCTURE
))

records <- tibble::tribble(
  ~record_id, ~redcap_event_name, ~group, ~test_performed, ~test_date,
  ~visit_status, ~visit_date,
  "001", "baseline", "A", NA_character_, NA_character_, NA_character_, NA_character_,
  "001", "test_event_1", NA_character_, "yes", "2026-01-10", "1", "2026-01-11",
  "001", "test_event_2", NA_character_, "no", NA_character_, "2", "2026-02-11",
  "001", "test_event_3", NA_character_, "yes", "2026-03-10", "3", "2026-03-11",
  "002", "baseline", "B", NA_character_, NA_character_, NA_character_, NA_character_,
  "002", "test_event_1", NA_character_, "yes", NA_character_, NA_character_, NA_character_,
  "002", "test_event_2", NA_character_, NA_character_, NA_character_, "2", NA_character_,
  "002", "test_event_3", NA_character_, NA_character_, NA_character_, "3", NA_character_,
  "003", "baseline", "A", NA_character_, NA_character_, NA_character_, NA_character_,
  "003", "test_event_1", NA_character_, "no", NA_character_, NA_character_, NA_character_,
  "004", "baseline", "B", NA_character_, NA_character_, NA_character_, NA_character_,
  "004", "test_event_1", NA_character_, "yes", NA_character_, "1", NA_character_,
  "004", "test_event_2", NA_character_, "no", NA_character_, "2", "2026-02-14",
  "004", "test_event_3", NA_character_, "yes", NA_character_, NA_character_, "2026-03-14"
)
```

### 1. Construct an assessment plan

Use `plan_from_data()` to identify the record, event, and instrument
combinations observed in `records` and permitted by the project
structure:

``` r
plan <- plan_from_data(
  data = records,
  rcon = rcon,
  instruments = all_instruments(rcon)
)
```

### 2. Run the plan

Evaluate the planned combinations against the observed records:

``` r
report <- run_plan(plan, records, rcon, progress = FALSE)
```

### 3. Inspect results

`registry()` documents the exact check codes, report levels, assessment
order, presentation labels, and pass conditions used throughout the
package.

The result contains `plan`, `target_results`, `summary`, `missing`,
`verification`, `diagnostics`, `details`, and normalized field-selection
`settings`. Structural absence uses typed `NA` values.

#### Stored field values

With `details = TRUE`, `details$value_summary` stores each assessed
ordinary field value as character. For a checkbox field,
`details$value_summary` contains the names of its selected exported
checkbox child columns. Reports also contain record IDs and may contain
REDCap data entry URLs.

With `details = FALSE`, the result contains `details = NULL`. Apply the
same storage, access, retention, and sharing rules to each report that
apply to its REDCap export.

Review the failed checks by event and instrument, then inspect the
individual missing fields:

``` r
get_summary(report) |>
  dplyr::filter(failed > 0L) |>
  dplyr::select(redcap_event_name, instrument, validation_check, failed) |>
  knitr::kable()
```

| redcap_event_name | instrument | validation_check   | failed |
|:------------------|:-----------|:-------------------|-------:|
| test_event_1      | testing    | field-complete     |      2 |
| test_event_2      | testing    | instrument-started |      1 |
| test_event_3      | testing    | instrument-started |      1 |
| test_event_3      | testing    | field-complete     |      1 |
| test_event_1      | visit      | instrument-started |      2 |
| test_event_1      | visit      | field-complete     |      1 |
| test_event_2      | visit      | field-complete     |      1 |
| test_event_3      | visit      | field-complete     |      2 |

``` r

get_missing(report) |>
  dplyr::select(
    record_id,
    redcap_event_name,
    instrument,
    validation_check,
    field_name
  ) |>
  knitr::kable()
```

| record_id | redcap_event_name | instrument | validation_check   | field_name   |
|:----------|:------------------|:-----------|:-------------------|:-------------|
| 002       | test_event_2      | testing    | instrument-started | NA           |
| 002       | test_event_3      | testing    | instrument-started | NA           |
| 002       | test_event_1      | visit      | instrument-started | NA           |
| 003       | test_event_1      | visit      | instrument-started | NA           |
| 002       | test_event_1      | testing    | field-complete     | test_date    |
| 004       | test_event_1      | testing    | field-complete     | test_date    |
| 004       | test_event_3      | testing    | field-complete     | test_date    |
| 004       | test_event_1      | visit      | field-complete     | visit_date   |
| 002       | test_event_2      | visit      | field-complete     | visit_date   |
| 002       | test_event_3      | visit      | field-complete     | visit_date   |
| 004       | test_event_3      | visit      | field-complete     | visit_status |

`plan_from_data()` scopes this report to record-event crossings
represented in `records`. Records `001`, `002`, and `004` have rows at
all three test events, so the plan assesses both permitted forms at each
event, even when a form is entirely blank, and reports applicable form-
and field-level failures.

Record `003` has rows only at `baseline` and `test_event_1`. Its blank
`visit` form at `test_event_1` is assessed and reported as not started,
but `test_event_2` and `test_event_3` never enter the plan. The
potentially expected `testing` and `visit` assessments at those two
events (four targets in total) are therefore absent from the report
rather than counted as passes or failures for record `003`.

## Verified field failures

For projects using REDCap's Data Resolution Workflow, export history
through the [Data Quality API
module](https://github.com/vanderbilt-redcap/data_quality_api) and pass
it directly to `run_plan()`. The module must be enabled for the project.
Supply the exact reviewer username to apply their latest `"VERIFIED"`
status to eligible `field-complete` failures:

``` r
verified <- export_data_quality(rcon)
report <- run_plan(
  plan,
  records,
  rcon,
  verified = verified,
  verified_user = verified_user
)
```

Use `records = c("1", "2")` in `export_data_quality()` to restrict
retrieval. The default retrieves the complete project history and can be
expensive; export once and reuse the table. `run_plan()` ignores
contexts outside its plan and never retrieves history itself. A newer
nonverified or missing status prevents fallback to an older
verification.

Projects without this workflow can omit both verification arguments. See
`?export_data_quality` for prerequisites and returned columns,
`?run_plan` for matching rules, and `vignette("redcapmissing")` for a
runnable synthetic example with before/after results. The report's
`verification` component records supplied evidence and applied
overrides.

## Compare assessments

Reports generated with `details = TRUE` can be compared for changes.

**`compare_reports()` — create the comparison**

`compare_reports(previous, current)` returns a
`redcapmissing_comparison` object. The returned object records plan
membership in two components:

- **Target results** · `comparison$target_results`<br> Contains the
  union of exact target keys from both plans. Its `target_scope` column
  is `"shared"` when a target is in both plans, `"added"` when it is
  only in the current plan, and `"removed"` when it is only in the
  previous plan.
- **Scope changes** · `comparison$scope_changes`<br> Contains the added
  and removed target rows, including targets with no failures.

The input reports must have matching project structure and
field-selection settings. Plans and verification setups may differ.
Reports saved without settings or details must be regenerated for
comparison. See the [runnable longitudinal and repeating-instrument
example](vignettes/redcapmissing.html#compare-assessments) for both
population views and changing field denominators.

**`get_summary()` — choose a comparison population**

`get_summary(comparison)` returns summary rows with a `population`
column. Its `population` argument accepts one or both of these values:

- **Full scope** · `population = "full"`<br> Compares each report
  exactly as assessed, including targets that entered or left the plan.
- **Shared targets** · `population = "shared"`<br> Compares only the
  exact target keys present in both plans. Stored outcomes are retained
  without reevaluating branching logic.

**`get_changes()` — inspect failure transitions**

`get_changes(comparison)` returns failures present in either report with
a `change` column. Its `change` argument accepts `NULL` to return every
transition, or one or more of these exact values:

- **Newly detected** · `change = "newly_detected"`<br> The check fails
  in the current report but did not fail previously.
- **Still missing** · `change = "still_missing"`<br> The check fails in
  both reports.
- **Completed** · `change = "completed"`<br> The check failed previously
  and now passes directly.
- **Verified** · `change = "verified"`<br> The check failed previously
  and now passes through verification.
- **No longer assessed** · `change = "no_longer_assessed"`<br> The check
  failed previously but is now blocked by branching logic or an upstream
  gate; this does not represent completed data entry.
- **Added to scope** · `change = "added_to_scope"`<br> The failed target
  appears only in the current plan.
- **Removed from scope** · `change = "removed_from_scope"`<br> The
  failed target appears only in the previous plan.

In this example, both reports use the same plan, and the current export
changes only the four fields reported missing for record `004`:
`test_date` at `test_event_1` and `test_event_3`, `visit_date` at
`test_event_1`, and `visit_status` at `test_event_3`.

``` r
previous <- run_plan(
  plan,
  records,
  rcon,
  details = TRUE,
  progress = FALSE
)
current_records <- records |>
  dplyr::mutate(
    test_date = dplyr::case_when(
      record_id == "004" &
        redcap_event_name == "test_event_1" ~ "2026-01-14",
      record_id == "004" &
        redcap_event_name == "test_event_3" ~ "2026-03-14",
      TRUE ~ test_date
    ),
    visit_status = dplyr::if_else(
      record_id == "004" & redcap_event_name == "test_event_3",
      "3",
      visit_status
    ),
    visit_date = dplyr::if_else(
      record_id == "004" & redcap_event_name == "test_event_1",
      "2026-01-15",
      visit_date
    )
  )
current <- run_plan(
  plan,
  current_records,
  rcon,
  details = TRUE,
  progress = FALSE
)
comparison <- compare_reports(previous, current)

get_summary(
  comparison,
  validation_check = "field-complete",
  population = "full"
) |>
  dplyr::select(
    population,
    redcap_event_name,
    instrument,
    previous_failed,
    previous_assessed,
    current_failed,
    current_assessed,
    completed
  ) |>
  knitr::kable()
```

| population | redcap_event_name | instrument | previous_failed | previous_assessed | current_failed | current_assessed | completed |
|:---|:---|:---|---:|---:|---:|---:|---:|
| full | baseline | study_baseline | 0 | 4 | 0 | 4 | 0 |
| full | test_event_1 | testing | 2 | 7 | 1 | 7 | 1 |
| full | test_event_2 | testing | 0 | 2 | 0 | 2 | 0 |
| full | test_event_3 | testing | 1 | 4 | 0 | 4 | 1 |
| full | test_event_1 | visit | 1 | 4 | 0 | 4 | 1 |
| full | test_event_2 | visit | 1 | 6 | 1 | 6 | 0 |
| full | test_event_3 | visit | 2 | 6 | 1 | 6 | 1 |

``` r

get_changes(comparison, change = "completed") |>
  dplyr::select(
    record_id,
    redcap_event_name,
    instrument,
    validation_check,
    field_name,
    change
  ) |>
  knitr::kable()
```

| record_id | redcap_event_name | instrument | validation_check | field_name | change |
|:---|:---|:---|:---|:---|:---|
| 004 | test_event_1 | testing | field-complete | test_date | completed |
| 004 | test_event_3 | testing | field-complete | test_date | completed |
| 004 | test_event_1 | visit | field-complete | visit_date | completed |
| 004 | test_event_3 | visit | field-complete | visit_status | completed |

## Learn more

- Run `vignette("redcapmissing")` for complete schedule schemas, missing
  value rules, repeating structure, branching logic, verification, and
  recovery from validation errors.
- Open `?all_instruments`, `?build_explicit_schedule`,
  `?build_extended_schedule`, `?plan_from_data`, `?plan_explicit`, and
  `?run_plan` for exact argument and return value descriptions.
- Read [NEWS](NEWS.md) for release changes.
- Report problems through [GitHub
  Issues](https://github.com/blankuzr/redcapmissing/issues).
