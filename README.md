
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redcapmissing

<!-- badges: start -->

![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-339999)
![License](https://img.shields.io/badge/license-MIT-blue.svg)
<!-- badges: end -->

<img src="man/figures/logo.svg" align="right" width="180" alt="redcapmissing hex logo" />

`redcapmissing` is an R package for working with REDCap and building
missingness reports.

It is designed for REDCap data-quality workflows where the expectation
of a field value depends on two logic layers:

1.  REDCap project structure, including:
    - project metadata
    - project mapping
    - repeating events and instruments
    - checkbox semantics
2.  External, user-provided constraints that cannot be specified in
    REDCap, including:
    - expected number of repeat instances for a given form
    - expected number or set of REDCap events for a given form

## Dependency on `redcapAPI`

`redcapmissing` is built on top of and depends heavily on
[`redcapAPI`](https://github.com/vubiostat/redcapAPI).

The main function in this package takes a
`redcapAPI::redcapConnection()` object as `rcon`. All project
information is discerned from the supplied `rcon` object. The `data`
argument is expected to be created with
`redcapAPI::exportRecordsTyped()`.

This package depends on `redcapAPI` for REDCap metadata access,
instrument labels, form-event mapping, repeating event/instrument
structure, and REDCap-style blank-value handling.

## Recommended typed export

For missingness reports, use an explicit `exportRecordsTyped()` cast so
branching-logic comparisons stay in REDCap code space while checkbox and
system fields remain in their raw REDCap export form.

``` r
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

If your project uses additional coded multiple-choice field types,
extend this same pattern so those fields are also kept in code space.

## Why use `redcapmissing`?

`redcapmissing` extends missingness assessment beyond the REDCap project
metadata and works around REDCap export behavior to produce informative
missingness reports from REDCap projects:

- `redcapmissing` reports five positive validation checks: event row for
  record exists, repeat instance row for record exists, form started,
  form complete, and fields complete.
- `redcapmissing` uses the `pointblank` package for validation plans and
  summary output.
- `redcapmissing` returns both row-level failures and validation-level
  summary output.

## Installation

``` r
# install.packages("pak")
pak::pkg_install("blankuzr/redcapmissing")
```

Installing `redcapmissing` also installs `redcapAPI` as a package
dependency.

## Core functions

- `find_missing()`
  - build a missingness report for one or more REDCap forms/instruments
- `tidy()`
  - return a focused validation summary tibble from a report object
- `flex()`
  - format the validation summary as a `flextable`
- `flex_html()`
  - render a `flextable` summary as an HTML string

## What the report returns

`find_missing()` returns a structured report object centered on a
standard `pointblank` agent. When multiple forms are requested, the
report combines all validation rows into one object and keeps the
assessed form in the `form` column. The most commonly used components
are:

- `report$agent`
  - the interrogated `pointblank` agent, including validation metadata
    and underlying summary counts
- `report$missing`
  - the row-level dataset detailing all failed validation checks
- `report$event_row_exists_failures`,
  `report$repeat_instance_row_exists_failures`,
  `report$form_started_failures`, `report$form_complete_failures`, and
  `report$fields_complete_failures`
  - the row-level failure datasets for each validation check
- `report$forms`, `report$form_labels`, `report$events`, and
  `report$instances`
  - the normalized form list, labels, selected events, and expanded
    repeat instance IDs used to build the report

## Tidy and formatted output

`tidy(report)` returns a focused validation summary tibble with one row
per validation step and REDCap context. It includes the assessed form
name, form label, validation label, REDCap event and repeat columns,
`assessed`, `passed`, `failed`, `pass_rate`, and `fail_rate`.

For reporting workflows, `flex()` formats that validation summary as a
`flextable` with a Context column, and `flex_html()` renders the
`flextable` as an HTML string for email or report insertion.

``` r
tidy_tbl <- tidy(report)
ft <- flex(report)
html <- flex_html(ft)
```

## Example

The example below uses a lightweight synthetic stand-in for a REDCap
connection so it can run without live REDCap access. In production use,
create `rcon` with `redcapAPI::redcapConnection()` and use the typed
export pattern shown above for `redcapAPI::exportRecordsTyped()`.

``` r
library(redcapmissing)

metadata <- tibble::tibble(
  field_name = c("record_id", "branch_flag", "required_note", "conditional_note"),
  form_name = c("baseline_form", "baseline_form", "baseline_form", "baseline_form"),
  field_type = c("text", "yesno", "text", "text"),
  field_label = c("Record ID", "Branch flag", "Required note", "Conditional note"),
  select_choices_or_calculations = c("", "", "", ""),
  text_validation_type_or_show_slider_number = c("", "", "", ""),
  branching_logic = c("", "", "", "[branch_flag] = '1'"),
  required_field = c("y", "y", "y", "y")
)

instruments <- tibble::tibble(
  instrument_name = "baseline_form",
  instrument_label = "Baseline form"
)

rcon <- list(
  metadata = function() metadata,
  instruments = function() instruments
)

records <- tibble::tibble(
  record_id = c("r1", "r2"),
  branch_flag = c("1", "0"),
  required_note = c("entered", "entered"),
  conditional_note = c("", "")
)

report <- find_missing(
  data = records,
  rcon = rcon,
  forms = "baseline_form"
)

report$missing[, c("record_id", "field_name", "validation_scope")]
```

## Validation checks

REDCap exports make it important to distinguish between a missing
**value** and a missing **row context**. In longitudinal and repeating
projects, a record can be missing because REDCap exported no row at all
for the relevant event or repeat instance. In other contexts, REDCap
does export a row, but every field on the form is still blank.
`redcapmissing` separates those cases into five validation checks.

### `event_row_exists`

This check passes when a form is offered on a longitudinal event and the
export has a row for that record-event context.

Why this exists:

- REDCap exports cannot show field-level blanks for an event row that
  was never exported.
- The package therefore uses the project form-event mapping from `rcon`
  to build expected record-event contexts before any field-level check
  can happen.
- When an expected event row is absent, the failed rows identify that
  event context instead of creating many synthetic field failures.

### `repeat_instance_row_exists`

This check passes when a form is assessed in a repeating event or as a
repeating instrument and the expected repeat instance row exists in the
export.

Why this exists:

- REDCap only exports repeat instances that actually exist.
- If instance 2 should exist but was never created, there is no exported
  row to inspect.
- `instances` therefore acts as an expected-row rule: the function
  builds the expected record-event-repeat contexts and compares them to
  what REDCap exported.
- When an expected repeat row is absent, the failed rows identify that
  repeat context.

### `form_started`

This check passes when REDCap exported the row for the form context and
at least one data-capturing field on that form is not blank or
unchecked.

Why this exists:

- In practice, this often means the form was available in REDCap but no
  real data entry started.
- REDCap may still export the record/event/repeat row even though every
  form field is empty.
- Reporting each field separately would overstate the problem, so failed
  `form_started` rows are kept as a single form-level failure for that
  context.

### `form_complete`

This check passes when the row context exists, the form is started, and
every expected field is complete for the record/event/repeat context.

Why this exists:

- Granular field-level counts can be much larger than the patient count
  because every expected patient-field combination is assessed.
- This roll-up reports one pass/fail result per evaluable patient
  context, so users can see how many records have fully complete
  expected fields.
- Records that fail `event_row_exists`, `repeat_instance_row_exists`, or
  `form_started` are not counted in this check because those failures
  are owned by upstream checks.

### `fields_complete`

This check passes when the row context exists, the form is started, and
a specific expected field is complete after REDCap branching logic is
evaluated.

Why this exists:

- This is the ordinary field-level missingness check.
- A field is only assessed after the package confirms the row context
  exists, the form is not wholly blank, the field is on the requested
  form, and its branching logic is open.
- This is where REDCap-specific missingness behaves most like standard
  value-level QA.

## Restricting assessment to selected events

When a form is offered on many REDCap events, you can restrict
assessment to a chosen subset with `events`. If you do not supply it,
the function defaults to all REDCap events where the form is offered.
With multiple forms, pass either one event vector for all forms or a
named list by form. Omitted list entries use that form's default offered
events.

``` r
followup_report <- find_missing(
  data = records,
  rcon = rcon,
  forms = "patient_status",
  events = c(
    "follow_up_1_arm_1",
    "follow_up_2_arm_1",
    "follow_up_3_arm_1"
  )
)
```

This is especially useful in longitudinal REDCap projects where several
events play the same conceptual role but only a subset should count
toward the current missingness review.

If a form is regular on some requested events and repeating on others,
the package applies validation checks by event type:

- regular-form events use the standard `fields_complete`,
  `form_started`, and `event_row_exists` logic
- repeating events and repeating-instrument events use
  `fields_complete`, `form_started`, and `repeat_instance_row_exists`
  logic
- both regular and repeating contexts use `form_complete` to roll
  expected field rows up to patient-context counts

When `instances` is omitted, the default `1L` assumption is only applied
for the requested events where the form actually repeats.

For form-specific event settings, use a named list:

``` r
multi_form_report <- find_missing(
  data = records,
  rcon = rcon,
  forms = c("imaging", "demographics"),
  events = list(
    imaging = c("event_2_arm_1", "event_3_arm_1")
  )
)
```

In this example, `imaging` is assessed only on the two listed events,
while `demographics` uses all events where REDCap offers that form. If
`imaging` repeats on one requested event and no `instances` entry is
supplied, the report assumes instance `1` for that repeating context and
records the expanded value in `report$instances$imaging`.

## Repeat expectations

For repeating events and instruments, scalar `instances` values apply a
count to all requested repeating contexts. A value of `2L` means
instances `1` and `2`. Vectors with length greater than one are treated
as exact repeat-instance IDs. With multiple forms, pass a named list for
form-specific expectations.

``` r
repeat_report <- find_missing(
  data = records,
  rcon = rcon,
  forms = "repeat_form",
  instances = 2L
)
```

This checks that repeat instances `1` and `2` exist everywhere that
`repeat_form` is expected. The key REDCap detail is that missing repeat
instances are absent as rows, not merely blank as values. `instances`
lets `redcapmissing` create those expected row contexts explicitly
before comparing them to the export.

## Acknowledgement and citation

### `redcapAPI`

This package relies heavily on `redcapAPI` and would not be practical
without it. If `redcapmissing` contributes to your work, please also
cite `redcapAPI`.

#### Foundational package citation

> Nutter B, Garbett S, Obregon S, Obadia T, Lehr M, High B, Lane S,
> Beasley W, Gray W, Kennedy N, Hsi-Nien T, Horner J, Stephens J, Beck
> C, Johnson B, Chase P, Tobias P (2026). *redcapAPI: Accessing data
> from REDCap projects using the API*. R package version 2.12.0.
> <https://doi.org/10.5281/zenodo.10564837>

#### Current package ownership and maintenance

Current public stewardship appears under VUMC Biostatistics /
`vubiostat`, with Shawn Garbett listed as maintainer in current package
documentation and the upstream project README stating that ownership
transfer to VUMC Biostatistics is complete.

Useful current `redcapAPI` references:

- VUMC Biostatistics redcapAPI project page and abstract by Savannah
  Obregon, Shawn Garbett, and Benjamin Nutter:
  <https://www.vumc.org/biostatistics/node/565>
- Current GitHub repository for the package:
  <https://github.com/vubiostat/redcapAPI>
- Current package site: <https://vubiostat.r-universe.dev/redcapAPI>

### `pointblank`

This package relies on `pointblank` for validating missingness,
standardizing return summaries, and exposing row-level flags.

#### Current package resources

Useful current `pointblank` references:

- R package GitHub repository: <https://github.com/rstudio/pointblank>
- R package site: <https://rstudio.github.io/pointblank/>
- Python package GitHub repository:
  <https://github.com/posit-dev/pointblank>
- Python package site: <https://posit-dev.github.io/pointblank/>

## Learn more

See the package vignette for a fuller synthetic walk-through of
branching-aware and repeat-aware validation.

## Development

``` r
devtools::document()
devtools::test()
devtools::check()
```
