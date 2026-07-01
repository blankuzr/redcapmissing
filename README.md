
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
form-event mapping, repeating event/instrument structure, and
REDCap-style blank-value handling.

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

- `redcapmissing` extends missingness assessment to five scopes: missing
  field, any field missing, missing form, missing event, and missing
  repeat instance.
- `redcapmissing` uses the `pointblank` package for validation plans and
  summary output.
- `redcapmissing` returns both row-level failures and scope-level
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
  - build a missingness report for one REDCap form/instrument
- `summary()`
  - return the unmodified `pointblank` validation summary from a report
    object
- `flex()`
  - format the validation summary as a `flextable`
- `flex_html()`
  - render a `flextable` summary as an HTML string

## What the report returns

`find_missing()` returns a structured report object centered on a
standard `pointblank` agent. The most commonly used components are:

- `report$agent`
  - the interrogated `pointblank` agent, including validation metadata
    and underlying summary counts
- `report$missing`
  - the row-level dataset detailing all failed validation scopes
- `report$form_missing`, `report$event_missing`, `report$repeat_missing`
  - the row-level datasets for the three whole-context missingness
    scopes
- `report$any_field_missing`
  - the row-level patient-context roll-up for contexts with at least one
    missing expected field

## Summary and formatted output

`summary(report)` returns the `pointblank` validation summary stored
inside `report$agent$validation_set`. It includes `validation_context`
plus the REDCap event and repeat columns used to stratify `n`,
`n_passed`, `n_failed`, `f_passed`, and `f_failed`.

For reporting workflows, `flex()` formats that validation summary as a
`flextable` with a Context column, and `flex_html()` renders the
`flextable` as an HTML string for email or report insertion.

``` r
summary_tbl <- summary(report)
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

rcon <- list(metadata = function() metadata)

records <- tibble::tibble(
  record_id = c("r1", "r2"),
  branch_flag = c("1", "0"),
  required_note = c("entered", "entered"),
  conditional_note = c("", "")
)

report <- find_missing(
  data = records,
  rcon = rcon,
  form = "baseline_form"
)

report$missing[, c("record_id", "field_name", "missing_scope")]
```

## Missingness scopes

REDCap exports make it important to distinguish between a missing
**value** and a missing **row context**. In longitudinal and repeating
projects, a record can be missing because REDCap exported no row at all
for the relevant event or repeat instance. In other contexts, REDCap
does export a row, but every field on the form is still blank.
`redcapmissing` separates those cases into five scopes.

### `event_absent`

Use this scope when a form is offered on a longitudinal event, but the
export has no row at all for that record-event context.

Why this exists:

- REDCap exports cannot show field-level blanks for an event row that
  was never exported.
- The package therefore uses the project form-event mapping from `rcon`
  to build expected record-event contexts before any field-level check
  can happen.
- When an expected event row is absent, the report returns one row for
  the missing event context instead of many synthetic field failures.

### `repeat_absent`

Use this scope when a form is assessed in a repeating event or as a
repeating instrument and an expected repeat instance row does not exist
in the export.

Why this exists:

- REDCap only exports repeat instances that actually exist.
- If instance 2 should exist but was never created, there is no exported
  row to inspect.
- `instances` therefore acts as an expected-row rule: the function
  builds the expected record-event-repeat contexts and compares them to
  what REDCap exported.
- When an expected repeat row is absent, the report returns one row for
  that missing repeat context.

### `form_blank`

Use this scope when REDCap exported the row for the form context, but
every data-capturing field on that form is blank or unchecked.

Why this exists:

- In practice, this often means the form was available in REDCap but no
  real data entry started.
- REDCap may still export the record/event/repeat row even though every
  form field is empty.
- Reporting each field separately would overstate the problem, so the
  package records a single form-level failure for that context.

### `any_field_missing`

Use this scope when the row context exists, the form is not wholly
blank, and at least one expected field is blank for the
record/event/repeat context.

Why this exists:

- Granular field-level counts can be much larger than the patient count
  because every expected patient-field combination is assessed.
- This roll-up reports one pass/fail result per evaluable patient
  context, so users can see how many records have any missing expected
  field.
- Records that fail `event_absent`, `repeat_absent`, or `form_blank` are
  not counted in this scope because those failures are owned by upstream
  scopes.

### `field`

Use this scope when the row context exists, the form is not wholly
blank, and a specific field is expected after REDCap branching logic is
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

``` r
followup_report <- find_missing(
  data = records,
  rcon = rcon,
  form = "patient_status",
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
the package applies scopes by event type:

- regular-form events use the standard `field`, `form_blank`, and
  `event_absent` logic
- repeating events and repeating-instrument events use `field`,
  `form_blank`, and `repeat_absent` logic
- both regular and repeating contexts use `any_field_missing` to roll
  expected field rows up to patient-context counts

When `instances` is omitted, the default `1L` assumption is only applied
for the requested events where the form actually repeats.

## Repeat expectations

For repeating events and instruments, `instances` applies a uniform
expectation to all assessed record/event contexts.

``` r
repeat_report <- find_missing(
  data = records,
  rcon = rcon,
  form = "repeat_form",
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
