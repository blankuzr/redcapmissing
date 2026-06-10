
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redcapmissing

<!-- badges: start -->

![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-339999)
![License](https://img.shields.io/badge/license-MIT-blue.svg)
<!-- badges: end -->

<img src="man/figures/logo.svg" align="right" width="180" alt="redcapmissing hex logo" />

`redcapmissing` builds missingness reports for REDCap
record exports.

It is designed for REDCap data-quality workflows where the expectation 
of values for a field is dependant on the following 2 logic layers:

1. REDCap project structure, including:
  - project metadata
  - project mapping
  - repeating instruments
  - checkbox semantics

2. External, user provided constraints that cannot be specified in REDCap including:
  - expected number of repeat instances for a given form
  - expected number / set of redcap events for a given form

## Dependency on `redcapAPI`

`redcapmissing` is built on top of and depends heavily on
[`redcapAPI`](https://github.com/vubiostat/redcapAPI).

The main function in this package takes 
the `redcapAPI::redcapConnection()` rcon object as an argument. 
All project information is discerned from the supplied rcon object. 
The data passed to the main function argument `data` is expected to be
created with `redcapAPI::exportRecordsTyped()`.


## Why use `redcapmissing`?

`redcapmissing` extends functionality for assessing missingness requiring information beyond
the REDCap project metadata, and works around REDCap export behavior to produce informative 
missing fields reports from REDCap projects:

- `redcapmissing` extends missingness assessment to 4 scopes
  (missing field, missing form, missing event, missing repeat instance)
- `redcapmissing` utilizes the `pointblank` package for validation and summary outputs
- `redcapmissing` returns both row-level failures as well as a summary of missingness for all 4 scopes. 


## Installation

``` r
# install.packages("pak")
pak::pkg_install("blankuzr/redcapmissing")
```

Installing `redcapmissing` also installs `redcapAPI` as a package
dependency.

## Core functions

- `redcap_missing_report()`
  - build a missingness report for one REDCap form/instrument
- `redcap_missing_summary()`
  - format the `pointblank` validation summary from a report object

## What the report returns

`redcap_missing_report()` returns a standard `pointblank` object.
The most commonly used components are:

- `report$agent`
  - the interrogated `pointblank` agent, including validation metadata
    and the underlying summary counts
- `report$missing`
  - the row-level dataset detailing missingness for the field scope
- `report$form_missing`, `report$event_missing`, `report$repeat_missing`
  - the row-level datasets for the three whole-context missingness scopes

## Summary helper

`redcap_missing_summary()` is a convenience formatter for the
`pointblank` summary stored inside `report$agent`. 

It returns:

- `agent_summary`
  - a `flextable` object
- `agent_summary_html`
  - an HTML representation of the same summary table

``` r
summary_tbl <- redcap_missing_summary(report)
summary_tbl$agent_summary
summary_tbl$agent_summary_html
```

## Example

The example below uses a lightweight synthetic stand-in for a REDCap
connection so it can run without live REDCap access. In production use,
create `rcon` with `redcapAPI::redcapConnection()` and export records
with `redcapAPI::exportRecordsTyped()`.

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

report <- redcap_missing_report(
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
`redcapmissing` separates those cases into four scopes.

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

Use this scope when a form is a repeating instrument and an expected
repeat instance row does not exist in the export.

Why this exists:

- REDCap only exports repeat instances that actually exist.
- If instance 2 should exist but was never created, there is no exported
  row to inspect.
- `expected_repeats` therefore acts as an expected-row rule: the
  function builds the expected record-event-repeat contexts and compares
  them to what REDCap exported.
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

### `field`

Use this scope when the row context exists, the form is not wholly
blank, and a specific field is expected after REDCap branching logic is
evaluated.

Why this exists:

- This is the ordinary field-level missingness check.
- A field is only assessed after the package confirms the row context
  exists, the form is not wholly blank, the field is on the requested
  form, and its branching logic is open.


## Restricting assessment to selected events

When a form is offered on many REDCap events, you can restrict
assessment to a chosen subset with `desired_events`. If you do not
supply it, the function defaults to all REDCap events where the form is
offered.

``` r
followup_report <- redcap_missing_report(
  data = records,
  rcon = rcon,
  form = "patient_status",
  desired_events = c(
    "follow_up_1_arm_1",
    "follow_up_2_arm_1",
    "follow_up_3_arm_1"
  )
)
```

This is especially useful in longitudinal REDCap projects where several
events play the same conceptual role but only a subset should count
toward the current missingness review.

## Repeat expectations

For repeating instruments, `expected_repeats` applies a uniform
expectation to all assessed record/event contexts.

``` r
repeat_report <- redcap_missing_report(
  data = records,
  rcon = rcon,
  form = "repeat_form",
  expected_repeats = 2L
)
```

This checks that repeat instances `1` and `2` exist everywhere that
`repeat_form` is expected. The key REDCap detail is that missing repeat
instances are absent as rows, not merely blank as values within an existing row.
`expected_repeats` lets `redcapmissing` create those expected row
contexts explicitly before comparing them to the export.

## Forms having both repeat and non-repeat instrument context

If a form is regular on some requested events and repeating on others,
the package applies scopes by event type:

- regular-form events use the standard `field`, `form_blank`, and
  `event_absent` logic
- repeating-instrument events use `field`, `form_blank`, and
  `repeat_absent` logic

When `expected_repeats` is omitted, the default `1L` assumption is only
applied for the requested events where the form actually repeats. 
For more details on this scenario, please see the redcapmissing vignette. 

## Acknowledgement and citation

This package relies heavily on `redcapAPI` and would not be practical
without it. If `redcapmissing` contributes to your work, please also
cite `redcapAPI`.

### Foundational package citation

> Nutter B, Garbett S, Obregon S, Obadia T, Lehr M, High B, Lane S,
> Beasley W, Gray W, Kennedy N, Hsi-Nien T, Horner J, Stephens J, Beck
> C, Johnson B, Chase P, Tobias P (2026). *redcapAPI: Accessing data
> from REDCap projects using the API*. R package version 2.12.0.
> <https://doi.org/10.5281/zenodo.10564837>

### Current package ownership and maintanance

The `redcapAPI` package is managed by VUMC Biostatistics / `vubiostat`
<https://github.com/vubiostat>

Useful current `redcapAPI` references:

- VUMC Biostatistics redcapAPI project page and abstract by Savannah
  Obregon, Shawn Garbett, and Benjamin Nutter:
  <https://www.vumc.org/biostatistics/node/565>
- Current GitHub repository for the package:
  <https://github.com/vubiostat/redcapAPI>
- Current package site: <https://vubiostat.r-universe.dev/redcapAPI>

## Learn more about `redcapmissing`

See the package vignette for a fuller synthetic walk-through of
branching-aware and repeat-aware validation.

## Development

``` r
devtools::document()
devtools::test()
devtools::check()
```
