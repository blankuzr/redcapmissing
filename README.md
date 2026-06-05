<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.svg" align="right" height="180" alt="redcapmissing hex logo" />

# redcapmissing

<!-- badges: start -->
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-339999)
![License](https://img.shields.io/badge/license-MIT-blue.svg)
<!-- badges: end -->

`redcapmissing` builds branching-aware missingness reports for REDCap record exports.

It is designed for REDCap data-quality workflows where expected values depend on project structure, including:

- branching logic
- longitudinal event mapping
- repeating instruments
- checkbox semantics
- whole-form missingness
- `pointblank` validation summaries and failed-row extracts

## Why use `redcapmissing`?

`redcapmissing` separates REDCap-aware expectation building from auditable validation output:

- R code determines which record, event, repeat, and field contexts should be checked.
- `pointblank` records the validation plan and exposes scope-specific summaries.
- Row-level failures remain easy to extract for correction workflows.

## Installation

```r
# install.packages("pak")
pak::pkg_install("blankuzr/redcapmissing")
```

## Core functions

- `redcap_missing_report()`
  - build a missingness report for one REDCap form/instrument
- `redcap_missing_summary()`
  - format the `pointblank` validation summary from a report object

## Example

```r
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

The package can identify four scopes of missingness for a requested form:

- `event_absent`
- `repeat_absent`
- `form_blank`
- `field`

## Repeat expectations

For repeating instruments, `expected_repeats` applies a uniform expectation to all assessed record/event contexts.

```r
repeat_report <- redcap_missing_report(
  data = records,
  rcon = rcon,
  form = "repeat_form",
  expected_repeats = 2L
)
```

This checks that repeat instances `1` and `2` exist everywhere that `repeat_form` is expected.

## Learn more

See the package vignette for a fuller synthetic walk-through of branching-aware and repeat-aware validation.

## Development

```r
devtools::document()
devtools::test()
devtools::check()
```
