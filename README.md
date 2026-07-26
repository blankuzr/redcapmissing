
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redcapmissing

<!-- badges: start -->

![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-339999)
![License](https://img.shields.io/badge/license-MIT-blue.svg)
<!-- badges: end -->

`redcapmissing` records which REDCap record, event, instrument, and
repeat instance combinations require assessment. `run_plan()` evaluates
physical rows, instrument start, and field completeness for those
combinations.

<p align="center">

<img src="man/figures/logo.svg" width="160" alt="redcapmissing hex logo" />
</p>

## Installation

`redcapmissing` requires R 4.1.0 or later. Install it from GitHub:

``` r
# install.packages("pak")
pak::pak("blankuzr/redcapmissing")
```

## Prepare the connection and records

The examples assume that `rcon` is a `redcapAPI::redcapConnection()`
that can read the project structure and records used below.
`instruments` is a character vector of raw REDCap instrument names. Keep
API tokens outside source files, console output, reports, and saved R
objects.

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

`plan_from_data()` uses the project record ID field and the applicable
`redcap_event_name`, `redcap_repeat_instrument`, and
`redcap_repeat_instance` columns to identify observed crossings.
`run_plan()` evaluates their responses.

## 1. Construct an assessment plan

Use `plan_from_data()` to begin with record, event, instrument, and
repeat instance combinations observed in `records` and permitted by the
project structure:

``` r
plan <- plan_from_data(
  data = records,
  rcon = rcon,
  instruments = instruments
)
```

Pass a caller supplied `extended_schedule` to add permitted
combinations:

``` r
plan <- plan_from_data(
  data = records,
  rcon = rcon,
  instruments = instruments,
  extended_schedule = extended_schedule
)
```

Each extension row expands across records observed in its arm. In a
classic project, it expands across all observed records. Crossings
observed in `records` remain in the plan when their rows are absent from
`extended_schedule`.

Use `plan_explicit()` when every assessed combination must have a row in
a caller supplied `explicit_schedule`:

``` r
plan <- plan_explicit(
  data = records,
  rcon = rcon,
  instruments = instruments,
  explicit_schedule = explicit_schedule
)
```

Each row in `explicit_schedule` creates one permitted target. A target
may name a record that is absent from `records`. During `run_plan()`, an
absent longitudinal event fails `event-row-started`; a present event
with an absent repeat instance fails `repeat-instance-row-started`; and
an absent classic target with `repeat_instance = NA_integer_` has both
row checks marked `"not applicable"` and fails `instrument-started`.

Both constructors return a `redcapmissing_plan`. The plan contains the
selected instruments, Assessible targets, project identity, project
structure fingerprint, construction value, and schema version. Source
records and `rcon` remain outside the plan.

## 2. Run the plan

``` r
report <- run_plan(
  plan = plan,
  data = records,
  rcon = rcon
)
```

The runner evaluates four checks in order:

1.  `event-row-started`
2.  `repeat-instance-row-started`
3.  `instrument-started`
4.  `field-complete`

Classic event checks have status `"not applicable"`. Repeat instance
checks have that status when a target stores `NA_integer_` in
`repeat_instance`. A failed physical row check gives later checks status
`"not reached"`. `instrument-started` uses data entry metadata fields
other than the record ID field and fields of type `descriptive` or
`calc`. Checkbox roots require at least one selected exported child
column. This detection set is independent of `required_fields`,
`exclude_types`, and `ignore_fields`. Those three arguments affect
`field-complete`, in that order. When they leave zero assessible fields,
`field-complete` has status `"not applicable"` and reason
`"no assessible fields after field policy"`.

`run_plan()` accepts a newer record export when the REDCap project
structure matches the stored fingerprint. The plan continues to supply
its Assessible targets.

### Verified field failures

Supply caller provided `verified` and `verified_user` together to apply
the latest exact `"VERIFIED"` status for an assessed `field-complete`
failure:

``` r
report <- run_plan(
  plan,
  records,
  rcon,
  verified = verified,
  verified_user = verified_user
)
```

See `?run_plan` for the required verification columns, accepted
timestamps, and the exact field context used for matching. The
`verification` component contains counts for the supplied rows and
applied results.

## 3. Inspect results

``` r
get_summary(report)
get_missing(report, validation_check = "field-complete")
get_summary(report, instruments = instruments)
```

The result contains `plan`, `target_results`, `summary`, `missing`,
`verification`, `diagnostics`, and `details`. Structural absence uses
typed `NA` values.

### Stored field values

With `details = TRUE`, `details$value_summary` stores each assessed
ordinary field value as character. For a checkbox field,
`details$value_summary` contains the names of its selected exported
checkbox child columns. Reports also contain record IDs and may contain
REDCap data entry URLs.

With `details = FALSE`, the result contains `details = NULL`. Apply the
same storage, access, retention, and sharing rules to each report that
apply to its REDCap export.

## Learn more

- Run `vignette("redcapmissing")` for complete schedule schemas, missing
  value rules, repeating structure, branching logic, verification, and
  recovery from validation errors.
- Open `?plan_from_data`, `?plan_explicit`, and `?run_plan` for exact
  argument and return value descriptions.
- Read [NEWS](NEWS.md) for release changes.
- Report problems through [GitHub
  Issues](https://github.com/blankuzr/redcapmissing/issues).
