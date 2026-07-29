
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

## First success with a synthetic offline project

This credential-free example uses a `redcapOfflineConnection` and a
small synthetic classic project to construct, run, and inspect a plan.

``` r
library(redcapmissing)

metadata <- data.frame(
  field_name = c("record_id", "started", "value"),
  form_name = "baseline",
  field_type = "text",
  field_label = c("Record ID", "Started", "Value"),
  required_field = c("y", "", "y")
)
project_info <- data.frame(
  project_id = "1",
  is_longitudinal = "0",
  has_repeating_instruments_or_events = "0"
)
rcon <- suppressWarnings(redcapAPI::offlineConnection(
  meta_data = metadata,
  project_info = project_info,
  repeat_instrument = redcapAPI::REDCAP_REPEAT_INSTRUMENT_STRUCTURE
))
records <- data.frame(
  record_id = c("1", "2"),
  started = c("yes", "yes"),
  value = c("complete", "")
)

plan <- plan_from_data(records, rcon, "baseline")
report <- run_plan(plan, records, rcon, progress = FALSE)
knitr::kable(
  get_summary(report)[, c("validation_check", "status", "failed")]
)
```

| validation_check            | status         | failed |
|:----------------------------|:---------------|-------:|
| event-row-started           | not applicable |      0 |
| repeat-instance-row-started | not applicable |      0 |
| instrument-started          | assessed       |      0 |
| field-complete              | assessed       |      1 |

``` r
knitr::kable(
  get_missing(report)[, c("record_id", "validation_check", "field_name")]
)
```

| record_id | validation_check | field_name |
|:----------|:-----------------|:-----------|
| 2         | field-complete   | value      |

Both records start the instrument; record `2` then fails
`field-complete` because its required `value` field is blank.

## Prepare a live connection and records

Supported `rcon` objects inherit from `redcapApiConnection`, returned by
`redcapAPI::redcapConnection()`, or `redcapOfflineConnection`, returned
by `redcapAPI::offlineConnection()` or
`redcapAPI::readPreservedProject()`. `instruments` is a character vector
of raw REDCap instrument names. Use `all_instruments(rcon)` when the
plan should include the complete project inventory. Keep API tokens
outside source files, console output, reports, and saved R objects.

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

Build an `extended_schedule` when selected instruments should also be
assessed at every event where REDCap permits them. The builder's
instruments may be all plan instruments or a subset:

``` r
instruments <- all_instruments(rcon)
extended_schedule <- build_extended_schedule(
  rcon = rcon,
  instruments = c("followup", "diary"),
  n_repeat_instances = 3L
)
```

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

In a classic project, builder rows use
`redcap_event_name = NA_character_`; repeating instruments receive
instances `1` through `n_repeat_instances`. In a longitudinal project,
the builder returns only concrete instrument/event crossings designated
in REDCap. A requested project instrument designated to no longitudinal
event contributes no row and produces one classed warning; the builder
never invents an eventless longitudinal crossing. Omit
`extended_schedule` to request observed-only planning.

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
selected instruments, `assessible_targets`, project identity, project
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
`field-complete`, in that order. When no fields remain, `field-complete`
has status `"not applicable"` and reason
`"no fields remain after field policy"`.

`run_plan()` accepts a newer record export when the REDCap project
structure matches the stored fingerprint. The plan continues to supply
its `assessible_targets`.

Runtime response requirements follow the frozen targets, not every
instrument retained in the plan's declared scope. `run_plan()` requires
the detection and assessment fields for instruments represented in
`assessible_targets`, along with any fields needed to evaluate their
branching logic and the structural columns needed to match targets. An
instrument with no target imposes no exclusive response-column
requirement.

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
registry()
get_summary(report)
get_missing(report, validation_check = "field-complete")
get_summary(report, instruments = instruments)
```

`registry()` documents the exact check codes, report levels, assessment
order, presentation labels, and pass conditions used throughout the
package.

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
- Open `?all_instruments`, `?build_extended_schedule`,
  `?plan_from_data`, `?plan_explicit`, and `?run_plan` for exact
  argument and return value descriptions.
- Read [NEWS](NEWS.md) for release changes.
- Report problems through [GitHub
  Issues](https://github.com/blankuzr/redcapmissing/issues).
