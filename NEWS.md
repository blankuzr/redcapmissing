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
