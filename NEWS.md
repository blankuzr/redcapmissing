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
