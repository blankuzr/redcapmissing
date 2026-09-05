test_that("comparison instrument metrics agree with both original reports", {
  reports <- comparison_reports_fixture()
  comparison <- compare_reports(reports$previous, reports$current)
  for (threshold in c(0, 0.5, 1)) {
    parts <- .comparison_build_instrument_table(comparison, threshold, "full")
    for (side in c("previous", "current")) {
      original <- .flex_event_instruments_build_table(
        reports[[side]],
        threshold
      )
      expected <- original$data
      columns <- c(
        "N",
        "Instrument Incomplete",
        "Instrument Not Started",
        "Instrument Missing Threshold"
      )
      shown <- c(
        "N (started/due)",
        columns[2:3],
        original$missing_threshold_heading
      )
      prefix <- if (side == "previous") "Previous" else "Current"
      for (j in seq_along(columns)) {
        expect_identical(
          parts$data[[paste(prefix, shown[j])]][-1],
          expected[[columns[j]]]
        )
      }
    }
  }
  shared <- .comparison_build_instrument_table(comparison, 0.1, "shared")$data
  expect_identical(
    shared$`Previous Instrument Incomplete`,
    c("", "4/5 (80%)", "", "2/3 (66.7%)", "", "2/2 (100%)")
  )
  expect_identical(
    shared$`Current Instrument Incomplete`,
    c("", "2/5 (40%)", "", "1/3 (33.3%)", "", "1/2 (50%)")
  )
  expect_identical(shared$`Change Instrument Incomplete (pp)`[4], "-33.3 pp")
  expect_identical(shared$`Previous N (started/due)`[6], "1/2 (50%)")
  expect_identical(shared$`Current N (started/due)`[6], "2/2 (100%)")
})

test_that("event headers deduplicate records while All sums instrument opportunities", {
  reports <- comparison_reports_fixture(shared_event = TRUE)
  comparison <- compare_reports(reports$previous, reports$current)
  full <- .comparison_build_instrument_table(comparison, 0.1, "full")$data
  shared <- .comparison_build_instrument_table(comparison, 0.1, "shared")$data
  expect_identical(
    full$`Previous N (started/due)`[full$Event == "Baseline"],
    "3/3 (100%)"
  )
  expect_identical(
    full$`Current N (started/due)`[full$Event == "Baseline"],
    "4/4 (100%)"
  )
  expect_identical(
    shared$`Current N (started/due)`[shared$Event == "Baseline"],
    "3/3 (100%)"
  )
  expect_identical(
    full$`Previous Instrument Incomplete`[full$Event == "All"],
    "8/9 (88.9%)"
  )
  expect_identical(
    full$`Current Instrument Incomplete`[full$Event == "All"],
    "7/10 (70%)"
  )
  expect_identical(
    shared$`Current Instrument Incomplete`[shared$Event == "All"],
    "5/8 (62.5%)"
  )
})

test_that("comparison formatters handle labels rates empty populations and optional packages", {
  reports <- comparison_reports_fixture()
  comparison <- compare_reports(reports$previous, reports$current)
  skip_if_not_installed("flextable")
  skip_if_not_installed("glue")
  table <- flex_event_instruments(comparison)
  expect_s3_class(table, "flextable")
  expect_true(all(
    c("Full scope", "Shared targets") %in% table$body$dataset$Event
  ))
  expect_true(all(c("Visit", "Diary") %in% table$body$dataset$Instrument))
  summary <- get_summary(comparison, validation_check = "field-complete")
  selected <- summary[, c(
    "population",
    "instrument",
    "previous_fail_rate",
    "current_fail_rate",
    "delta_fail_rate"
  )]
  original <- selected
  formatted <- flexify(selected)
  expect_s3_class(formatted, "flextable")
  expect_identical(selected, original)
  expect_s3_class(flexify(get_changes(comparison)), "flextable")
  expect_error(
    flex_event_instruments(comparison, missing_threshold = 2),
    "missing_threshold"
  )
  expect_error(
    flex_event_instruments(comparison, population = "invalid"),
    "population"
  )
  testthat::local_mocked_bindings(.flex_require_packages = function(...) {
    stop("Optional packages unavailable")
  })
  expect_error(flex_event_instruments(comparison), "Optional packages")
  expect_error(flexify(summary), "Optional packages")
})

test_that("absent contexts display out of scope and disjoint targets stay separate", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(record_id = c("001", "002"))
  before_plan <- plan_explicit(data, rcon, run_plan_explicit_schedule("001"))
  after_plan <- plan_explicit(data, rcon, run_plan_explicit_schedule("002"))
  before <- run_plan(before_plan, data, rcon, details = TRUE, progress = FALSE)
  after <- run_plan(after_plan, data, rcon, details = TRUE, progress = FALSE)
  disjoint <- compare_reports(before, after)
  expect_true(
    "No shared targets" %in%
      .comparison_build_instrument_table(
        disjoint,
        0.1,
        c("full", "shared")
      )$data$Event
  )
  empty_plan <- plan_explicit(data, rcon, run_plan_explicit_schedule()[0, ])
  empty <- run_plan(empty_plan, data, rcon, details = TRUE, progress = FALSE)
  added <- compare_reports(empty, after)
  rows <- get_summary(added, population = "full")
  expect_true(all(!rows$previous_in_scope & rows$current_in_scope))
  expect_true(all(
    rows$previous_assessed == 0L &
      is.na(rows$previous_status) &
      is.na(rows$previous_fail_rate)
  ))
  display <- .comparison_build_instrument_table(added, 0.1, "full")$data
  expect_true("Out of scope" %in% display$`Previous Instrument Incomplete`)
  expect_identical(nrow(added$scope_changes), 1L)
  expect_identical(nrow(added$changes), 0L)
})
