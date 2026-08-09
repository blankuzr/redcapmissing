test_that("run-local response masks preserve response and checkbox semantics", {
  records <- tibble::tibble(
    ordinary = c(NA_character_, "", "   ", "NA", "entered"),
    numeric = c(NA_real_, NaN, Inf, -Inf, 0),
    checkbox = c(NA_character_, "", "0", "unchecked", "1")
  )
  masks <- .run_plan_response_masks_build(records)

  expect_identical(ls(masks$present), character())
  expect_identical(ls(masks$selected), character())
  expect_identical(
    .run_plan_response_masks_present(masks, "ordinary"),
    c(FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  expect_identical(
    .run_plan_response_masks_present(masks, "numeric"),
    c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    .run_plan_response_masks_selected(masks, "checkbox"),
    c(FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  expect_true(exists("ordinary", envir = masks$present, inherits = FALSE))
  expect_true(exists("numeric", envir = masks$present, inherits = FALSE))
  expect_true(exists("checkbox", envir = masks$selected, inherits = FALSE))
})

test_that("run_plan projects only after full preflight and keeps runtime fields", {
  rcon <- run_plan_rcon()
  planner_data <- run_plan_data()
  plan <- plan_from_data(planner_data, rcon, "baseline_form")
  runtime_data <- dplyr::mutate(planner_data, unrelated_extra = "unused")
  captured_columns <- NULL
  original_builder <- .run_plan_response_masks_build
  testthat::local_mocked_bindings(
    .run_plan_response_masks_build = function(records) {
      captured_columns <<- names(records)
      original_builder(records)
    },
    .package = "redcapmissing"
  )

  expect_s3_class(
    run_plan(plan, runtime_data, rcon, details = TRUE, progress = FALSE),
    "redcapmissing"
  )
  expect_false("unrelated_extra" %in% captured_columns)
  expect_true(all(c(
    ".rcm_record_id", "redcap_event_name", "redcap_repeat_instrument",
    "redcap_repeat_instance", "checkbox_field___1", "checkbox_field___2"
  ) %in% captured_columns))
})
