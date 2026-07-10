skip_flex_packages <- function() {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")
}

flex_baseline_report <- function() {
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    required_note = "",
    optional_note = "",
    checkbox_field___1 = "1",
    checkbox_field___2 = "0",
    checkbox_other = "",
    conditional_note = ""
  )

  find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form"
  )
}

flex_event_report <- function() {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c("event_1_arm_1", "event_2_arm_1", "event_3_arm_1"),
    form = c("status_form", "status_form", "status_form")
  )
  events <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c("event_1_arm_1", "event_2_arm_1", "event_3_arm_1"),
    event_name = c("Baseline", "Follow-up", "Closeout"),
    custom_event_label = c("Custom baseline", "Custom follow-up", "")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1", "r2", "r2"),
    redcap_event_name = c(
      "event_1_arm_1",
      "event_2_arm_1",
      "event_1_arm_1",
      "event_2_arm_1"
    ),
    status_started = c("yes", "yes", "yes", "yes"),
    status_value = c("entered", "", "entered", "entered")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(status_meta, events = events, mapping = mapping),
    forms = "status_form"
  )
}

flex_mixed_repeat_report <- function() {
  mixed_meta <- dplyr::bind_rows(
    meta_row("record_id", "mixed_form", field_label = "Record ID", required = "y"),
    meta_row("mixed_started", "mixed_form", field_label = "Mixed started", required = "y"),
    meta_row("mixed_value", "mixed_form", field_label = "Mixed value", required = "y"),
    meta_row("mixed_other", "mixed_form", field_label = "Mixed other", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c("regular_a_arm_1", "repeat_b_arm_1", "regular_c_arm_1"),
    form = c("mixed_form", "mixed_form", "mixed_form")
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "repeat_b_arm_1",
    form_name = "mixed_form",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2", "r1", "r2"),
    redcap_event_name = c(
      "regular_a_arm_1",
      "regular_a_arm_1",
      "repeat_b_arm_1",
      "repeat_b_arm_1"
    ),
    redcap_repeat_instrument = c("", "", "mixed_form", "mixed_form"),
    redcap_repeat_instance = c("", "", "1", "1"),
    mixed_started = c("yes", "", "yes", "yes"),
    mixed_value = c("", "", "entered", "entered"),
    mixed_other = c("entered", "", "", "entered")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(
      mixed_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "mixed_form",
    instances = 2L
  )
}

test_that("flex rejects invalid inputs", {
  expect_error(
    flex(list()),
    "no applicable method"
  )
})

test_that("flex returns the default formatted table", {
  skip_flex_packages()

  report <- flex_baseline_report()

  flex_out <- flex(report)
  tidy_tbl <- tidy(report)

  expect_s3_class(flex_out, "flextable")
  expect_identical(names(flex_out$body$dataset), c(
    "Event",
    "Form",
    "Validation Check",
    "Assessed",
    "Passed",
    "Failed"
  ))
  expect_equal(flex_out$body$dataset$Assessed, tidy_tbl$assessed)
  expect_true(any(flex_out$body$dataset$Form == "baseline_form label"))
  expect_false(any(flex_out$body$dataset$`Validation Check` == "Event complete"))
  expect_true(any(
    flex_out$body$dataset$`Validation Check` == "Event row started" &
      flex_out$body$dataset$Passed == "0 (0%)" &
      flex_out$body$dataset$Failed == "0 (0%)"
  ))
  expect_false("Form Label" %in% names(flex_out$body$dataset))
  expect_false("Validation Level" %in% names(flex_out$body$dataset))
  expect_false("Check Type" %in% names(flex_out$body$dataset))
})

test_that("flex labels forms in multi-form summaries", {
  skip_flex_packages()

  metadata <- dplyr::bind_rows(
    meta_row("record_id", "alpha_form", field_label = "Record ID", required = "y"),
    meta_row("alpha_value", "alpha_form", field_label = "Alpha value", required = "y"),
    meta_row("beta_value", "beta_form", field_label = "Beta value", required = "y")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    alpha_value = c("entered", ""),
    beta_value = c("", "entered")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = c("alpha_form", "beta_form")
  )
  flex_out <- flex(report)

  expect_s3_class(flex_out, "flextable")
  expect_setequal(
    flex_out$body$dataset$Form,
    c("alpha_form label", "beta_form label")
  )
})

test_that("flex filters by raw events, forms, and validation checks before applying labels", {
  skip_flex_packages()

  report <- flex_event_report()

  event_flex <- flex(report, events = "event_2_arm_1")
  expect_setequal(event_flex$body$dataset$Event, "Follow-up")
  expect_false(any(event_flex$body$dataset$Event == "Custom follow-up"))

  form_flex <- flex(report, forms = "status_form")
  expect_false(any(form_flex$body$dataset$`Validation Check` == "Event complete"))
  expect_setequal(form_flex$body$dataset$Form, "status_form label")

  check_flex <- flex(report, validation_check = "field-complete")
  expect_setequal(check_flex$body$dataset$`Validation Check`, "Field complete")
  expect_equal(
    nrow(check_flex$body$dataset),
    sum(tidy(report)$validation_check == "field-complete")
  )

  multi_check_flex <- flex(
    report,
    validation_check = c("event-row-started", "form-started")
  )
  expect_setequal(
    multi_check_flex$body$dataset$`Validation Check`,
    c("Event row started", "Form started")
  )

  intersect_flex <- flex(
    report,
    events = "event_2_arm_1",
    forms = "status_form",
    validation_check = "field-complete"
  )
  expect_setequal(intersect_flex$body$dataset$Event, "Follow-up")
  expect_setequal(intersect_flex$body$dataset$Form, "status_form label")
  expect_setequal(intersect_flex$body$dataset$`Validation Check`, "Field complete")
})

test_that("flex errors on invalid filters and empty intersections", {
  skip_flex_packages()

  report <- flex_event_report()
  base_report <- flex_baseline_report()

  expect_error(
    flex(report, forms = "unknown_form"),
    "Unknown form"
  )
  expect_error(
    flex(report, events = "unknown_event"),
    "Unknown event"
  )
  expect_error(
    flex(report, forms = character()),
    "at least one non-blank"
  )
  expect_error(
    flex(report, validation_check = 1),
    "`validation_check` must be a character vector"
  )
  expect_error(
    flex(report, validation_check = character()),
    "at least one non-blank"
  )
  expect_error(
    flex(report, validation_check = "unknown-check"),
    "Unknown validation check"
  )
  expect_error(
    flex(report, validation_check = "instance-row-started"),
    "Unknown validation check"
  )

  inconsistent_report <- base_report
  inconsistent_report$spec$forms <- "not_in_tidy"
  expect_error(
    flex(inconsistent_report, forms = "not_in_tidy"),
    "Unknown form"
  )

  metadata <- dplyr::bind_rows(
    meta_row("record_id", "alpha_form", field_label = "Record ID", required = "y"),
    meta_row("alpha_value", "alpha_form", field_label = "Alpha value", required = "y"),
    meta_row("beta_value", "beta_form", field_label = "Beta value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "followup_event"),
    form = c("alpha_form", "beta_form")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1"),
    redcap_event_name = c("baseline_event", "followup_event"),
    alpha_value = c("entered", ""),
    beta_value = c("", "entered")
  )
  split_event_report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata, mapping = mapping),
    forms = c("alpha_form", "beta_form")
  )

  expect_error(
    flex(
      split_event_report,
      events = "followup_event",
      forms = "alpha_form",
      validation_check = "field-complete"
    ),
    "produced no validation rows"
  )
})

test_that("flex repeat columns follow the filtered rows", {
  skip_flex_packages()

  report <- flex_mixed_repeat_report()

  full_flex <- flex(report)
  expect_true("Repeat Instrument" %in% names(full_flex$body$dataset))
  expect_true("Repeat Instance" %in% names(full_flex$body$dataset))
  expect_true(any(full_flex$body$dataset$`Repeat Instrument` == "mixed_form label"))

  regular_event_flex <- flex(report, events = "regular_a_arm_1")
  expect_false("Repeat Instrument" %in% names(regular_event_flex$body$dataset))
  expect_false("Repeat Instance" %in% names(regular_event_flex$body$dataset))

  repeat_check_flex <- flex(report, validation_check = "instance-row-started")
  expect_true("Repeat Instrument" %in% names(repeat_check_flex$body$dataset))
  expect_true("Repeat Instance" %in% names(repeat_check_flex$body$dataset))

  nonrepeat_check_flex <- flex(report, validation_check = "event-row-started")
  expect_false("Repeat Instrument" %in% names(nonrepeat_check_flex$body$dataset))
  expect_false("Repeat Instance" %in% names(nonrepeat_check_flex$body$dataset))
})

test_that("flex no longer accepts summary objects", {
  skip_flex_packages()

  summary_tbl <- tibble::tibble(
    label = "Legacy validation",
    n = 0,
    n_passed = 0,
    n_failed = 0,
    f_passed = 0,
    f_failed = 0
  )
  class(summary_tbl) <- c("summary.redcapmissing", class(summary_tbl))

  expect_error(
    flex(summary_tbl),
    "no applicable method"
  )
})
