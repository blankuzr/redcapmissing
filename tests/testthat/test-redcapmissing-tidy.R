tidy_baseline_report <- function() {
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
    form = "baseline_form"
  )
}

tidy_expected_columns <- function() {
  c(
    "validation_id",
    "validation",
    "validation_context",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "all_passed",
    "assessed",
    "passed",
    "failed",
    "pass_rate",
    "fail_rate"
  )
}

test_that("tidy returns a focused validation-summary tibble", {
  report <- tidy_baseline_report()

  tidy_tbl <- tidy(report)
  validation_set <- report$agent$validation_set

  expect_s3_class(tidy_tbl, "tbl_df")
  expect_false(inherits(tidy_tbl, "summary.redcapmissing"))
  expect_identical(names(tidy_tbl), tidy_expected_columns())
  expect_identical(tidy_tbl$validation_id, validation_set$step_id)
  expect_identical(tidy_tbl$validation, validation_set$label)
  expect_identical(tidy_tbl$validation_context, validation_set$validation_context)
  expect_identical(tidy_tbl$redcap_event_name, validation_set$redcap_event_name)
  expect_identical(
    tidy_tbl$redcap_repeat_instrument,
    validation_set$redcap_repeat_instrument
  )
  expect_identical(
    tidy_tbl$redcap_repeat_instance,
    validation_set$redcap_repeat_instance
  )
  expect_identical(tidy_tbl$all_passed, validation_set$all_passed)
  expect_equal(tidy_tbl$assessed, validation_set$n)
  expect_equal(tidy_tbl$passed, validation_set$n_passed)
  expect_equal(tidy_tbl$failed, validation_set$n_failed)
  expect_equal(tidy_tbl$pass_rate, validation_set$f_passed)
  expect_equal(tidy_tbl$fail_rate, validation_set$f_failed)
  expect_false(any(c(
    "sha1",
    "column",
    "values",
    "tbl_checked",
    "time_processed"
  ) %in% names(tidy_tbl)))
})

test_that("tidy dispatch is available from redcapmissing", {
  report <- tidy_baseline_report()

  expect_true("tidy" %in% getNamespaceExports("redcapmissing"))
  expect_identical(tidy(report), generics::tidy(report))
})

test_that("tidy preserves zero-denominator rows with zero fractions", {
  report <- tidy_baseline_report()

  event_summary <- tidy(report)[
    tidy(report)$validation_id == "baseline_form_event_row_missing",
    ,
    drop = FALSE
  ]

  expect_equal(event_summary$assessed, 0)
  expect_equal(event_summary$passed, 0)
  expect_equal(event_summary$failed, 0)
  expect_equal(event_summary$pass_rate, 0)
  expect_equal(event_summary$fail_rate, 0)
  expect_equal(event_summary$validation_context, "overall")
})

test_that("tidy returns multi-event context denominators", {
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

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    form = "status_form"
  )
  tidy_tbl <- tidy(report)

  event_summary <- tidy_tbl[
    tidy_tbl$validation_id == "status_form_event_row_missing",
    ,
    drop = FALSE
  ]
  event_summary <- event_summary[
    order(event_summary$redcap_event_name),
    ,
    drop = FALSE
  ]
  field_summary <- tidy_tbl[
    tidy_tbl$validation_id == "status_form_missing_fields",
    ,
    drop = FALSE
  ]
  field_summary <- field_summary[
    order(field_summary$redcap_event_name),
    ,
    drop = FALSE
  ]

  expect_equal(
    event_summary$validation_context,
    paste0("event: event_", 1:3, "_arm_1")
  )
  expect_equal(event_summary$assessed, c(2, 2, 2))
  expect_equal(event_summary$failed, c(0, 0, 2))
  expect_equal(event_summary$pass_rate, c(1, 1, 0))
  expect_equal(field_summary$assessed, c(6, 6))
  expect_equal(field_summary$failed, c(0, 1))
})

test_that("tidy returns repeating context denominators", {
  repeat_meta <- dplyr::bind_rows(
    meta_row("record_id", "screen_form", field_label = "Record ID", required = "y"),
    meta_row("screen_started", "screen_form", field_label = "Screen started", required = "y"),
    meta_row("repeat_started", "repeat_form", field_label = "Repeat started", required = "y"),
    meta_row("repeat_value", "repeat_form", field_label = "Repeat value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "baseline_event"),
    form = c("screen_form", "repeat_form")
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "baseline_event",
    form_name = "repeat_form",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2", "r2"),
    redcap_event_name = c("baseline_event", "baseline_event", "baseline_event"),
    redcap_repeat_instrument = c("repeat_form", "repeat_form", "repeat_form"),
    redcap_repeat_instance = c("1", "1", "2"),
    repeat_started = c("yes", "yes", "yes"),
    repeat_value = c("10", "20", "30")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      repeat_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    form = "repeat_form",
    instances = 2L
  )
  repeat_summary <- tidy(report)[
    tidy(report)$validation_id == "repeat_form_repeat_instance_missing",
    ,
    drop = FALSE
  ]
  repeat_summary <- repeat_summary[
    order(repeat_summary$redcap_repeat_instance),
    ,
    drop = FALSE
  ]

  expect_equal(repeat_summary$validation_context, c(
    "event: baseline_event; repeat: 1",
    "event: baseline_event; repeat: 2"
  ))
  expect_equal(repeat_summary$redcap_repeat_instance, c("1", "2"))
  expect_equal(repeat_summary$assessed, c(2, 2))
  expect_equal(repeat_summary$failed, c(0, 1))
})

test_that("tidy rejects invalid inputs and malformed validation sets", {
  expect_error(
    redcapmissing:::tidy.redcapmissing(list()),
    "`x` must be a `redcapmissing` object"
  )

  broken_report <- structure(
    list(agent = list(validation_set = tibble::tibble(step_id = "step"))),
    class = "redcapmissing"
  )

  expect_error(
    redcapmissing:::tidy.redcapmissing(broken_report),
    "current validation summary columns"
  )
})

test_that("summary redcapmissing methods are not registered", {
  expect_null(getS3method("summary", "redcapmissing", optional = TRUE))
  expect_null(getS3method("print", "summary.redcapmissing", optional = TRUE))
  expect_false(exists(
    "summary.redcapmissing",
    envir = asNamespace("redcapmissing"),
    inherits = FALSE
  ))
})
