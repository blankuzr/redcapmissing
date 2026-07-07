skip_flex_event_forms_packages <- function() {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")
}

flex_event_forms_longitudinal_report <- function() {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y"),
    meta_row("survey_started", "survey_form", field_label = "Survey started", required = "y"),
    meta_row("survey_value", "survey_form", field_label = "Survey value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1, 1),
    unique_event_name = c(
      "event_1_arm_1",
      "event_1_arm_1",
      "event_2_arm_1",
      "event_2_arm_1"
    ),
    form = c("status_form", "survey_form", "status_form", "survey_form")
  )
  events <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("event_1_arm_1", "event_2_arm_1"),
    event_name = c("Baseline", "Follow-up"),
    custom_event_label = c("", "")
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
    status_value = c("entered", "", "entered", "entered"),
    survey_started = c("yes", "yes", "", "yes"),
    survey_value = c("entered", "entered", "", "")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(status_meta, events = events, mapping = mapping),
    forms = c("status_form", "survey_form")
  )
}

flex_event_forms_nonlongitudinal_report <- function() {
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

flex_event_forms_missing_event_report <- function() {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "closeout_event"),
    form = c("status_form", "status_form")
  )
  events <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "closeout_event"),
    event_name = c("Baseline", "Closeout"),
    custom_event_label = c("", "")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    redcap_event_name = c("baseline_event", "baseline_event"),
    status_started = c("yes", "yes"),
    status_value = c("entered", "entered")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(status_meta, events = events, mapping = mapping),
    forms = "status_form"
  )
}

flex_event_forms_repeat_report <- function() {
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
  events <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c("regular_a_arm_1", "repeat_b_arm_1", "regular_c_arm_1"),
    event_name = c("Regular A", "Repeat B", "Regular C"),
    custom_event_label = c("", "", "")
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
      events = events,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "mixed_form",
    instances = 2L
  )
}

flex_event_forms_custom_id_report <- function() {
  status_meta <- dplyr::bind_rows(
    meta_row("study_id", "status_form", field_label = "Study ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("event_1_arm_1", "event_2_arm_1"),
    form = c("status_form", "status_form")
  )
  events <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("event_1_arm_1", "event_2_arm_1"),
    event_name = c("Baseline", "Follow-up"),
    custom_event_label = c("", "")
  )
  records <- tibble::tibble(
    study_id = c("r1", "r1", "r2", "r2"),
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

flex_event_forms_custom_id_repeat_report <- function() {
  mixed_meta <- dplyr::bind_rows(
    meta_row("study_id", "mixed_form", field_label = "Study ID", required = "y"),
    meta_row("mixed_started", "mixed_form", field_label = "Mixed started", required = "y"),
    meta_row("mixed_value", "mixed_form", field_label = "Mixed value", required = "y"),
    meta_row("mixed_other", "mixed_form", field_label = "Mixed other", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("regular_a_arm_1", "repeat_b_arm_1"),
    form = c("mixed_form", "mixed_form")
  )
  events <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("regular_a_arm_1", "repeat_b_arm_1"),
    event_name = c("Regular A", "Repeat B"),
    custom_event_label = c("", "")
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "repeat_b_arm_1",
    form_name = "mixed_form",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    study_id = c("r1", "r2", "r1", "r2"),
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
      events = events,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "mixed_form",
    instances = 2L
  )
}

test_that("flex_event_forms rejects invalid inputs", {
  expect_error(
    flex_event_forms(list()),
    "no applicable method"
  )
})

test_that("flex_event_forms reduces longitudinal forms under event rows", {
  skip_flex_event_forms_packages()

  flex_out <- flex_event_forms(flex_event_forms_longitudinal_report())
  body <- tibble::as_tibble(flex_out$body$dataset)

  expect_s3_class(flex_out, "flextable")
  expect_identical(names(body), c(
    "Event",
    "Form",
    "N",
    "Form Complete",
    "Field-Complete Fails"
  ))
  expect_equal(
    body,
    tibble::tibble(
      Event = c("Total N", "Baseline", "", "", "Follow-up", "", ""),
      Form = c(
        "",
        "",
        "status_form label",
        "survey_form label",
        "",
        "status_form label",
        "survey_form label"
      ),
      N = c("2", "2", "", "", "2", "", ""),
      `Form Complete` = c("", "", "2 (100%)", "1 (50%)", "", "1 (50%)", "1 (50%)"),
      `Field-Complete Fails` = c(
        "",
        "",
        "0 (0% of 6 fields)",
        "0 (0% of 2 fields)",
        "",
        "1 (16.7% of 6 fields)",
        "1 (25% of 4 fields)"
      )
    )
  )
})

test_that("flex_event_forms counts reports whose REDCap ID field is not record_id", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_custom_id_report()
  tidy_out <- tidy(report)
  flex_body <- tibble::as_tibble(flex(report)$body$dataset)
  event_form_body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)

  expect_equal(report$id_col, "study_id")
  expect_true("record_id" %in% names(report$validation_rows))
  expect_false("study_id" %in% names(report$validation_rows))
  expect_true(nrow(tidy_out) > 0)
  expect_true(any(tidy_out$passed > 0))
  expect_true(nrow(flex_body) > 0)
  expect_true(any(flex_body$Assessed > 0))
  expect_true(any(flex_body$Form == "status_form label"))

  expect_equal(
    event_form_body,
    tibble::tibble(
      Event = c("Total N", "Baseline", "", "Follow-up", ""),
      Form = c("", "", "status_form label", "", "status_form label"),
      N = c("2", "2", "", "2", ""),
      `Form Complete` = c("", "", "2 (100%)", "", "1 (50%)"),
      `Field-Complete Fails` = c(
        "",
        "",
        "0 (0% of 6 fields)",
        "",
        "1 (16.7% of 6 fields)"
      )
    )
  )
})

test_that("flex_event_forms repeat denominators use normalized record_id", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_custom_id_repeat_report()
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)

  expect_equal(report$id_col, "study_id")
  expect_true("Repeat Instrument" %in% names(body))
  expect_equal(body$N[body$Event == "Total N"], "2")
  expect_equal(body$N[body$Event == "Regular A"], "2")
  expect_equal(body$N[body$Event == "Repeat B"], "2")

  repeat_instance_one <- body[
    body$Event == "" &
      body$`Repeat Instrument` == "mixed_form label" &
      body$`Repeat Instance` == "1",
  ]
  repeat_instance_two <- body[
    body$Event == "" &
      body$`Repeat Instrument` == "mixed_form label" &
      body$`Repeat Instance` == "2",
  ]

  expect_equal(repeat_instance_one$N, "2")
  expect_equal(repeat_instance_one$`Form Complete`, "1 (50%)")
  expect_equal(repeat_instance_two$N, "0")
  expect_equal(repeat_instance_two$`Form Complete`, "0 (0%)")
})

test_that("flex_event_forms renders single-event reports with form rows", {
  skip_flex_event_forms_packages()

  flex_out <- flex_event_forms(flex_event_forms_nonlongitudinal_report())
  body <- tibble::as_tibble(flex_out$body$dataset)

  expect_equal(
    body,
    tibble::tibble(
      Event = c("Total N", "Single event", ""),
      Form = c("", "", "baseline_form label"),
      N = c("1", "1", ""),
      `Form Complete` = c("", "", "0 (0%)"),
      `Field-Complete Fails` = c("", "", "1 (25% of 4 fields)")
    )
  )
})

test_that("flex_event_forms collapses missing events to event rows", {
  skip_flex_event_forms_packages()

  flex_out <- flex_event_forms(flex_event_forms_missing_event_report())
  body <- tibble::as_tibble(flex_out$body$dataset)

  expect_equal(body$Event, c("Total N", "Baseline", "", "Closeout"))
  expect_equal(body$N, c("2", "2", "", "0"))
  expect_false(any(body$Form[body$Event == "Closeout"] != ""))
  expect_equal(body$`Field-Complete Fails`[[3]], "0 (0% of 6 fields)")
})

test_that("flex_event_forms includes repeat context and zero-denominator rows", {
  skip_flex_event_forms_packages()

  flex_out <- flex_event_forms(flex_event_forms_repeat_report())
  body <- tibble::as_tibble(flex_out$body$dataset)

  expect_true("Repeat Instrument" %in% names(body))
  expect_true("Repeat Instance" %in% names(body))
  expect_true(any(body$`Repeat Instrument` == "mixed_form label"))
  expect_true(any(body$`Repeat Instance` == "2"))

  repeat_instance_two <- body[
    body$Event == "" &
      body$`Repeat Instrument` == "mixed_form label" &
      body$`Repeat Instance` == "2",
  ]
  expect_equal(repeat_instance_two$N, "0")
  expect_equal(repeat_instance_two$`Form Complete`, "0 (0%)")
  expect_equal(repeat_instance_two$`Field-Complete Fails`, "0 (0% of 0 fields)")
})

test_that("flex_event_forms applies event and form row styling", {
  skip_flex_event_forms_packages()

  flex_out <- flex_event_forms(flex_event_forms_longitudinal_report())
  bold_data <- flex_out$body$styles$text$bold$data
  padding_data <- flex_out$body$styles$pars$padding.left$data

  expect_true(all(as.matrix(bold_data[c(1, 2, 5), ])))
  expect_false(any(as.matrix(bold_data[c(3, 4, 6, 7), ])))
  expect_true(all(padding_data[c(3, 4, 6, 7), "Form"] > padding_data[c(1, 2, 5, 1), "Form"]))
})

test_that("flex_event_forms output renders through flex_html", {
  skip_flex_event_forms_packages()
  testthat::skip_if_not_installed("htmltools")

  html_out <- flex_html(flex_event_forms(flex_event_forms_nonlongitudinal_report()))

  expect_type(html_out, "character")
  expect_true(grepl("baseline_form label", html_out, fixed = TRUE))
})

test_that("flex_event_forms rendered HTML includes custom-ID labels and counts", {
  skip_flex_event_forms_packages()
  testthat::skip_if_not_installed("htmltools")

  html_out <- flex_html(flex_event_forms(flex_event_forms_custom_id_report()))

  expect_type(html_out, "character")
  expect_true(grepl("Baseline", html_out, fixed = TRUE))
  expect_true(grepl("Follow-up", html_out, fixed = TRUE))
  expect_true(grepl("status_form label", html_out, fixed = TRUE))
  expect_true(grepl("2 (100%)", html_out, fixed = TRUE))
  expect_true(grepl("1 (50%)", html_out, fixed = TRUE))
  expect_true(grepl("1 (16.7% of 6 fields)", html_out, fixed = TRUE))
})
