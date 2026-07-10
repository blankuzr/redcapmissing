skip_flex_event_forms_packages <- function() {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")
}

flex_event_forms_longitudinal_report <- function(details = FALSE) {
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
    forms = c("status_form", "survey_form"),
    details = details
  )
}

flex_event_forms_partly_started_event_report <- function() {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "followup_event"),
    form = c("status_form", "status_form")
  )
  events <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "followup_event"),
    event_name = c("Baseline", "Follow-up"),
    custom_event_label = c("", "")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2", "r1"),
    redcap_event_name = c("baseline_event", "baseline_event", "followup_event"),
    status_started = c("yes", "yes", "yes"),
    status_value = c("entered", "entered", "entered")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(status_meta, events = events, mapping = mapping),
    forms = "status_form",
    details = TRUE
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

flex_event_forms_no_assessed_fields_report <- function() {
  conditional_meta <- dplyr::bind_rows(
    meta_row("record_id", "conditional_form", field_label = "Record ID"),
    meta_row(
      "branch_flag",
      "conditional_form",
      field_type = "yesno",
      field_label = "Branch flag"
    ),
    meta_row(
      "conditional_note",
      "conditional_form",
      field_label = "Conditional note",
      branching = "[branch_flag] = '1'",
      required = "y"
    )
  )
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    conditional_note = ""
  )

  find_missing(
    data = records,
    rcon = fake_rcon(conditional_meta),
    forms = "conditional_form",
    details = TRUE
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
    forms = "status_form",
    details = TRUE
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
    instances = 2L,
    details = TRUE
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
    forms = "status_form",
    details = TRUE
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
    instances = 2L,
    details = TRUE
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
    "N (started/due)",
    "Form Incomplete"
  ))
  expect_equal(
    body,
    tibble::tibble(
      Event = c("All", "Baseline", "", "", "Follow-up", "", ""),
      Form = c(
        "",
        "",
        "status_form label",
        "survey_form label",
        "",
        "status_form label",
        "survey_form label"
      ),
      `N (started/due)` = c("", "2/2 (100%)", "", "", "2/2 (100%)", "", ""),
      `Form Incomplete` = c(
        "3/8 (37.5%)",
        "",
        "0 (0%)",
        "1 (50%)",
        "",
        "1 (50%)",
        "1 (50%)"
      )
    )
  )
})

test_that("flex_event_forms event headers follow tidy summaries over raw checks", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_longitudinal_report(details = TRUE)
  raw_checks <- report$details$checks[["event-row-started"]]
  report$details$checks[["event-row-started"]] <- raw_checks[
    raw_checks$redcap_event_name != "event_2_arm_1" |
      raw_checks$record_id != "r2",
    ,
    drop = FALSE
  ]

  tidy_event <- tidy(report)
  tidy_event <- tidy_event[
    tidy_event$validation_check == "event-row-started" &
      tidy_event$redcap_event_name == "event_2_arm_1",
    c("passed", "assessed"),
    drop = FALSE
  ]
  tidy_event <- unique(tidy_event)
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)
  tidy_event_pct <- round(
    (tidy_event$passed[[1]] / tidy_event$assessed[[1]]) * 100,
    1
  )
  tidy_event_n <- paste0(
    tidy_event$passed[[1]],
    "/",
    tidy_event$assessed[[1]],
    " (",
    tidy_event_pct,
    "%)"
  )

  expect_equal(nrow(tidy_event), 1)
  expect_equal(
    body[body$Event == "Follow-up", "N (started/due)", drop = TRUE],
    tidy_event_n
  )
})

test_that("flex_event_forms form-incomplete percentage uses context assessed denominator", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_partly_started_event_report()
  summary_out <- tidy(report)
  followup_event <- summary_out[
    summary_out$validation_check == "event-row-started" &
      summary_out$redcap_event_name == "followup_event",
    ,
    drop = FALSE
  ]
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)
  followup_row <- which(body$Event == "Follow-up")
  followup_form_row <- followup_row + 1L

  expect_equal(followup_event$passed, 1L)
  expect_equal(followup_event$assessed, 2L)
  expect_equal(body[["N (started/due)"]][[followup_row]], "1/2 (50%)")
  expect_equal(body$Form[[followup_form_row]], "status_form label")
  expect_equal(body$`Form Incomplete`[[followup_form_row]], "1 (50%)")
})

test_that("flex_event_forms form denominators follow exact form context", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_longitudinal_report()
  status_followup <- report$summary$validation_check == "event-row-started" &
    report$summary$redcap_event_name == "event_2_arm_1" &
    report$summary$form == "status_form"
  report$summary$assessed[status_followup] <- 1L
  report$summary$passed[status_followup] <- 1L
  report$summary$failed[status_followup] <- 0L
  report$summary$pass_rate[status_followup] <- 1
  report$summary$fail_rate[status_followup] <- 0

  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)
  followup_row <- which(body$Event == "Follow-up")
  status_form_row <- followup_row + 1L
  survey_form_row <- followup_row + 2L

  expect_equal(body$`Form Incomplete`[[1]], "3/7 (42.9%)")
  expect_equal(body[["N (started/due)"]][[followup_row]], "2/2 (100%)")
  expect_equal(body$Form[[status_form_row]], "status_form label")
  expect_equal(body$`Form Incomplete`[[status_form_row]], "1 (100%)")
  expect_equal(body$Form[[survey_form_row]], "survey_form label")
  expect_equal(body$`Form Incomplete`[[survey_form_row]], "1 (50%)")
})

test_that("flex_event_forms requires exact event-row-started denominator rows", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_longitudinal_report()
  status_followup <- report$summary$validation_check == "event-row-started" &
    report$summary$redcap_event_name == "event_2_arm_1" &
    report$summary$form == "status_form"
  report$summary <- report$summary[!status_followup, , drop = FALSE]

  expect_error(
    flex_event_forms(report),
    "could not find exact `event-row-started` summary.*event `event_2_arm_1`, form `status_form`"
  )
})

test_that("flex_event_forms does not fall back from invalid event-row-started denominators", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_longitudinal_report()
  status_followup <- report$summary$validation_check == "event-row-started" &
    report$summary$redcap_event_name == "event_2_arm_1" &
    report$summary$form == "status_form"
  report$summary$assessed[status_followup] <- 0L
  report$summary$passed[status_followup] <- 0L
  report$summary$failed[status_followup] <- 0L
  report$summary$pass_rate[status_followup] <- 0
  report$summary$fail_rate[status_followup] <- 0

  expect_error(
    flex_event_forms(report),
    "invalid `event-row-started` assessed N.*event `event_2_arm_1`, form `status_form`"
  )
})

test_that("flex_event_forms requires exact instance-row-started denominator rows", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_repeat_report()
  repeat_instance_one <- report$summary$validation_check == "instance-row-started" &
    report$summary$redcap_event_name == "repeat_b_arm_1" &
    report$summary$form == "mixed_form" &
    report$summary$redcap_repeat_instrument == "mixed_form" &
    report$summary$redcap_repeat_instance == "1"
  report$summary <- report$summary[!repeat_instance_one, , drop = FALSE]

  expect_error(
    flex_event_forms(report),
    "could not find exact `instance-row-started` summary.*repeat instance `1`"
  )
})

test_that("flex_event_forms does not fall back from invalid instance-row-started denominators", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_repeat_report()
  repeat_instance_one <- report$summary$validation_check == "instance-row-started" &
    report$summary$redcap_event_name == "repeat_b_arm_1" &
    report$summary$form == "mixed_form" &
    report$summary$redcap_repeat_instrument == "mixed_form" &
    report$summary$redcap_repeat_instance == "1"
  report$summary$assessed[repeat_instance_one] <- 0L
  report$summary$passed[repeat_instance_one] <- 0L
  report$summary$failed[repeat_instance_one] <- 0L
  report$summary$pass_rate[repeat_instance_one] <- 0
  report$summary$fail_rate[repeat_instance_one] <- 0

  expect_error(
    flex_event_forms(report),
    "invalid `instance-row-started` assessed N.*repeat instance `1`"
  )
})

test_that("flex_event_forms errors on conflicting event-row-started summaries", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_longitudinal_report()
  validation_set <- report$summary
  conflict_source <- which(
    validation_set$validation_check == "event-row-started" &
      validation_set$redcap_event_name == "event_2_arm_1"
  )[[1]]
  conflict_row <- validation_set[conflict_source, , drop = FALSE]
  conflict_row$passed <- conflict_row$passed - 1L
  report$summary <- rbind(validation_set, conflict_row)

  expect_error(
    flex_event_forms(report),
    "conflicting `event-row-started` summaries.*event_2_arm_1"
  )
})

test_that("flex_event_forms counts reports whose REDCap ID field is not record_id", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_custom_id_report()
  tidy_out <- tidy(report)
  flex_body <- tibble::as_tibble(flex(report)$body$dataset)
  event_form_body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)

  expect_equal(report$spec$id_col, "study_id")
  expect_true("record_id" %in% names(report$details$validation_rows))
  expect_false("study_id" %in% names(report$details$validation_rows))
  expect_true(nrow(tidy_out) > 0)
  expect_true(any(tidy_out$passed > 0))
  expect_true(nrow(flex_body) > 0)
  expect_true(any(flex_body$Assessed > 0))
  expect_true(any(flex_body$Form == "status_form label"))

  expect_equal(
    event_form_body,
    tibble::tibble(
      Event = c("All", "Baseline", "", "Follow-up", ""),
      Form = c("", "", "status_form label", "", "status_form label"),
      `N (started/due)` = c("", "2/2 (100%)", "", "2/2 (100%)", ""),
      `Form Incomplete` = c("1/4 (25%)", "", "0 (0%)", "", "1 (50%)")
    )
  )
})

test_that("flex_event_forms repeat denominators use normalized record_id", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_custom_id_repeat_report()
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)

  expect_equal(report$spec$id_col, "study_id")
  expect_true("Repeat Instrument" %in% names(body))
  expect_equal(body$Event[[1]], "All")
  expect_equal(body$Form[[1]], "")
  expect_equal(body$`Repeat Instrument`[[1]], "")
  expect_equal(body$`Repeat Instance`[[1]], "")
  expect_equal(body$`Form Incomplete`[[1]], "5/6 (83.3%)")
  expect_equal(body[body$Event == "Regular A", "N (started/due)", drop = TRUE], "2/2 (100%)")
  expect_equal(body[body$Event == "Repeat B", "N (started/due)", drop = TRUE], "2/2 (100%)")

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

  expect_equal(repeat_instance_one[["N (started/due)"]], "2/2 (100%)")
  expect_equal(repeat_instance_one$`Form Incomplete`, "1 (50%)")
  expect_equal(repeat_instance_two[["N (started/due)"]], "0/2 (0%)")
  expect_equal(repeat_instance_two$`Form Incomplete`, "2 (100%)")
})

test_that("flex_event_forms renders single-event reports with form rows", {
  skip_flex_event_forms_packages()

  flex_out <- flex_event_forms(flex_event_forms_nonlongitudinal_report())
  body <- tibble::as_tibble(flex_out$body$dataset)

  expect_equal(
    body,
    tibble::tibble(
      Event = c("All", "Single event", ""),
      Form = c("", "", "baseline_form label"),
      `N (started/due)` = c("", "1/1 (100%)", ""),
      `Form Incomplete` = c("1/1 (100%)", "", "1 (100%)")
    )
  )
})

test_that("flex_event_forms counts each failed record context once", {
  skip_flex_event_forms_packages()

  multi_field_meta <- dplyr::bind_rows(
    meta_row("record_id", "multi_form", field_label = "Record ID", required = "y"),
    meta_row("started", "multi_form", field_label = "Started", required = "y"),
    meta_row("field_a", "multi_form", field_label = "Field A", required = "y"),
    meta_row("field_b", "multi_form", field_label = "Field B", required = "y")
  )
  records <- tibble::tibble(
    record_id = "r1",
    started = "yes",
    field_a = "",
    field_b = ""
  )
  report <- find_missing(
    data = records,
    rcon = fake_rcon(multi_field_meta),
    forms = "multi_form"
  )

  expect_gt(
    sum(
      report$missing$record_id == "r1" &
        report$missing$form == "multi_form" &
        report$missing$validation_check == "field-complete"
    ),
    1
  )

  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)
  form_row <- body[body$Form == "multi_form label", , drop = FALSE]

  expect_equal(form_row$`Form Incomplete`, "1 (100%)")
  expect_equal(body$`Form Incomplete`[[1]], "1/1 (100%)")
})

test_that("flex_event_forms requires positive total N for single-event reports", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_nonlongitudinal_report()
  report$spec$total_n <- 0L

  expect_error(
    flex_event_forms(report),
    "positive `x\\$spec\\$total_n`"
  )
})

test_that("flex_event_forms does not count unassessed forms as incomplete", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_no_assessed_fields_report()
  summary_out <- tidy(report)
  form_started <- summary_out[
    summary_out$validation_check == "form-started",
    ,
    drop = FALSE
  ]
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)

  expect_equal(form_started$passed, 1L)
  expect_equal(form_started$failed, 0L)
  expect_equal(
    body,
    tibble::tibble(
      Event = c("All", "Single event", ""),
      Form = c("", "", "conditional_form label"),
      `N (started/due)` = c("", "1/1 (100%)", ""),
      `Form Incomplete` = c("0/1 (0%)", "", "0 (0%)")
    )
  )
})

test_that("flex_event_forms shows forms for missing events", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_missing_event_report()
  summary_out <- tidy(report)
  closeout_event <- summary_out[
    summary_out$validation_check == "event-row-started" &
      summary_out$redcap_event_name == "closeout_event",
    ,
    drop = FALSE
  ]
  closeout_forms <- summary_out[
    summary_out$validation_check == "form-started" &
      summary_out$redcap_event_name == "closeout_event",
    ,
    drop = FALSE
  ]
  flex_out <- flex_event_forms(report)
  body <- tibble::as_tibble(flex_out$body$dataset)

  expect_equal(nrow(closeout_event), 1)
  expect_equal(closeout_event$passed, 0L)
  expect_equal(closeout_event$assessed, 2L)
  expect_equal(closeout_event$failed, 2L)
  expect_equal(nrow(closeout_forms), 0)
  expect_equal(body$Event, c("All", "Baseline", "", "Closeout", ""))
  expect_equal(body[["N (started/due)"]], c("", "2/2 (100%)", "", "0/2 (0%)", ""))
  expect_equal(body$`Form Incomplete`[[1]], "2/4 (50%)")
  expect_equal(body$Form[[5]], "status_form label")
  expect_equal(body$`Form Incomplete`[[5]], "2 (100%)")
})

test_that("flex_event_forms includes repeat context and missing repeat rows", {
  skip_flex_event_forms_packages()

  flex_out <- flex_event_forms(flex_event_forms_repeat_report())
  body <- tibble::as_tibble(flex_out$body$dataset)

  expect_true("Repeat Instrument" %in% names(body))
  expect_true("Repeat Instance" %in% names(body))
  expect_equal(body$Event[[1]], "All")
  expect_equal(body$Form[[1]], "")
  expect_equal(body$`Repeat Instrument`[[1]], "")
  expect_equal(body$`Repeat Instance`[[1]], "")
  expect_equal(body$`Form Incomplete`[[1]], "7/8 (87.5%)")
  expect_true(any(body$`Repeat Instrument` == "mixed_form label"))
  expect_true(any(body$`Repeat Instance` == "2"))

  repeat_instance_two <- body[
    body$Event == "" &
      body$`Repeat Instrument` == "mixed_form label" &
      body$`Repeat Instance` == "2",
  ]
  expect_equal(repeat_instance_two[["N (started/due)"]], "0/2 (0%)")
  expect_equal(repeat_instance_two$`Form Incomplete`, "2 (100%)")

  regular_c_form <- body[
    body$Event == "" &
      body$Form == "mixed_form label" &
      body$`Repeat Instrument` == "" &
      body$`Repeat Instance` == "" &
      dplyr::lag(body$Event, default = "") == "Regular C",
  ]
  expect_equal(nrow(regular_c_form), 1)
  expect_equal(regular_c_form$`Form Incomplete`, "2 (100%)")
})

test_that("flex_event_forms applies event and form row styling", {
  skip_flex_event_forms_packages()

  flex_out <- flex_event_forms(flex_event_forms_longitudinal_report())
  body <- tibble::as_tibble(flex_out$body$dataset)
  bold_data <- flex_out$body$styles$text$bold$data
  padding_data <- flex_out$body$styles$pars$padding.left$data
  event_rows <- which(body$Event != "")
  form_rows <- which(body$Event == "")

  expect_true(all(as.matrix(bold_data[event_rows, ])))
  expect_false(any(as.matrix(bold_data[form_rows, ])))
  expect_true(all(padding_data[form_rows, "Form"] > padding_data[event_rows[[1]], "Form"]))
})

test_that("flex_event_forms output renders through flex_html", {
  skip_flex_event_forms_packages()
  testthat::skip_if_not_installed("htmltools")

  html_out <- flex_html(flex_event_forms(flex_event_forms_nonlongitudinal_report()))

  expect_type(html_out, "character")
  expect_true(grepl("N (started/due)", html_out, fixed = TRUE))
  expect_true(grepl("Form Incomplete", html_out, fixed = TRUE))
  expect_true(grepl("All", html_out, fixed = TRUE))
  expect_false(grepl("Fields Missing", html_out, fixed = TRUE))
  expect_false(grepl("Form Complete", html_out, fixed = TRUE))
  expect_false(grepl("Total N", html_out, fixed = TRUE))
  expect_true(grepl("baseline_form label", html_out, fixed = TRUE))
})

test_that("flex_event_forms rendered HTML includes custom-ID labels and counts", {
  skip_flex_event_forms_packages()
  testthat::skip_if_not_installed("htmltools")

  html_out <- flex_html(flex_event_forms(flex_event_forms_custom_id_report()))

  expect_type(html_out, "character")
  expect_true(grepl("Baseline", html_out, fixed = TRUE))
  expect_true(grepl("Follow-up", html_out, fixed = TRUE))
  expect_true(grepl("All", html_out, fixed = TRUE))
  expect_true(grepl("status_form label", html_out, fixed = TRUE))
  expect_true(grepl("N (started/due)", html_out, fixed = TRUE))
  expect_true(grepl("1/4 (25%)", html_out, fixed = TRUE))
  expect_true(grepl("2/2 (100%)", html_out, fixed = TRUE))
  expect_true(grepl("0 (0%)", html_out, fixed = TRUE))
  expect_true(grepl("1 (50%)", html_out, fixed = TRUE))
  expect_false(grepl("Fields Missing", html_out, fixed = TRUE))
})
