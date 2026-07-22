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

flex_event_forms_threshold_report <- function(details = FALSE) {
  threshold_meta <- dplyr::bind_rows(
    meta_row("record_id", "threshold_form", required = "y"),
    meta_row(
      "branch_flag",
      "threshold_form",
      field_type = "yesno",
      required = "y"
    ),
    meta_row("field_1", "threshold_form", required = "y"),
    meta_row("field_2", "threshold_form", required = "y"),
    meta_row("field_3", "threshold_form", required = "y"),
    meta_row("field_4", "threshold_form", required = "y"),
    meta_row("field_5", "threshold_form", required = "y"),
    meta_row("field_6", "threshold_form", required = "y"),
    meta_row("field_7", "threshold_form", required = "y"),
    meta_row(
      "conditional_field",
      "threshold_form",
      required = "y",
      branching = "[branch_flag] = '1'"
    )
  )
  records <- tibble::tibble(
    record_id = c("one_of_ten", "one_of_nine", "two_of_ten"),
    branch_flag = c("1", "0", "1"),
    field_1 = c("entered", "", ""),
    field_2 = c("entered", "entered", ""),
    field_3 = rep("entered", 3),
    field_4 = rep("entered", 3),
    field_5 = rep("entered", 3),
    field_6 = rep("entered", 3),
    field_7 = rep("entered", 3),
    conditional_field = c("", "", "entered")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(threshold_meta),
    forms = "threshold_form",
    details = details
  )
}

flex_event_forms_form_specific_eligibility_report <- function() {
  event_meta <- dplyr::bind_rows(
    meta_row("record_id", "demographics", required = "y"),
    meta_row("demographics_started", "demographics", required = "y"),
    meta_row("demographics_value", "demographics", required = "y"),
    meta_row("imaging_started", "imaging", required = "y"),
    meta_row("imaging_value", "imaging", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "baseline_event"),
    form = c("demographics", "imaging")
  )
  events <- tibble::tibble(
    arm_num = 1,
    unique_event_name = "baseline_event",
    event_name = "Baseline",
    custom_event_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    redcap_event_name = c("baseline_event", "baseline_event"),
    demographics_started = c("yes", "yes"),
    demographics_value = c("entered", "entered"),
    imaging_started = c("yes", "yes"),
    imaging_value = c("entered", "entered")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(event_meta, events = events, mapping = mapping),
    forms = c("demographics", "imaging"),
    records = list(baseline_event = list(imaging = "r1"))
  )
}

test_that("flex_event_forms rejects invalid inputs", {
  expect_identical(
    names(formals(flex_event_forms)),
    c("x", "missing_threshold", "...")
  )
  expect_equal(formals(flex_event_forms)$missing_threshold, 0.10)
  expect_identical(
    names(formals(flex_event_forms.redcapmissing)),
    c("x", "missing_threshold", "...")
  )
  expect_error(
    flex_event_forms(list()),
    "no applicable method"
  )
})

test_that("flex_event_forms reduces longitudinal forms under event rows", {
  skip_flex_event_forms_packages()

  expect_warning(
    flex_out <- flex_event_forms(flex_event_forms_longitudinal_report()),
    NA
  )
  body <- tibble::as_tibble(flex_out$body$dataset)

  expect_s3_class(flex_out, "flextable")
  expect_identical(names(body), c(
    "Event",
    "Form",
    "N (started/due)",
    "Form Incomplete",
    "Form Not Started",
    "Form >10% Missing"
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
        "0/2 (0%)",
        "1/2 (50%)",
        "",
        "1/2 (50%)",
        "1/2 (50%)"
      ),
      `Form Not Started` = c(
        "1/8 (12.5%)",
        "",
        "0/2 (0%)",
        "1/2 (50%)",
        "",
        "0/2 (0%)",
        "0/2 (0%)"
      ),
      `Form >10% Missing` = c(
        "3/8 (37.5%)",
        "",
        "0/2 (0%)",
        "1/2 (50%)",
        "",
        "1/2 (50%)",
        "1/2 (50%)"
      )
    )
  )
})

test_that("flex_event_forms event headers follow accessor summaries over raw checks", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_longitudinal_report(details = TRUE)
  raw_checks <- report$details$checks[["event-row-started"]]
  report$details$checks[["event-row-started"]] <- raw_checks[
    raw_checks$redcap_event_name != "event_2_arm_1" |
      raw_checks$record_id != "r2",
    ,
    drop = FALSE
  ]

  summary_event <- get_summary(report)
  summary_event <- summary_event[
    summary_event$validation_check == "event-row-started" &
      summary_event$redcap_event_name == "event_2_arm_1",
    c("passed", "assessed"),
    drop = FALSE
  ]
  summary_event <- unique(summary_event)
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)
  summary_event_pct <- round(
    (summary_event$passed[[1]] / summary_event$assessed[[1]]) * 100,
    1
  )
  summary_event_n <- paste0(
    summary_event$passed[[1]],
    "/",
    summary_event$assessed[[1]],
    " (",
    summary_event_pct,
    "%)"
  )

  expect_equal(nrow(summary_event), 1)
  expect_equal(
    body[body$Event == "Follow-up", "N (started/due)", drop = TRUE],
    summary_event_n
  )
})

test_that("flex_event_forms form-incomplete percentage uses context assessed denominator", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_partly_started_event_report()
  summary_out <- get_summary(report)
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
  expect_equal(body$`Form Incomplete`[[followup_form_row]], "1/2 (50%)")
})

test_that("flex_event_forms rejects denominators unreconciled with eligibility", {
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

  expect_error(
    flex_event_forms(report),
    "2 cached record context.*displayed denominator is 1.*event `event_2_arm_1`, form `status_form`"
  )
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
  summary_out <- get_summary(report)
  flex_body <- tibble::as_tibble(flexify(summary_out)$body$dataset)
  event_form_body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)

  expect_equal(report$spec$id_col, "study_id")
  expect_true("record_id" %in% names(report$details$validation_rows))
  expect_false("study_id" %in% names(report$details$validation_rows))
  expect_true(nrow(summary_out) > 0)
  expect_true(any(summary_out$passed > 0))
  expect_true(nrow(flex_body) > 0)
  expect_true(any(flex_body$assessed > 0))
  expect_true(any(flex_body$form == "status_form label"))

  expect_equal(
    event_form_body,
    tibble::tibble(
      Event = c("All", "Baseline", "", "Follow-up", ""),
      Form = c("", "", "status_form label", "", "status_form label"),
      `N (started/due)` = c("", "2/2 (100%)", "", "2/2 (100%)", ""),
      `Form Incomplete` = c("1/4 (25%)", "", "0/2 (0%)", "", "1/2 (50%)"),
      `Form Not Started` = c("0/4 (0%)", "", "0/2 (0%)", "", "0/2 (0%)"),
      `Form >10% Missing` = c("1/4 (25%)", "", "0/2 (0%)", "", "1/2 (50%)")
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
  expect_equal(body$`Form Not Started`[[1]], "3/6 (50%)")
  expect_equal(body$`Form >10% Missing`[[1]], "5/6 (83.3%)")
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
  expect_equal(repeat_instance_one$`Form Incomplete`, "1/2 (50%)")
  expect_equal(repeat_instance_two[["N (started/due)"]], "0/2 (0%)")
  expect_equal(repeat_instance_two$`Form Incomplete`, "2/2 (100%)")
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
      `Form Incomplete` = c("1/1 (100%)", "", "1/1 (100%)"),
      `Form Not Started` = c("0/1 (0%)", "", "0/1 (0%)"),
      `Form >10% Missing` = c("1/1 (100%)", "", "1/1 (100%)")
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

  expect_equal(form_row$`Form Incomplete`, "1/1 (100%)")
  expect_equal(body$`Form Incomplete`[[1]], "1/1 (100%)")
  expect_equal(form_row$`Form Not Started`, "0/1 (0%)")
  expect_equal(form_row$`Form >10% Missing`, "1/1 (100%)")
  expect_equal(body$`Form >10% Missing`[[1]], "1/1 (100%)")
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
  summary_out <- get_summary(report)
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
      `Form Incomplete` = c("0/1 (0%)", "", "0/1 (0%)"),
      `Form Not Started` = c("0/1 (0%)", "", "0/1 (0%)"),
      `Form >10% Missing` = c("0/1 (0%)", "", "0/1 (0%)")
    )
  )
  zero_body <- tibble::as_tibble(
    flex_event_forms(report, missing_threshold = 0)$body$dataset
  )
  expect_equal(
    zero_body[zero_body$Form == "conditional_form label", "Form >0% Missing", drop = TRUE],
    "0/1 (0%)"
  )
})

test_that("flex_event_forms shows forms for missing events", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_missing_event_report()
  summary_out <- get_summary(report)
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
  expect_equal(body$`Form Not Started`[[1]], "2/4 (50%)")
  expect_equal(body$`Form >10% Missing`[[1]], "2/4 (50%)")
  expect_equal(body$Form[[5]], "status_form label")
  expect_equal(body$`Form Incomplete`[[5]], "2/2 (100%)")
  expect_equal(body$`Form Not Started`[[5]], "2/2 (100%)")
  expect_equal(body$`Form >10% Missing`[[5]], "2/2 (100%)")
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
  expect_equal(body$`Form Not Started`[[1]], "5/8 (62.5%)")
  expect_equal(body$`Form >10% Missing`[[1]], "7/8 (87.5%)")
  expect_true(any(body$`Repeat Instrument` == "mixed_form label"))
  expect_true(any(body$`Repeat Instance` == "2"))

  repeat_instance_two <- body[
    body$Event == "" &
      body$`Repeat Instrument` == "mixed_form label" &
      body$`Repeat Instance` == "2",
  ]
  expect_equal(repeat_instance_two[["N (started/due)"]], "0/2 (0%)")
  expect_equal(repeat_instance_two$`Form Incomplete`, "2/2 (100%)")
  expect_equal(repeat_instance_two$`Form Not Started`, "2/2 (100%)")
  expect_equal(repeat_instance_two$`Form >10% Missing`, "2/2 (100%)")

  regular_c_form <- body[
    body$Event == "" &
      body$Form == "mixed_form label" &
      body$`Repeat Instrument` == "" &
      body$`Repeat Instance` == "" &
      dplyr::lag(body$Event, default = "") == "Regular C",
  ]
  expect_equal(nrow(regular_c_form), 1)
  expect_equal(regular_c_form$`Form Incomplete`, "2/2 (100%)")
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
  expect_true(grepl("Form Not Started", html_out, fixed = TRUE))
  expect_true(grepl("Form &gt;10% Missing", html_out, fixed = TRUE))
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
  expect_true(grepl("0/2 (0%)", html_out, fixed = TRUE))
  expect_true(grepl("1/2 (50%)", html_out, fixed = TRUE))
  expect_false(grepl("Fields Missing", html_out, fixed = TRUE))
})

test_that("flex_event_forms applies record-specific threshold comparisons", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_threshold_report()
  cache <- report$spec$.flex_event_forms_field_counts
  cache <- cache[match(
    c("one_of_ten", "one_of_nine", "two_of_ten"),
    cache$record_id
  ), , drop = FALSE]

  expect_equal(cache$field_assessed, c(10L, 9L, 10L))
  expect_equal(cache$field_failed, c(1L, 1L, 2L))

  default_body <- tibble::as_tibble(
    flex_event_forms(report)$body$dataset
  )
  default_form <- default_body[default_body$Form == "threshold_form label", ]
  expect_true("Form >10% Missing" %in% names(default_body))
  expect_equal(default_form$`Form Not Started`, "0/3 (0%)")
  expect_equal(default_form$`Form >10% Missing`, "2/3 (66.7%)")

  zero_body <- tibble::as_tibble(
    flex_event_forms(report, missing_threshold = 0)$body$dataset
  )
  expect_equal(
    zero_body[zero_body$Form == "threshold_form label", "Form >0% Missing", drop = TRUE],
    "3/3 (100%)"
  )

  one_body <- tibble::as_tibble(
    flex_event_forms(report, missing_threshold = 1)$body$dataset
  )
  expect_equal(
    one_body[one_body$Form == "threshold_form label", "Form = 100% Missing", drop = TRUE],
    "0/3 (0%)"
  )

  custom_body <- tibble::as_tibble(
    flex_event_forms(report, missing_threshold = 0.125)$body$dataset
  )
  expect_true("Form >12.5% Missing" %in% names(custom_body))
  expect_equal(
    custom_body[custom_body$Form == "threshold_form label", "Form >12.5% Missing", drop = TRUE],
    "1/3 (33.3%)"
  )
})

test_that("flex_event_forms validates missing_threshold", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_threshold_report()
  invalid <- list(
    NULL,
    numeric(),
    c(0.1, 0.2),
    TRUE,
    "0.1",
    NA_real_,
    NaN,
    Inf,
    -0.01,
    1.01
  )
  for (value in invalid) {
    expect_error(
      flex_event_forms(report, missing_threshold = value),
      "`missing_threshold` must be one finite numeric value from 0 through 1"
    )
  }
})

test_that("flex_event_forms compact and detailed reports use the same cache", {
  skip_flex_event_forms_packages()

  compact <- flex_event_forms_threshold_report(details = FALSE)
  detailed <- flex_event_forms_threshold_report(details = TRUE)
  compact_body <- tibble::as_tibble(flex_event_forms(compact)$body$dataset)
  detailed_body <- tibble::as_tibble(flex_event_forms(detailed)$body$dataset)

  expect_null(compact$details)
  expect_false(is.null(detailed$details))
  expect_equal(
    compact$spec$.flex_event_forms_field_counts,
    detailed$spec$.flex_event_forms_field_counts
  )
  expect_equal(compact_body, detailed_body)
})

test_that("flex_event_forms counts a failed form-started gate once", {
  skip_flex_event_forms_packages()

  start_meta <- dplyr::bind_rows(
    meta_row("record_id", "start_form", required = "y"),
    meta_row("started_field", "start_form", required = "y"),
    meta_row("value_field", "start_form", required = "y")
  )
  report <- find_missing(
    data = tibble::tibble(
      record_id = "r1",
      started_field = "",
      value_field = ""
    ),
    rcon = fake_rcon(start_meta),
    forms = "start_form"
  )
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)
  form_row <- body[body$Form == "start_form label", , drop = FALSE]

  expect_equal(report$spec$.flex_event_forms_field_counts$field_assessed, 0L)
  expect_equal(form_row$`Form Not Started`, "1/1 (100%)")
  expect_equal(form_row$`Form >10% Missing`, "1/1 (100%)")

  one_body <- tibble::as_tibble(
    flex_event_forms(report, missing_threshold = 1)$body$dataset
  )
  one_form_row <- one_body[one_body$Form == "start_form label", ]
  expect_equal(one_form_row$`Form = 100% Missing`, "1/1 (100%)")
  expect_equal(one_body$`Form = 100% Missing`[[1]], "1/1 (100%)")
})

test_that("threshold 1 counts started forms with all assessed fields missing", {
  skip_flex_event_forms_packages()

  all_missing_meta <- dplyr::bind_rows(
    meta_row("record_id", "all_missing_form"),
    meta_row("started_signal", "all_missing_form"),
    meta_row("required_a", "all_missing_form", required = "y"),
    meta_row("required_b", "all_missing_form", required = "y")
  )
  report <- find_missing(
    data = tibble::tibble(
      record_id = "r1",
      started_signal = "yes",
      required_a = "",
      required_b = ""
    ),
    rcon = fake_rcon(all_missing_meta),
    forms = "all_missing_form"
  )
  body <- tibble::as_tibble(
    flex_event_forms(report, missing_threshold = 1)$body$dataset
  )
  form_row <- body[body$Form == "all_missing_form label", , drop = FALSE]

  expect_equal(report$spec$.flex_event_forms_field_counts$field_assessed, 2L)
  expect_equal(report$spec$.flex_event_forms_field_counts$field_failed, 2L)
  expect_equal(form_row$`Form Not Started`, "0/1 (0%)")
  expect_equal(form_row$`Form = 100% Missing`, "1/1 (100%)")
  expect_equal(body$`Form = 100% Missing`[[1]], "1/1 (100%)")
})

test_that("flex_event_forms sums form-specific opportunities in All", {
  skip_flex_event_forms_packages()

  report <- flex_event_forms_form_specific_eligibility_report()
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)
  demographics <- body[body$Form == "demographics label", , drop = FALSE]
  imaging <- body[body$Form == "imaging label", , drop = FALSE]

  expect_equal(body$`Form Incomplete`[[1]], "0/3 (0%)")
  expect_equal(body$`Form Not Started`[[1]], "0/3 (0%)")
  expect_equal(body$`Form >10% Missing`[[1]], "0/3 (0%)")
  expect_equal(demographics$`Form Not Started`, "0/2 (0%)")
  expect_equal(imaging$`Form Not Started`, "0/1 (0%)")
})

test_that("flex_event_forms honors repeat-instance record eligibility", {
  skip_flex_event_forms_packages()

  repeat_meta <- dplyr::bind_rows(
    meta_row("record_id", "screen_form", required = "y"),
    meta_row("screen_started", "screen_form", required = "y"),
    meta_row("repeat_started", "repeat_form", required = "y"),
    meta_row("repeat_value", "repeat_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "baseline_event"),
    form = c("screen_form", "repeat_form")
  )
  events <- tibble::tibble(
    arm_num = 1,
    unique_event_name = "baseline_event",
    event_name = "Baseline",
    custom_event_label = ""
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "baseline_event",
    form_name = "repeat_form",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    redcap_event_name = c("baseline_event", "baseline_event"),
    redcap_repeat_instrument = c("repeat_form", "repeat_form"),
    redcap_repeat_instance = c("1", "2"),
    repeat_started = c("yes", "yes"),
    repeat_value = c("10", "20")
  )
  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      repeat_meta,
      events = events,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "repeat_form",
    instances = 2L,
    records = list(
      baseline_event = list(
        repeat_form = list(
          `1` = "r1",
          `2` = c("r2", "r3")
        )
      )
    )
  )
  body <- tibble::as_tibble(flex_event_forms(report)$body$dataset)
  instance_one <- body[body$`Repeat Instance` == "1", , drop = FALSE]
  instance_two <- body[body$`Repeat Instance` == "2", , drop = FALSE]

  expect_equal(body$`Form Not Started`[[1]], "1/3 (33.3%)")
  expect_equal(body$`Form >10% Missing`[[1]], "1/3 (33.3%)")
  expect_equal(instance_one$`Form Not Started`, "0/1 (0%)")
  expect_equal(instance_two$`Form Not Started`, "1/2 (50%)")
  expect_equal(instance_two$`Form >10% Missing`, "1/2 (50%)")
})

test_that("flex_event_forms requires a valid complete field-count cache", {
  skip_flex_event_forms_packages()

  old_report <- flex_event_forms_threshold_report(details = TRUE)
  old_report$spec$.flex_event_forms_field_counts <- NULL
  expect_error(
    flex_event_forms(old_report),
    "Rerun .* to rebuild this report"
  )

  missing_report <- flex_event_forms_threshold_report()
  missing_report$spec$.flex_event_forms_field_counts <-
    missing_report$spec$.flex_event_forms_field_counts[-1, , drop = FALSE]
  expect_error(
    flex_event_forms(missing_report),
    "no field-count data for record"
  )

  duplicate_report <- flex_event_forms_threshold_report()
  cache <- duplicate_report$spec$.flex_event_forms_field_counts
  duplicate_report$spec$.flex_event_forms_field_counts <- rbind(
    cache,
    cache[1, , drop = FALSE]
  )
  expect_error(
    flex_event_forms(duplicate_report),
    "duplicate field-count data for record"
  )

  corrupt_report <- flex_event_forms_threshold_report()
  corrupt_report$spec$.flex_event_forms_field_counts$field_failed[[1]] <- -1L
  expect_error(
    flex_event_forms(corrupt_report),
    "invalid `field_failed` for record"
  )

  impossible_report <- flex_event_forms_threshold_report()
  impossible_report$spec$.flex_event_forms_field_counts$field_failed[[1]] <-
    impossible_report$spec$.flex_event_forms_field_counts$field_assessed[[1]] + 1L
  expect_error(
    flex_event_forms(impossible_report),
    "`field_failed` greater than `field_assessed` for record"
  )
})

test_that("flex_event_forms cache reflects ignored IDs fields and checkbox roots", {
  skip_flex_event_forms_packages()

  records <- tibble::tibble(
    record_id = c("keep", "drop"),
    branch_flag = c("0", "0"),
    required_note = c("", ""),
    optional_note = c("", ""),
    checkbox_field___1 = c("1", "1"),
    checkbox_field___2 = c("0", "0"),
    checkbox_other = c("", ""),
    conditional_note = c("", "")
  )
  report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form",
    required_fields = FALSE,
    ignore_fields = c("optional_note", "checkbox_field___2"),
    ignore_ids = "drop",
    details = TRUE
  )
  cache <- report$spec$.flex_event_forms_field_counts
  field_checks <- report$details$checks[["field-complete"]]

  expect_equal(cache$record_id, "keep")
  expect_equal(cache$field_assessed, nrow(field_checks))
  expect_equal(cache$field_failed, sum(!field_checks$validation_passed))
  expect_false(any(field_checks$field_name %in% c(
    "optional_note",
    "checkbox_field"
  )))
})
