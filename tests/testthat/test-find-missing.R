test_that("public report API exposes canonical validation surfaces", {
  report_args <- formals(find_missing)

  expect_setequal(
    names(report_args),
    c(
      "data",
      "rcon",
      "forms",
      "events",
      "records",
      "required_fields",
      "ignore_fields",
      "ignore_ids",
      "exclude_types",
      "instances"
    )
  )
  expect_true("find_missing" %in% getNamespaceExports("redcapmissing"))
  expect_true("registry" %in% getNamespaceExports("redcapmissing"))
  expect_false("form" %in% names(report_args))
  expect_false("redcap_missing_report" %in% getNamespaceExports("redcapmissing"))
})

test_that("old report argument names are not supported", {
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    required_note = "entered",
    optional_note = "",
    checkbox_field___1 = "1",
    checkbox_field___2 = "0",
    checkbox_other = "",
    conditional_note = ""
  )
  report_args <- list(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form"
  )

  expect_error(
    do.call(
      find_missing,
      c(report_args[setdiff(names(report_args), "forms")], list(form = "baseline_form"))
    ),
    "unused argument"
  )
  expect_error(
    do.call(
      find_missing,
      c(report_args, list(desired_events = NULL))
    ),
    "unused argument"
  )
  expect_error(
    do.call(
      find_missing,
      c(report_args, list(expected_repeats = 1L))
    ),
    "unused argument"
  )
})

test_that("report object uses validation-check canon and removes old names", {
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    branch_flag = c("0", "0"),
    required_note = c("entered", ""),
    optional_note = c("", ""),
    checkbox_field___1 = c("1", "1"),
    checkbox_field___2 = c("0", "0"),
    checkbox_other = c("", ""),
    conditional_note = c("", "")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form"
  )

  expect_true(all(c(
    "event_row_started_checks",
    "event_row_started_failures",
    "instance_row_started_checks",
    "instance_row_started_failures",
    "form_started_checks",
    "form_started_failures",
    "form_complete_checks",
    "form_complete_failures",
    "event_complete_checks",
    "event_complete_failures",
    "field_complete_checks",
    "field_complete_failures",
    "eligible_records"
  ) %in% names(report)))
  expect_false(any(c(
    "event_row_exists_checks",
    "event_row_exists_failures",
    "repeat_instance_row_exists_checks",
    "repeat_instance_row_exists_failures",
    "fields_complete_checks",
    "fields_complete_failures"
  ) %in% names(report)))
  expect_true(all(c(
    "validation_level",
    "validation_check_type",
    "validation_check",
    "validation_label",
    "validation_passed"
  ) %in% names(report$validation_rows)))
  expect_false(any(c(
    "validation_scope",
    "event_row_exists",
    "repeat_instance_row_exists",
    "form_started",
    "form_complete",
    "field_complete"
  ) %in% names(report$validation_rows)))
  expect_setequal(
    unique(report$validation_rows$validation_check),
    c("form-started", "form-complete", "field-complete", "event-complete")
  )
  expect_setequal(
    unique(report$validation_rows$validation_level),
    c("event:form", "event")
  )
  expect_true(all(report$agent$validation_set$label %in% registry()$validation_check))
  expect_true(any(report$agent$validation_set$step_id == "baseline_form_event-row-started"))
  expect_true(any(report$agent$validation_set$step_id == "event-complete"))
})

test_that("non-field validation rows use typed NA field columns", {
  records <- tibble::tibble(
    record_id = "blank",
    branch_flag = "",
    required_note = "",
    optional_note = "",
    checkbox_field___1 = "0",
    checkbox_field___2 = "0",
    checkbox_other = "",
    conditional_note = ""
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form",
    required_fields = FALSE
  )

  non_field_failures <- report$missing[
    report$missing$validation_check != "field-complete",
    ,
    drop = FALSE
  ]

  expect_true(nrow(non_field_failures) > 0)
  expect_true(all(is.na(non_field_failures$field_name)))
  expect_true(all(is.na(non_field_failures$field_label)))
  expect_true(all(is.na(non_field_failures$field_type)))
  expect_true(all(is.na(non_field_failures$branching_logic)))
  expect_true(all(is.na(non_field_failures$value_summary)))
  expect_true(all(is.na(non_field_failures$export_fields)))
  expect_false(any(
    report$missing$record_id == "blank" &
      report$missing$validation_check == "field-complete"
  ))
})

test_that("branch-open fields fail and branch-closed fields are not expected", {
  records <- tibble::tibble(
    record_id = c("open", "closed"),
    branch_flag = c("1", "0"),
    required_note = c("entered", "entered"),
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
    required_fields = FALSE
  )

  expect_true(any(
    report$missing$record_id == "open" &
      report$missing$field_name == "conditional_note" &
      report$missing$validation_check == "field-complete"
  ))
  expect_false(any(
    report$field_complete_checks$record_id == "closed" &
      report$field_complete_checks$field_name == "conditional_note"
  ))
})

test_that("event-row-started strictly gates downstream checks", {
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
    redcap_event_name = c("event_1_arm_1", "event_2_arm_1", "event_1_arm_1", "event_2_arm_1"),
    status_started = c("yes", "yes", "yes", "yes"),
    status_value = c("entered", "", "entered", "entered")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form"
  )

  expect_true(any(
    report$missing$redcap_event_name == "event_3_arm_1" &
      report$missing$validation_check == "event-row-started"
  ))
  expect_false(any(
    report$form_started_checks$redcap_event_name == "event_3_arm_1"
  ))
  expect_false(any(
    report$form_complete_checks$redcap_event_name == "event_3_arm_1"
  ))
  expect_false(any(
    report$field_complete_checks$redcap_event_name == "event_3_arm_1"
  ))
  expect_true(any(
    report$missing$redcap_event_name == "event_2_arm_1" &
      report$missing$validation_check == "form-complete"
  ))
  expect_true(any(
    report$missing$redcap_event_name == "event_2_arm_1" &
      report$missing$validation_check == "field-complete"
  ))
  event_complete_summary <- report$agent$validation_set[
    report$agent$validation_set$validation_check == "event-complete",
    ,
    drop = FALSE
  ]
  event_complete_summary <- event_complete_summary[
    order(event_complete_summary$redcap_event_name),
    ,
    drop = FALSE
  ]
  expect_equal(
    event_complete_summary$redcap_event_name,
    paste0("event_", 1:3, "_arm_1")
  )
  expect_equal(event_complete_summary$n, c(2, 2, 2))
  expect_equal(event_complete_summary$n_failed, c(0, 1, 2))
  expect_true(any(
    report$missing$redcap_event_name == "event_3_arm_1" &
      report$missing$validation_check == "event-complete"
  ))
})

test_that("failed event-row-started checks do not create blank-event downstream contexts", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "patient_status", field_label = "Record ID", required = "y"),
    meta_row("patient_status_started", "patient_status", required = "y"),
    meta_row("patient_status_value", "patient_status", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c("baseline_event", "follow_up_1", "follow_up_2"),
    form = c("enrollment", "patient_status", "patient_status")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2", "r3"),
    redcap_event_name = "baseline_event",
    patient_status_started = "yes",
    patient_status_value = "entered"
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "patient_status"
  )

  event_started <- report$agent$validation_set[
    report$agent$validation_set$validation_check == "event-row-started",
    ,
    drop = FALSE
  ]
  event_started <- event_started[order(event_started$redcap_event_name), , drop = FALSE]
  expect_equal(event_started$redcap_event_name, c("follow_up_1", "follow_up_2"))
  expect_equal(event_started$n, c(3, 3))
  expect_equal(event_started$n_failed, c(3, 3))

  expect_equal(nrow(report$form_started_checks), 0)
  expect_equal(nrow(report$form_complete_checks), 0)
  expect_equal(nrow(report$field_complete_checks), 0)

  form_started <- report$agent$validation_set[
    report$agent$validation_set$validation_check == "form-started",
    ,
    drop = FALSE
  ]
  expect_equal(form_started$n, 0)
  expect_equal(form_started$n_passed, 0)

  event_complete <- report$agent$validation_set[
    report$agent$validation_set$validation_check == "event-complete",
    ,
    drop = FALSE
  ]
  event_complete <- event_complete[order(event_complete$redcap_event_name), , drop = FALSE]
  expect_equal(event_complete$redcap_event_name, c("follow_up_1", "follow_up_2"))
  expect_equal(event_complete$n, c(3, 3))
  expect_equal(event_complete$n_failed, c(3, 3))
  expect_false(any(
    event_complete$redcap_event_name == "" &
      event_complete$n_passed > 0
  ))
})

test_that("instance-row-started gates downstream repeat checks", {
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
    forms = "repeat_form",
    instances = 2L
  )

  expect_equal(nrow(report$instance_row_started_checks), 4)
  expect_equal(nrow(report$instance_row_started_failures), 1)
  expect_true(any(report$validation_rows$validation_level == "event:form:instance"))
  expect_true(any(
    report$agent$validation_set$validation_level == "event:form:instance"
  ))
  expect_true(any(
    report$missing$record_id == "r1" &
      report$missing$redcap_repeat_instance == "2" &
      report$missing$validation_check == "instance-row-started"
  ))
  expect_false(any(
    report$form_started_checks$record_id == "r1" &
      report$form_started_checks$redcap_repeat_instance == "2"
  ))
  expect_equal(nrow(report$event_row_started_checks), 0)
  event_complete_summary <- report$agent$validation_set[
    report$agent$validation_set$validation_check == "event-complete",
    ,
    drop = FALSE
  ]
  expect_equal(event_complete_summary$redcap_event_name, "baseline_event")
  expect_equal(event_complete_summary$n, 2)
  expect_equal(event_complete_summary$n_failed, 1)
  expect_true(any(
    report$missing$record_id == "r1" &
      report$missing$redcap_event_name == "baseline_event" &
      report$missing$validation_check == "event-complete"
  ))
})

test_that("mixed repeat and non-repeat forms activate row checks per context", {
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

  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      mixed_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "mixed_form",
    instances = 2L
  )

  expect_true(any(report$event_row_started_failures$redcap_event_name == "regular_c_arm_1"))
  expect_false(any(report$event_row_started_checks$redcap_event_name == "repeat_b_arm_1"))
  expect_true(any(report$instance_row_started_failures$redcap_event_name == "repeat_b_arm_1"))
  expect_false(any(report$form_started_checks$redcap_event_name == "regular_c_arm_1"))
  expect_false(any(
    report$field_complete_checks$record_id == "r2" &
      report$field_complete_checks$redcap_event_name == "regular_a_arm_1"
  ))
  expect_true(any(
    report$missing$redcap_event_name == "repeat_b_arm_1" &
      report$missing$validation_check == "form-complete"
  ))
  expect_true(any(
    report$missing$redcap_event_name == "repeat_b_arm_1" &
      report$missing$validation_check == "field-complete"
  ))
})

test_that("event-complete rolls up on-route failures across requested forms", {
  event_meta <- dplyr::bind_rows(
    meta_row("record_id", "demographics", field_label = "Record ID", required = "y"),
    meta_row("demographics_started", "demographics", required = "y"),
    meta_row("demographics_value", "demographics", required = "y"),
    meta_row("imaging_started", "imaging", required = "y"),
    meta_row("imaging_value", "imaging", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1, 1),
    unique_event_name = c(
      "baseline_event",
      "followup_event",
      "baseline_event",
      "followup_event"
    ),
    form = c("demographics", "demographics", "imaging", "imaging")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1", "r2", "r2"),
    redcap_event_name = c(
      "baseline_event",
      "followup_event",
      "baseline_event",
      "followup_event"
    ),
    demographics_started = "yes",
    demographics_value = "entered",
    imaging_started = "yes",
    imaging_value = c("", "entered", "entered", "entered")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(event_meta, mapping = mapping),
    forms = c("demographics", "imaging")
  )
  tidy_tbl <- tidy(report)
  event_complete <- tidy_tbl[
    tidy_tbl$validation_check == "event-complete",
    ,
    drop = FALSE
  ]
  event_complete <- event_complete[
    order(event_complete$redcap_event_name),
    ,
    drop = FALSE
  ]

  expect_equal(event_complete$form, c("", ""))
  expect_equal(event_complete$validation_level, c("event", "event"))
  expect_equal(event_complete$validation_check_type, c("detour", "detour"))
  expect_equal(
    event_complete$redcap_event_name,
    c("baseline_event", "followup_event")
  )
  expect_equal(event_complete$assessed, c(2, 2))
  expect_equal(event_complete$failed, c(1, 0))
  expect_true(any(
    report$event_complete_failures$record_id == "r1" &
      report$event_complete_failures$redcap_event_name == "baseline_event"
  ))
  expect_true(any(
    report$missing$record_id == "r1" &
      report$missing$redcap_event_name == "baseline_event" &
      report$missing$validation_check == "event-complete"
  ))
  expect_equal(unique(report$event_complete_checks$form), "")
  expect_true(all(is.na(report$event_complete_checks$field_name)))
  expect_true(all(is.na(report$event_complete_checks$field_label)))
  expect_true(all(is.na(report$event_complete_checks$field_type)))
  expect_true(all(is.na(report$event_complete_checks$branching_logic)))
  expect_true(all(is.na(report$event_complete_checks$value_summary)))
  expect_true(all(is.na(report$event_complete_checks$export_fields)))
})

test_that("event-complete ignores detour-only validation failures", {
  context <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "baseline_event",
    redcap_repeat_instrument = "",
    redcap_repeat_instance = ""
  )
  on_route_pass <- .miss_build_issue_rows(
    contexts = context,
    form = "status_form",
    validation_check = "form-started",
    validation_passed = TRUE
  )
  detour_fail <- .miss_build_issue_rows(
    contexts = context,
    form = "status_form",
    validation_check = "form-complete",
    validation_passed = FALSE
  )

  event_complete <- .miss_build_event_complete_check_rows(
    dplyr::bind_rows(on_route_pass, detour_fail)
  )

  expect_equal(nrow(event_complete), 1)
  expect_true(event_complete$validation_passed)
  expect_equal(event_complete$validation_check, "event-complete")
})

test_that("multi-arm event denominators do not cross arms", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 2, 2),
    unique_event_name = c(
      "arm_1_event_1",
      "arm_1_event_2",
      "arm_2_event_1",
      "arm_2_event_2"
    ),
    form = c("status_form", "status_form", "status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    redcap_event_name = c("arm_1_event_1", "arm_2_event_1"),
    status_started = c("yes", "yes"),
    status_value = c("entered", "entered")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form"
  )
  event_summary <- report$agent$validation_set[
    report$agent$validation_set$validation_check == "event-row-started",
    ,
    drop = FALSE
  ]
  event_summary <- event_summary[order(event_summary$redcap_event_name), , drop = FALSE]

  expect_equal(event_summary$redcap_event_name, c(
    "arm_1_event_1",
    "arm_1_event_2",
    "arm_2_event_1",
    "arm_2_event_2"
  ))
  expect_equal(event_summary$n, c(1, 1, 1, 1))
  expect_equal(event_summary$n_failed, c(0, 1, 0, 1))
})

test_that("records limits event-level eligibility across requested forms", {
  event_meta <- dplyr::bind_rows(
    meta_row("record_id", "demographics", field_label = "Record ID", required = "y"),
    meta_row("demographics_started", "demographics", required = "y"),
    meta_row("demographics_value", "demographics", required = "y"),
    meta_row("surgery_started", "surgery", required = "y"),
    meta_row("surgery_value", "surgery", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c("event_a", "event_b", "event_c"),
    form = c("demographics", "surgery", "surgery")
  )
  records <- tibble::tibble(
    record_id = c("record_a", "record_a", "record_b", "record_b", "record_b"),
    redcap_event_name = c("event_a", "event_b", "event_a", "event_b", "event_c"),
    demographics_started = c("yes", "", "yes", "", ""),
    demographics_value = c("entered", "", "entered", "", ""),
    surgery_started = c("", "yes", "", "yes", "yes"),
    surgery_value = c("", "entered", "", "entered", "")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(event_meta, mapping = mapping),
    forms = c("surgery", "demographics"),
    records = list(
      event_a = c("record_a", "record_b"),
      event_b = c("record_a", "record_b"),
      event_c = "record_b"
    )
  )

  expect_identical(report$eligible_records$event_c, "record_b")
  expect_false(any(
    report$validation_rows$record_id == "record_a" &
      report$validation_rows$redcap_event_name == "event_c"
  ))
  expect_true(any(
    report$field_complete_failures$record_id == "record_b" &
      report$field_complete_failures$redcap_event_name == "event_c" &
      report$field_complete_failures$form == "surgery"
  ))
  surgery_event_summary <- report$agent$validation_set[
    report$agent$validation_set$form == "surgery" &
      report$agent$validation_set$validation_check == "event-row-started",
    ,
    drop = FALSE
  ]
  surgery_event_summary <- surgery_event_summary[
    order(surgery_event_summary$redcap_event_name),
    ,
    drop = FALSE
  ]
  expect_equal(surgery_event_summary$redcap_event_name, c("event_b", "event_c"))
  expect_equal(surgery_event_summary$n, c(2, 1))
  expect_equal(surgery_event_summary$n_failed, c(0, 0))
})

test_that("records partially overrides only named events", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", required = "y"),
    meta_row("status_value", "status_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c("event_1", "event_2", "event_3"),
    form = c("status_form", "status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1", "r2", "r2"),
    redcap_event_name = c("event_1", "event_2", "event_1", "event_2"),
    status_started = "yes",
    status_value = "entered"
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    records = list(event_3 = "r1")
  )
  event_summary <- report$agent$validation_set[
    report$agent$validation_set$validation_check == "event-row-started",
    ,
    drop = FALSE
  ]
  event_summary <- event_summary[order(event_summary$redcap_event_name), , drop = FALSE]

  expect_equal(event_summary$redcap_event_name, c("event_1", "event_2", "event_3"))
  expect_equal(event_summary$n, c(2, 2, 1))
  expect_equal(event_summary$n_failed, c(0, 0, 1))
  expect_true(any(
    report$event_row_started_failures$record_id == "r1" &
      report$event_row_started_failures$redcap_event_name == "event_3"
  ))
})

test_that("events and records intersect before assessment", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", required = "y"),
    meta_row("status_value", "status_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c("event_1", "event_2", "event_3"),
    form = c("status_form", "status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    redcap_event_name = c("event_2", "event_3"),
    status_started = "yes",
    status_value = "entered"
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    events = "event_3",
    records = list(event_2 = "r1", event_3 = "r2")
  )

  expect_identical(report$events$status_form, "event_3")
  expect_equal(unique(report$validation_rows$redcap_event_name), "event_3")
  expect_equal(report$event_row_started_checks$record_id, "r2")
  expect_equal(nrow(report$event_row_started_checks), 1)
})

test_that("records IDs absent from data create upstream failures", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", required = "y"),
    meta_row("status_value", "status_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("event_1", "event_2"),
    form = c("status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "event_1",
    status_started = "yes",
    status_value = "entered"
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    records = list(event_2 = "missing_id")
  )

  expect_true(any(
    report$event_row_started_failures$record_id == "missing_id" &
      report$event_row_started_failures$redcap_event_name == "event_2"
  ))
  expect_false(any(report$form_started_checks$record_id == "missing_id"))
  expect_false(any(report$field_complete_checks$record_id == "missing_id"))
})

test_that("records eligibility applies to repeat-instance checks", {
  repeat_meta <- dplyr::bind_rows(
    meta_row("record_id", "screen_form", field_label = "Record ID", required = "y"),
    meta_row("screen_started", "screen_form", required = "y"),
    meta_row("repeat_started", "repeat_form", required = "y"),
    meta_row("repeat_value", "repeat_form", required = "y")
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
    forms = "repeat_form",
    records = list(baseline_event = "r2"),
    instances = 2L
  )

  expect_equal(nrow(report$instance_row_started_checks), 2)
  expect_equal(unique(report$instance_row_started_checks$record_id), "r2")
  expect_false(any(report$validation_rows$record_id == "r1"))
})

test_that("ignore_ids wins over records eligibility", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", required = "y"),
    meta_row("status_value", "status_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("event_1", "event_2"),
    form = c("status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = c("keep", "drop"),
    redcap_event_name = c("event_2", "event_2"),
    status_started = "yes",
    status_value = "entered"
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    records = list(event_2 = c("keep", "drop")),
    ignore_ids = "drop"
  )

  expect_equal(report$ignored_ids, "drop")
  expect_identical(report$eligible_records$event_2, "keep")
  expect_false(any(report$validation_rows$record_id == "drop"))
})

test_that("unused valid records events are retained but harmless", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_value", "status_form", required = "y"),
    meta_row("other_value", "other_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("status_event", "unused_event"),
    form = c("status_form", "other_form")
  )
  records <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "status_event",
    status_value = "entered",
    other_value = ""
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    records = list(unused_event = "r1")
  )

  expect_identical(report$eligible_records$unused_event, "r1")
  expect_false(any(report$validation_rows$redcap_event_name == "unused_event"))
})

test_that("ignore fields and ignore ids are applied before assessment", {
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
    ignore_ids = "drop"
  )

  expect_equal(report$ignored_ids, "drop")
  expect_true(all(c("optional_note", "checkbox_field") %in% report$ignored_fields))
  expect_false(any(report$field_complete_checks$record_id == "drop"))
  expect_false(any(report$field_complete_checks$field_name %in% c(
    "optional_note",
    "checkbox_field"
  )))
})

test_that("empty records entries behave like omitted events", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", required = "y"),
    meta_row("status_value", "status_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("event_1", "event_2"),
    form = c("status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    redcap_event_name = c("event_1", "event_1"),
    status_started = "yes",
    status_value = "entered"
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    records = list(event_2 = c("", NA_character_))
  )
  event_summary <- report$agent$validation_set[
    report$agent$validation_set$validation_check == "event-row-started" &
      report$agent$validation_set$redcap_event_name == "event_2",
    ,
    drop = FALSE
  ]

  expect_equal(report$eligible_records, list())
  expect_equal(event_summary$n, 2)
  expect_equal(event_summary$n_failed, 2)
})

test_that("invalid forms, events, records, and instances fail clearly", {
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

  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(baseline_form_meta())
    ),
    "Provide `forms`"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(baseline_form_meta()),
      forms = character()
    ),
    "at least one"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(baseline_form_meta()),
      forms = "baseline_form",
      records = "r1"
    ),
    "`records` must be NULL or a named list"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(baseline_form_meta()),
      forms = "baseline_form",
      records = list("r1")
    ),
    "named by raw REDCap `redcap_event_name`"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(
        baseline_form_meta(),
        mapping = tibble::tibble(
          arm_num = c(1, 1),
          unique_event_name = c("event_1", "event_2"),
          form = c("baseline_form", "baseline_form")
        )
      ),
      forms = "baseline_form",
      records = structure(
        list("r1", "r2"),
        names = c("event_1", "event_1")
      )
    ),
    "must not be duplicated"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(
        baseline_form_meta(),
        mapping = tibble::tibble(
          arm_num = 1,
          unique_event_name = "event_1",
          form = "baseline_form"
        )
      ),
      forms = "baseline_form",
      records = list(unknown_event = "r1")
    ),
    "Unknown event"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(
        baseline_form_meta(),
        mapping = tibble::tibble(
          arm_num = 1,
          unique_event_name = "event_1",
          form = "baseline_form"
        )
      ),
      forms = "baseline_form",
      records = list(event_1 = list("r1"))
    ),
    "must be a vector"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(baseline_form_meta()),
      forms = "baseline_form",
      instances = list(baseline_form = 2L)
    ),
    "does not include any requested REDCap repeating"
  )
})
