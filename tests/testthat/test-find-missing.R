test_that("public report API exposes canonical validation surfaces", {
  report_args <- formals(find_missing)

  expect_setequal(
    names(report_args),
    c(
      "data",
      "rcon",
      "forms",
      "events",
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
    "field_complete_checks",
    "field_complete_failures"
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
    c("form-started", "form-complete", "field-complete")
  )
  expect_true(all(report$agent$validation_set$label %in% registry()$validation_check))
  expect_true(any(report$agent$validation_set$step_id == "baseline_form_event-row-started"))
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
    report$missing$validation_level != "field",
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

test_that("invalid forms, events, and instances fail clearly", {
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
      instances = list(baseline_form = 2L)
    ),
    "does not include any requested REDCap repeating"
  )
})
