test_that("public report API is stable", {
  report_args <- formals(find_missing)
  old_event_arg <- paste0("desired", "_events")
  old_instance_arg <- paste0("expected", "_repeats")

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
  expect_identical(report_args$forms, quote(expr = ))
  expect_null(report_args$events)
  expect_true(report_args$required_fields)
  expect_null(report_args$ignore_fields)
  expect_null(report_args$ignore_ids)
  expect_null(report_args$instances)
  expect_false("form" %in% names(report_args))
  expect_false(old_event_arg %in% names(report_args))
  expect_false(old_instance_arg %in% names(report_args))
  expect_true("find_missing" %in% getNamespaceExports("redcapmissing"))
  expect_false("redcap_missing_report" %in% getNamespaceExports("redcapmissing"))
  expect_false("redcap_missing_summary" %in% getNamespaceExports("redcapmissing"))
})

test_that("old report argument and return names are not supported", {
  old_event_arg <- paste0("desired", "_events")
  old_instance_arg <- paste0("expected", "_repeats")
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
      c(report_args, stats::setNames(list(NULL), old_event_arg))
    ),
    "unused argument"
  )
  expect_error(
    do.call(
      find_missing,
      c(report_args, stats::setNames(list(1L), old_instance_arg))
    ),
    "unused argument"
  )

  report <- do.call(find_missing, report_args)
  expect_true(all(c("forms", "form_labels", "events", "instances") %in% names(report)))
  expect_false(any(c(old_event_arg, old_instance_arg, "form", "form_label") %in% names(report)))
})

test_that("instrument labels are required and strict", {
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
  metadata <- baseline_form_meta()

  expect_error(
    find_missing(
      data = records,
      rcon = list(metadata = function() metadata),
      forms = "baseline_form"
    ),
    "`rcon` must provide instrument labels"
  )

  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(
        metadata,
        instruments = tibble::tibble(
          instrument_name = "other_form",
          instrument_label = "Other form"
        )
      ),
      forms = "baseline_form"
    ),
    "exactly one row"
  )

  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(
        metadata,
        instruments = tibble::tibble(
          instrument_name = c("baseline_form", "baseline_form"),
          instrument_label = c("Baseline form", "Duplicate baseline form")
        )
      ),
      forms = "baseline_form"
    ),
    "exactly one row"
  )

  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(
        metadata,
        instruments = tibble::tibble(
          instrument_name = "baseline_form",
          instrument_label = ""
        )
      ),
      forms = "baseline_form"
    ),
    "non-blank `instrument_label`"
  )
})

test_that("report object uses positive validation terminology", {
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
    "event_row_exists_checks",
    "event_row_exists_failures",
    "form_started_checks",
    "form_started_failures",
    "form_complete_checks",
    "form_complete_failures",
    "fields_complete_checks",
    "fields_complete_failures"
  ) %in% names(report)))
  expect_false(any(c(
    "event_checks",
    "event_missing",
    "form_checks",
    "form_missing",
    "any_field_checks",
    "any_field_missing",
    "expected"
  ) %in% names(report)))
  expect_false(any(c(
    "check_scope",
    "missing_scope",
    "value_present",
    "event_row_present",
    "repeat_instance_present"
  ) %in% names(report$validation_rows)))
  expect_true(all(c(
    "validation_scope",
    "field_complete",
    "event_row_exists",
    "repeat_instance_row_exists",
    "form_complete"
  ) %in% names(report$validation_rows)))
  expect_false(any(report$validation_rows$validation_scope %in% c(
    "field",
    "any_field_missing",
    "event_absent",
    "repeat_absent",
    "form_blank"
  )))
  expect_true(all(report$agent$validation_set$label %in% c(
    "Event row for record exists",
    "Form started",
    "Form complete",
    "Fields complete"
  )))
})

test_that("branch-open fields fail and branch-closed fields are not expected", {
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
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

  expect_identical(class(report), "redcapmissing")
  expect_true(any(report$missing$record_id == "r1" & report$missing$field_name == "conditional_note"))
  expect_false(any(
    report$fields_complete_checks$record_id == "r2" &
      report$fields_complete_checks$field_name == "conditional_note"
  ))
})

test_that("required fields and excluded types are applied in the expected order", {
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

  required_report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form"
  )
  all_field_report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form",
    required_fields = FALSE
  )

  expect_true(required_report$required_fields)
  expect_false("optional_note" %in% required_report$field_plan$field_name)
  expect_true("optional_note" %in% all_field_report$field_plan$field_name)
  expect_false(any(required_report$field_plan$field_type == "descriptive"))
  expect_false(any(all_field_report$field_plan$field_type == "descriptive"))
})

test_that("any-field roll-up uses the same expected-field filtering as granular checks", {
  filter_meta <- dplyr::bind_rows(
    meta_row("record_id", "filter_form", field_label = "Record ID", required = "y"),
    meta_row("started", "filter_form", field_label = "Started", required = "y"),
    meta_row("required_value", "filter_form", field_label = "Required value", required = "y"),
    meta_row("optional_value", "filter_form", field_label = "Optional value"),
    meta_row("ignored_value", "filter_form", field_label = "Ignored value", required = "y"),
    meta_row("descriptive_text", "filter_form", field_type = "descriptive", field_label = "Descriptive text", required = "y"),
    meta_row("branch_flag", "filter_form", field_type = "yesno", field_label = "Branch flag", required = "y"),
    meta_row("conditional_value", "filter_form", field_label = "Conditional value", branching = "[branch_flag] = '1'", required = "y")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    started = c("yes", "yes"),
    required_value = c("entered", "entered"),
    optional_value = c("", ""),
    ignored_value = c("", ""),
    descriptive_text = c("", ""),
    branch_flag = c("0", "1"),
    conditional_value = c("", "")
  )

  required_report <- find_missing(
    data = records,
    rcon = fake_rcon(filter_meta),
    forms = "filter_form",
    ignore_fields = "ignored_value"
  )
  all_field_report <- find_missing(
    data = records,
    rcon = fake_rcon(filter_meta),
    forms = "filter_form",
    required_fields = FALSE,
    ignore_fields = "ignored_value"
  )

  expect_equal(required_report$form_complete_failures$record_id, "r2")
  expect_false(any(required_report$fields_complete_checks$field_name %in% c(
    "optional_value",
    "ignored_value",
    "descriptive_text"
  )))
  expect_false(any(
    required_report$fields_complete_checks$record_id == "r1" &
      required_report$fields_complete_checks$field_name == "conditional_value"
  ))
  expect_setequal(all_field_report$form_complete_failures$record_id, c("r1", "r2"))
  expect_true("optional_value" %in% all_field_report$fields_complete_checks$field_name)
  expect_false(any(all_field_report$fields_complete_checks$field_name %in% c(
    "ignored_value",
    "descriptive_text"
  )))
  expect_false(any(
    all_field_report$fields_complete_checks$record_id == "r1" &
      all_field_report$fields_complete_checks$field_name == "conditional_value"
  ))
})

test_that("form complete passes only when all expected fields are complete", {
  records <- tibble::tibble(
    record_id = c("complete", "incomplete"),
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
  form_complete_summary <- report$agent$validation_set[
    report$agent$validation_set$step_id == "baseline_form_form_complete",
    ,
    drop = FALSE
  ]

  expect_equal(form_complete_summary$n, 2)
  expect_equal(form_complete_summary$n_passed, 1)
  expect_equal(form_complete_summary$n_failed, 1)
  expect_equal(report$form_complete_failures$record_id, "incomplete")
  expect_true(report$form_complete_checks$form_complete[
    report$form_complete_checks$record_id == "complete"
  ])
  expect_false(report$form_complete_checks$form_complete[
    report$form_complete_checks$record_id == "incomplete"
  ])
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
  expect_false(any(report$fields_complete_checks$record_id == "drop"))
  expect_false(any(report$fields_complete_checks$field_name %in% c("optional_note", "checkbox_field")))
})

test_that("ignore ids are removed before event-context denominators are built", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("event_1_arm_1", "event_2_arm_1"),
    form = c("status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1", "r2", "r3", "r3"),
    redcap_event_name = c(
      "event_1_arm_1",
      "event_2_arm_1",
      "event_1_arm_1",
      "event_1_arm_1",
      "event_2_arm_1"
    ),
    status_started = c("yes", "yes", "yes", "yes", "yes"),
    status_value = c("entered", "entered", "entered", "entered", "entered")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    ignore_ids = "r3"
  )
  event_summary <- report$agent$validation_set[
    report$agent$validation_set$step_id == "status_form_event_row_exists",
    ,
    drop = FALSE
  ]
  event_summary <- event_summary[order(event_summary$redcap_event_name), , drop = FALSE]

  expect_equal(event_summary$n, c(2, 2))
  expect_equal(event_summary$n_failed, c(0, 1))
  expect_false(any(report$event_row_exists_checks$record_id == "r3"))
  expect_false(any(report$form_started_checks$record_id == "r3"))
  expect_false(any(report$form_complete_checks$record_id == "r3"))
  expect_false(any(report$fields_complete_checks$record_id == "r3"))
})

test_that("whole-form blank rows are reported once per form context", {
  records <- tibble::tibble(
    record_id = c("blank", "started"),
    branch_flag = c("", "1"),
    required_note = c("", ""),
    optional_note = c("", ""),
    checkbox_field___1 = c("0", "1"),
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

  form_summary <- report$agent$validation_set[
    report$agent$validation_set$label == "Form started",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(report$form_started_failures), 1)
  expect_true(any(
    report$missing$record_id == "blank" &
      report$missing$validation_scope == "form_started"
  ))
  expect_false(any(
    report$missing$record_id == "blank" &
      report$missing$validation_scope == "fields_complete"
  ))
  expect_equal(form_summary$n, nrow(report$form_started_checks))
  expect_equal(form_summary$n_failed, nrow(report$form_started_failures))
})

test_that("event-qualified branching and missing event rows are handled", {
  event_meta <- dplyr::bind_rows(
    meta_row("record_id", "baseline_form", field_label = "Record ID", required = "y"),
    meta_row("baseline_started", "baseline_form", field_label = "Baseline started", required = "y"),
    meta_row("same_event_note", "baseline_form", field_label = "Same event note", branching = "[follow_flag] = '1'", required = "y"),
    meta_row("cross_event_note", "baseline_form", field_label = "Cross event note", branching = "[followup_event][follow_flag] = '1'", required = "y"),
    meta_row("follow_flag", "followup_form", field_type = "yesno", field_label = "Follow flag", required = "y"),
    meta_row("follow_note", "followup_form", field_label = "Follow note", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "followup_event"),
    form = c("baseline_form", "followup_form")
  )
  records <- tibble::tibble(
    record_id = c("missing_followup", "cross_open", "cross_open", "same_open", "same_open"),
    redcap_event_name = c("baseline_event", "baseline_event", "followup_event", "baseline_event", "followup_event"),
    baseline_started = c("yes", "yes", "", "yes", ""),
    follow_flag = c("", "", "1", "1", "0"),
    same_event_note = c("", "", "", "", ""),
    cross_event_note = c("", "", "", "", ""),
    follow_note = c("", "", "entered", "", "entered")
  )

  baseline_report <- find_missing(
    data = records,
    rcon = fake_rcon(event_meta, mapping = mapping),
    forms = "baseline_form"
  )
  followup_report <- find_missing(
    data = records,
    rcon = fake_rcon(event_meta, mapping = mapping),
    forms = "followup_form"
  )

  expect_true(any(baseline_report$missing$record_id == "cross_open" & baseline_report$missing$field_name == "cross_event_note"))
  expect_true(any(baseline_report$missing$record_id == "same_open" & baseline_report$missing$field_name == "same_event_note"))
  expect_true(any(
    followup_report$missing$record_id == "missing_followup" &
      followup_report$missing$validation_scope == "event_row_exists"
  ))
  expect_false(any(
    followup_report$missing$record_id == "missing_followup" &
      followup_report$missing$validation_scope == "fields_complete"
  ))
})

test_that("repeat expectations create repeat-instance checks with scoped denominators", {
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

  explicit_report <- find_missing(
    data = records,
    rcon = fake_rcon(
      repeat_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "repeat_form",
    instances = 2L
  )

  expect_warning(
    default_report <- find_missing(
      data = records,
      rcon = fake_rcon(
        repeat_meta,
        mapping = mapping,
        repeat_instrument_event = repeat_instrument_event
      ),
      forms = "repeat_form"
    ),
    "Assuming instance 1"
  )

  repeat_summary <- explicit_report$agent$validation_set[
    explicit_report$agent$validation_set$label == "Repeat instance row for record exists",
    ,
    drop = FALSE
  ]
  repeat_summary <- repeat_summary[order(repeat_summary$redcap_repeat_instance), , drop = FALSE]

  expect_equal(explicit_report$instances$repeat_form, c("1", "2"))
  expect_equal(nrow(explicit_report$repeat_instance_row_exists_checks), 4)
  expect_equal(nrow(explicit_report$repeat_instance_row_exists_failures), 1)
  expect_true(any(
    explicit_report$missing$record_id == "r1" &
      explicit_report$missing$redcap_repeat_instance == "2" &
      explicit_report$missing$validation_scope == "repeat_instance_row_exists"
  ))
  expect_equal(repeat_summary$validation_context, c(
    "event: baseline_event; repeat: 1",
    "event: baseline_event; repeat: 2"
  ))
  expect_equal(repeat_summary$n, c(2, 2))
  expect_equal(repeat_summary$n_failed, c(0, 1))
  expect_equal(default_report$instances$repeat_form, "1")
})

test_that("repeating events create repeat-instance checks with blank repeat instruments", {
  repeat_meta <- dplyr::bind_rows(
    meta_row("record_id", "repeat_event_form", field_label = "Record ID", required = "y"),
    meta_row("repeat_started", "repeat_event_form", field_label = "Repeat started", required = "y"),
    meta_row("repeat_value", "repeat_event_form", field_label = "Repeat value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = 1,
    unique_event_name = "repeat_event_arm_1",
    form = "repeat_event_form"
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "repeat_event_arm_1",
    form_name = "",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2", "r2"),
    redcap_event_name = c("repeat_event_arm_1", "repeat_event_arm_1", "repeat_event_arm_1"),
    redcap_repeat_instrument = c("", "", ""),
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
    forms = "repeat_event_form",
    instances = 2L
  )
  repeat_summary <- report$agent$validation_set[
    report$agent$validation_set$step_id == "repeat_event_form_repeat_instance_row_exists",
    ,
    drop = FALSE
  ]
  repeat_summary <- repeat_summary[order(repeat_summary$redcap_repeat_instance), , drop = FALSE]

  expect_equal(report$instances$repeat_event_form, c("1", "2"))
  expect_equal(nrow(report$repeat_instance_row_exists_checks), 4)
  expect_true(all(report$repeat_instance_row_exists_checks$redcap_repeat_instrument == ""))
  expect_equal(nrow(report$repeat_instance_row_exists_failures), 1)
  expect_true(any(
    report$repeat_instance_row_exists_failures$record_id == "r1" &
      report$repeat_instance_row_exists_failures$redcap_repeat_instance == "2"
  ))
  expect_equal(repeat_summary$validation_context, c(
    "event: repeat_event_arm_1; repeat: 1",
    "event: repeat_event_arm_1; repeat: 2"
  ))
  expect_equal(repeat_summary$n, c(2, 2))
  expect_equal(repeat_summary$n_failed, c(0, 1))
})

test_that("multi-form reports keep form-specific events and default repeat instances", {
  multi_meta <- dplyr::bind_rows(
    meta_row("record_id", "demographics", field_label = "Record ID", required = "y"),
    meta_row("demo_started", "demographics", field_label = "Demo started", required = "y"),
    meta_row("demo_age", "demographics", field_label = "Age", required = "y"),
    meta_row("imaging_started", "imaging", field_label = "Imaging started", required = "y"),
    meta_row("image_value", "imaging", field_label = "Image value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1, 1, 1),
    unique_event_name = c(
      "event_1_arm_1",
      "event_2_arm_1",
      "event_1_arm_1",
      "event_2_arm_1",
      "event_3_arm_1"
    ),
    form = c(
      "demographics",
      "demographics",
      "imaging",
      "imaging",
      "imaging"
    )
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "event_3_arm_1",
    form_name = "imaging",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1", "r1", "r2", "r2"),
    redcap_event_name = c(
      "event_1_arm_1",
      "event_2_arm_1",
      "event_3_arm_1",
      "event_1_arm_1",
      "event_2_arm_1"
    ),
    redcap_repeat_instrument = c("", "", "imaging", "", ""),
    redcap_repeat_instance = c("", "", "1", "", ""),
    demo_started = c("yes", "yes", "", "yes", ""),
    demo_age = c("42", "43", "", "51", ""),
    imaging_started = c("", "yes", "yes", "", "yes"),
    image_value = c("", "", "present", "", "present")
  )

  expect_warning(
    report <- find_missing(
      data = records,
      rcon = fake_rcon(
        multi_meta,
        mapping = mapping,
        repeat_instrument_event = repeat_instrument_event
      ),
      forms = c("imaging", "demographics"),
      required_fields = FALSE,
      events = list(imaging = c("event_2_arm_1", "event_3_arm_1"))
    ),
    "imaging"
  )

  expect_identical(report$forms, c("imaging", "demographics"))
  expect_setequal(report$events$imaging, c("event_2_arm_1", "event_3_arm_1"))
  expect_setequal(report$events$demographics, c("event_1_arm_1", "event_2_arm_1"))
  expect_equal(report$instances$imaging, "1")
  expect_null(report$instances$demographics)
  expect_true(all(c("imaging", "demographics") %in% report$agent$validation_set$form))
  expect_false(any(grepl("^form:", report$agent$validation_set$validation_context)))
  expect_identical(
    unique(report$agent$validation_set$step_id),
    c(
      "imaging_event_row_exists",
      "imaging_repeat_instance_row_exists",
      "imaging_form_started",
      "imaging_form_complete",
      "imaging_fields_complete",
      "demographics_event_row_exists",
      "demographics_form_started",
      "demographics_form_complete",
      "demographics_fields_complete"
    )
  )
  expect_true(any(
    report$missing$form == "imaging" &
      report$missing$field_name == "image_value" &
      report$missing$redcap_event_name == "event_2_arm_1"
  ))
  expect_true(any(
    report$missing$form == "imaging" &
      report$missing$validation_scope == "repeat_instance_row_exists" &
      report$missing$record_id == "r2"
  ))
  expect_true(any(
    report$missing$form == "demographics" &
      report$missing$validation_scope == "form_started" &
      report$missing$redcap_event_name == "event_2_arm_1"
  ))
})

test_that("multi-form reports accept global events and global repeat counts", {
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
    repeat_value = c("10", "20", "30"),
    screen_started = c("", "", "")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      repeat_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = c("screen_form", "repeat_form"),
    events = "baseline_event",
    instances = 2L
  )

  expect_null(report$instances$screen_form)
  expect_equal(report$instances$repeat_form, c("1", "2"))
  expect_setequal(names(report$events), c("screen_form", "repeat_form"))
  expect_true(any(report$agent$validation_set$form == "screen_form"))
  expect_true(any(report$agent$validation_set$form == "repeat_form"))
})

test_that("repeat instance vectors select exact requested instances", {
  repeat_meta <- dplyr::bind_rows(
    meta_row("record_id", "screen_form", field_label = "Record ID", required = "y"),
    meta_row("repeat_started", "repeat_form", field_label = "Repeat started", required = "y")
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
    record_id = c("r1", "r1"),
    redcap_event_name = c("baseline_event", "baseline_event"),
    redcap_repeat_instrument = c("repeat_form", "repeat_form"),
    redcap_repeat_instance = c("2", "3"),
    repeat_started = c("yes", "yes")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      repeat_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "repeat_form",
    instances = c(2L, 3L)
  )

  expect_equal(report$instances$repeat_form, c("2", "3"))
  expect_setequal(
    report$repeat_instance_row_exists_checks$redcap_repeat_instance,
    c("2", "3")
  )
  expect_false(any(report$repeat_instance_row_exists_checks$redcap_repeat_instance == "1"))
})

test_that("multi-form input validation rejects ambiguous form-specific settings", {
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

  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(baseline_form_meta()),
      forms = c("baseline_form", "baseline_form")
    ),
    "duplicate"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(baseline_form_meta()),
      forms = "baseline_form",
      events = list(other_form = "event_1_arm_1")
    ),
    "Unknown name"
  )
  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(baseline_form_meta()),
      forms = "baseline_form",
      events = list(baseline_form = character())
    ),
    "empty vector"
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

test_that("multi-event summaries are stratified by event with record denominators", {
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

  event_summary <- report$agent$validation_set[
    report$agent$validation_set$label == "Event row for record exists",
    ,
    drop = FALSE
  ]
  event_summary <- event_summary[order(event_summary$redcap_event_name), , drop = FALSE]
  any_summary <- report$agent$validation_set[
    report$agent$validation_set$label == "Form complete",
    ,
    drop = FALSE
  ]
  any_summary <- any_summary[order(any_summary$redcap_event_name), , drop = FALSE]
  field_summary <- report$agent$validation_set[
    report$agent$validation_set$label == "Fields complete",
    ,
    drop = FALSE
  ]
  field_summary <- field_summary[order(field_summary$redcap_event_name), , drop = FALSE]

  expect_equal(event_summary$validation_context, paste0("event: event_", 1:3, "_arm_1"))
  expect_equal(event_summary$n, c(2, 2, 2))
  expect_equal(event_summary$n_failed, c(0, 0, 2))
  expect_equal(any_summary$validation_context, paste0("event: event_", 1:2, "_arm_1"))
  expect_equal(any_summary$n, c(2, 2))
  expect_equal(any_summary$n_failed, c(0, 1))
  expect_equal(field_summary$n, c(6, 6))
  expect_equal(field_summary$n_failed, c(0, 1))
  expect_equal(report$form_complete_failures$record_id, "r1")
  expect_equal(report$form_complete_failures$redcap_event_name, "event_2_arm_1")
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
    report$agent$validation_set$step_id == "status_form_event_row_exists",
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
  expect_false(any(
    report$event_row_exists_checks$record_id == "r1" &
      report$event_row_exists_checks$redcap_event_name %in% c("arm_2_event_1", "arm_2_event_2")
  ))
  expect_false(any(
    report$event_row_exists_checks$record_id == "r2" &
      report$event_row_exists_checks$redcap_event_name %in% c("arm_1_event_1", "arm_1_event_2")
  ))
})

test_that("all-upstream event failures do not create any-field checks", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "anchor_form", field_label = "Record ID", required = "y"),
    meta_row("anchor_value", "anchor_form", field_label = "Anchor value", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("anchor_event_arm_1", "status_event_arm_1"),
    form = c("anchor_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    redcap_event_name = c("anchor_event_arm_1", "anchor_event_arm_1"),
    anchor_value = c("entered", "entered")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form"
  )
  validation_set <- report$agent$validation_set
  event_summary <- validation_set[
    validation_set$step_id == "status_form_event_row_exists",
    ,
    drop = FALSE
  ]

  expect_equal(event_summary$n, 2)
  expect_equal(event_summary$n_failed, 2)
  expect_equal(nrow(report$form_complete_checks), 0)
  expect_equal(nrow(report$form_complete_failures), 0)
  expect_false("status_form_form_complete" %in% validation_set$step_id)
  expect_false(any(report$missing$validation_scope == "form_complete"))
})

test_that("non-longitudinal zero event summaries use zero fractions", {
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

  report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form"
  )
  event_summary <- report$agent$validation_set[
    report$agent$validation_set$label == "Event row for record exists",
    ,
    drop = FALSE
  ]

  expect_equal(event_summary$n, 0)
  expect_equal(event_summary$f_passed, 0)
  expect_equal(event_summary$f_failed, 0)
  expect_equal(event_summary$validation_context, "overall")
})

test_that("checkbox roots are present when any child is selected", {
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    required_note = "entered",
    optional_note = "",
    checkbox_field___1 = "0",
    checkbox_field___2 = "1",
    checkbox_other = "",
    conditional_note = ""
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form",
    required_fields = FALSE
  )

  expect_false("checkbox_field" %in% report$missing$field_name)
  expect_true(any(report$missing$field_name == "checkbox_other"))
})


test_that("events restrict multi-event assessment and defaults to all offered events", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c(
      "follow_up_1_arm_1",
      "follow_up_2_arm_1",
      "follow_up_3_arm_1"
    ),
    form = c("status_form", "status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1"),
    redcap_event_name = c("follow_up_1_arm_1", "follow_up_2_arm_1"),
    status_started = c("yes", "yes"),
    status_value = c("entered", "")
  )

  report_all <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form"
  )
  report_subset <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    events = c("follow_up_1_arm_1", "follow_up_2_arm_1")
  )

  expect_setequal(
    report_all$events$status_form,
    c("follow_up_1_arm_1", "follow_up_2_arm_1", "follow_up_3_arm_1")
  )
  expect_setequal(
    report_subset$events$status_form,
    c("follow_up_1_arm_1", "follow_up_2_arm_1")
  )
  expect_true(any(report_all$event_row_exists_failures$redcap_event_name == "follow_up_3_arm_1"))
  expect_false(any(report_subset$event_row_exists_failures$redcap_event_name == "follow_up_3_arm_1"))
  expect_true(any(report_subset$missing$redcap_event_name == "follow_up_2_arm_1" & report_subset$missing$field_name == "status_value"))
})

test_that("events is ignored for single-event forms", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y"),
    meta_row("status_value", "status_form", field_label = "Status value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = 1,
    unique_event_name = "baseline_event",
    form = "status_form"
  )
  records <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "baseline_event",
    status_started = "yes",
    status_value = ""
  )

  report_default <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form"
  )
  report_ignored <- find_missing(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    forms = "status_form",
    events = "not_an_offered_event"
  )

  expect_identical(report_default$missing, report_ignored$missing)
  expect_identical(report_ignored$events$status_form, "baseline_event")
})

test_that("events must match offered events for multi-event forms", {
  status_meta <- dplyr::bind_rows(
    meta_row("record_id", "status_form", field_label = "Record ID", required = "y"),
    meta_row("status_started", "status_form", field_label = "Status started", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("follow_up_1_arm_1", "follow_up_2_arm_1"),
    form = c("status_form", "status_form")
  )
  records <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "follow_up_1_arm_1",
    status_started = "yes"
  )

  expect_error(
    find_missing(
      data = records,
      rcon = fake_rcon(status_meta, mapping = mapping),
      forms = "status_form",
      events = "not_offered_event"
    ),
    "subset of the REDCap events"
  )
})


test_that("mixed repeat and non-repeat forms only apply repeat logic on repeating events", {
  mixed_meta <- dplyr::bind_rows(
    meta_row("record_id", "mixed_form", field_label = "Record ID", required = "y"),
    meta_row("mixed_started", "mixed_form", field_label = "Mixed started", required = "y"),
    meta_row("mixed_value", "mixed_form", field_label = "Mixed value", required = "y")
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
    record_id = "r1",
    redcap_event_name = "regular_a_arm_1",
    mixed_started = "yes",
    mixed_value = "entered"
  )

  expect_warning(
    report <- find_missing(
      data = records,
      rcon = fake_rcon(
        mixed_meta,
        mapping = mapping,
        repeat_instrument_event = repeat_instrument_event
      ),
      forms = "mixed_form"
    ),
    "Assuming instance 1"
  )

  expect_equal(report$instances$mixed_form, "1")
  expect_true(all(report$repeat_instance_row_exists_checks$redcap_event_name == "repeat_b_arm_1"))
  expect_false(any(report$event_row_exists_checks$redcap_event_name == "repeat_b_arm_1"))
  expect_true(any(report$repeat_instance_row_exists_failures$redcap_event_name == "repeat_b_arm_1"))
  expect_true(any(report$event_row_exists_failures$redcap_event_name == "regular_c_arm_1"))
})

test_that("mixed repeat and non-repeat summaries separate upstream and field scopes", {
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
  validation_set <- report$agent$validation_set

  expect_identical(unique(validation_set$step_id), c(
    "mixed_form_event_row_exists",
    "mixed_form_repeat_instance_row_exists",
    "mixed_form_form_started",
    "mixed_form_form_complete",
    "mixed_form_fields_complete"
  ))

  event_summary <- validation_set[
    validation_set$step_id == "mixed_form_event_row_exists",
    ,
    drop = FALSE
  ]
  event_summary <- event_summary[order(event_summary$redcap_event_name), , drop = FALSE]
  repeat_summary <- validation_set[
    validation_set$step_id == "mixed_form_repeat_instance_row_exists",
    ,
    drop = FALSE
  ]
  repeat_summary <- repeat_summary[order(repeat_summary$redcap_repeat_instance), , drop = FALSE]
  form_summary <- validation_set[
    validation_set$step_id == "mixed_form_form_started",
    ,
    drop = FALSE
  ]
  form_regular_a <- form_summary[
    form_summary$validation_context == "event: regular_a_arm_1",
    ,
    drop = FALSE
  ]
  any_summary <- validation_set[
    validation_set$step_id == "mixed_form_form_complete",
    ,
    drop = FALSE
  ]
  any_summary <- any_summary[order(any_summary$validation_context), , drop = FALSE]
  field_summary <- validation_set[
    validation_set$step_id == "mixed_form_fields_complete",
    ,
    drop = FALSE
  ]
  field_summary <- field_summary[order(field_summary$validation_context), , drop = FALSE]

  expect_equal(event_summary$n, c(2, 2))
  expect_equal(event_summary$n_failed, c(0, 2))
  expect_equal(repeat_summary$n, c(2, 2))
  expect_equal(repeat_summary$n_failed, c(0, 2))
  expect_equal(form_regular_a$n, 2)
  expect_equal(form_regular_a$n_failed, 1)
  expect_equal(any_summary$validation_context, c(
    "event: regular_a_arm_1",
    "event: repeat_b_arm_1; repeat: 1"
  ))
  expect_equal(any_summary$n, c(1, 2))
  expect_equal(any_summary$n_failed, c(1, 1))
  expect_equal(field_summary$n, c(4, 8))
  expect_equal(field_summary$n_failed, c(1, 1))
  expect_true(any(report$missing$validation_scope == "event_row_exists"))
  expect_true(any(report$missing$validation_scope == "repeat_instance_row_exists"))
  expect_true(any(report$missing$validation_scope == "form_started"))
  expect_true(any(report$missing$validation_scope == "form_complete"))
  expect_true(any(report$missing$validation_scope == "fields_complete"))
  expect_equal(report$form_complete_failures$field_name, c("mixed_form", "mixed_form"))
  any_extract <- report$missing[
    report$missing$validation_scope == "form_complete",
    ,
    drop = FALSE
  ]
  any_extract <- any_extract[order(any_extract$validation_context), , drop = FALSE]
  expect_equal(any_extract$pointblank_step, rep("mixed_form_form_complete", 2))
  expect_equal(any_extract$validation_context, c(
    "event: regular_a_arm_1",
    "event: repeat_b_arm_1; repeat: 1"
  ))
  expect_equal(any_extract$field_name, c("mixed_form", "mixed_form"))
  expect_equal(any_extract$field_label, rep("Form complete", 2))
})

test_that("non-repeating events do not trigger repeat-instance logic for mixed forms", {
  mixed_meta <- dplyr::bind_rows(
    meta_row("record_id", "mixed_form", field_label = "Record ID", required = "y"),
    meta_row("mixed_started", "mixed_form", field_label = "Mixed started", required = "y"),
    meta_row("mixed_value", "mixed_form", field_label = "Mixed value", required = "y")
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
    record_id = "r1",
    redcap_event_name = "regular_a_arm_1",
    mixed_started = "yes",
    mixed_value = "entered"
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      mixed_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "mixed_form",
    events = c("regular_a_arm_1", "regular_c_arm_1")
  )

  expect_null(report$instances$mixed_form)
  expect_equal(nrow(report$repeat_instance_row_exists_checks), 0)
  expect_equal(nrow(report$repeat_instance_row_exists_failures), 0)
  expect_setequal(report$events$mixed_form, c("regular_a_arm_1", "regular_c_arm_1"))
  expect_false(any(report$event_row_exists_checks$redcap_event_name == "repeat_b_arm_1"))
})

test_that("instances is ignored when events exclude repeating contexts", {
  mixed_meta <- dplyr::bind_rows(
    meta_row("record_id", "mixed_form", field_label = "Record ID", required = "y"),
    meta_row("mixed_started", "mixed_form", field_label = "Mixed started", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("regular_a_arm_1", "repeat_b_arm_1"),
    form = c("mixed_form", "mixed_form")
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "repeat_b_arm_1",
    form_name = "mixed_form",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "regular_a_arm_1",
    mixed_started = "yes"
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      mixed_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "mixed_form",
    events = "regular_a_arm_1",
    instances = 2L
  )

  expect_null(report$instances$mixed_form)
  expect_equal(nrow(report$repeat_instance_row_exists_checks), 0)
})
