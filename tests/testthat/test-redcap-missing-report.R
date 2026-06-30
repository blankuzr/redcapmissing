test_that("public report API is stable", {
  report_args <- formals(redcap_missing_report)

  expect_setequal(
    names(report_args),
    c(
      "data",
      "rcon",
      "form",
      "desired_events",
      "required_fields",
      "ignore_fields",
      "ignore_ids",
      "exclude_types",
      "expected_repeats"
    )
  )
  expect_identical(report_args$form, quote(expr = ))
  expect_null(report_args$desired_events)
  expect_true(report_args$required_fields)
  expect_null(report_args$ignore_fields)
  expect_null(report_args$ignore_ids)
  expect_null(report_args$expected_repeats)
  expect_true(is.function(flex))
  expect_true(is.function(flex_html))
  expect_false("redcap_missing_summary" %in% getNamespaceExports("redcapmissing"))
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

  report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form",
    required_fields = FALSE
  )

  expect_identical(class(report), "redcapmissing")
  expect_true(any(report$missing$record_id == "r1" & report$missing$field_name == "conditional_note"))
  expect_false(any(report$expected$record_id == "r2" & report$expected$field_name == "conditional_note"))
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

  required_report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form"
  )
  all_field_report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form",
    required_fields = FALSE
  )

  expect_true(required_report$required_fields)
  expect_false("optional_note" %in% required_report$field_plan$field_name)
  expect_true("optional_note" %in% all_field_report$field_plan$field_name)
  expect_false(any(required_report$field_plan$field_type == "descriptive"))
  expect_false(any(all_field_report$field_plan$field_type == "descriptive"))
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

  report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form",
    required_fields = FALSE,
    ignore_fields = c("optional_note", "checkbox_field___2"),
    ignore_ids = "drop"
  )

  expect_equal(report$ignored_ids, "drop")
  expect_true(all(c("optional_note", "checkbox_field") %in% report$ignored_fields))
  expect_false(any(report$expected$record_id == "drop"))
  expect_false(any(report$expected$field_name %in% c("optional_note", "checkbox_field")))
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

  report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form",
    required_fields = FALSE
  )

  form_summary <- report$agent$validation_set[
    report$agent$validation_set$label == "Offered REDCap form has at least one entered field",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(report$form_missing), 1)
  expect_true(any(report$missing$record_id == "blank" & report$missing$missing_scope == "form_blank"))
  expect_false(any(report$missing$record_id == "blank" & report$missing$missing_scope == "field"))
  expect_equal(form_summary$n, nrow(report$form_checks))
  expect_equal(form_summary$n_failed, nrow(report$form_missing))
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

  baseline_report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(event_meta, mapping = mapping),
    form = "baseline_form"
  )
  followup_report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(event_meta, mapping = mapping),
    form = "followup_form"
  )

  expect_true(any(baseline_report$missing$record_id == "cross_open" & baseline_report$missing$field_name == "cross_event_note"))
  expect_true(any(baseline_report$missing$record_id == "same_open" & baseline_report$missing$field_name == "same_event_note"))
  expect_true(any(followup_report$missing$record_id == "missing_followup" & followup_report$missing$missing_scope == "event_absent"))
  expect_false(any(followup_report$missing$record_id == "missing_followup" & followup_report$missing$missing_scope == "field"))
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

  explicit_report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(
      repeat_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    form = "repeat_form",
    expected_repeats = 2L
  )

  expect_warning(
    default_report <- redcap_missing_report(
      data = records,
      rcon = fake_rcon(
        repeat_meta,
        mapping = mapping,
        repeat_instrument_event = repeat_instrument_event
      ),
      form = "repeat_form"
    ),
    "Assuming `expected_repeats = 1L`"
  )

  repeat_summary <- explicit_report$agent$validation_set[
    explicit_report$agent$validation_set$label == "Expected REDCap repeat instances exist for the form",
    ,
    drop = FALSE
  ]

  expect_equal(explicit_report$expected_repeats, 2L)
  expect_equal(nrow(explicit_report$repeat_checks), 4)
  expect_equal(nrow(explicit_report$repeat_missing), 1)
  expect_true(any(explicit_report$missing$record_id == "r1" & explicit_report$missing$redcap_repeat_instance == "2" & explicit_report$missing$missing_scope == "repeat_absent"))
  expect_equal(repeat_summary$n, nrow(explicit_report$repeat_checks))
  expect_equal(repeat_summary$n_failed, nrow(explicit_report$repeat_missing))
  expect_equal(default_report$expected_repeats, 1L)
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

  report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form",
    required_fields = FALSE
  )

  expect_false("checkbox_field" %in% report$missing$field_name)
  expect_true(any(report$missing$field_name == "checkbox_other"))
})


test_that("desired_events restrict multi-event assessment and defaults to all offered events", {
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

  report_all <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    form = "status_form"
  )
  report_subset <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    form = "status_form",
    desired_events = c("follow_up_1_arm_1", "follow_up_2_arm_1")
  )

  expect_setequal(
    report_all$desired_events,
    c("follow_up_1_arm_1", "follow_up_2_arm_1", "follow_up_3_arm_1")
  )
  expect_setequal(
    report_subset$desired_events,
    c("follow_up_1_arm_1", "follow_up_2_arm_1")
  )
  expect_true(any(report_all$event_missing$redcap_event_name == "follow_up_3_arm_1"))
  expect_false(any(report_subset$event_missing$redcap_event_name == "follow_up_3_arm_1"))
  expect_true(any(report_subset$missing$redcap_event_name == "follow_up_2_arm_1" & report_subset$missing$field_name == "status_value"))
})

test_that("desired_events is ignored for single-event forms", {
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

  report_default <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    form = "status_form"
  )
  report_ignored <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(status_meta, mapping = mapping),
    form = "status_form",
    desired_events = "not_an_offered_event"
  )

  expect_identical(report_default$missing, report_ignored$missing)
  expect_identical(report_ignored$desired_events, "baseline_event")
})

test_that("desired_events must match offered events for multi-event forms", {
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
    redcap_missing_report(
      data = records,
      rcon = fake_rcon(status_meta, mapping = mapping),
      form = "status_form",
      desired_events = "not_offered_event"
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
    report <- redcap_missing_report(
      data = records,
      rcon = fake_rcon(
        mixed_meta,
        mapping = mapping,
        repeat_instrument_event = repeat_instrument_event
      ),
      form = "mixed_form"
    ),
    "Assuming `expected_repeats = 1L`"
  )

  expect_equal(report$expected_repeats, 1L)
  expect_true(all(report$repeat_checks$redcap_event_name == "repeat_b_arm_1"))
  expect_false(any(report$event_checks$redcap_event_name == "repeat_b_arm_1"))
  expect_true(any(report$repeat_missing$redcap_event_name == "repeat_b_arm_1"))
  expect_true(any(report$event_missing$redcap_event_name == "regular_c_arm_1"))
})

test_that("non-repeating desired events do not trigger repeat-instance logic for mixed forms", {
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

  report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(
      mixed_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    form = "mixed_form",
    desired_events = c("regular_a_arm_1", "regular_c_arm_1")
  )

  expect_identical(report$expected_repeats, NULL)
  expect_equal(nrow(report$repeat_checks), 0)
  expect_equal(nrow(report$repeat_missing), 0)
  expect_setequal(report$desired_events, c("regular_a_arm_1", "regular_c_arm_1"))
  expect_false(any(report$event_checks$redcap_event_name == "repeat_b_arm_1"))
})

test_that("expected_repeats is ignored when desired events exclude repeating contexts", {
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

  expect_warning(
    report <- redcap_missing_report(
      data = records,
      rcon = fake_rcon(
        mixed_meta,
        mapping = mapping,
        repeat_instrument_event = repeat_instrument_event
      ),
      form = "mixed_form",
      desired_events = "regular_a_arm_1",
      expected_repeats = 2L
    ),
    "does not include any REDCap repeating-instrument events"
  )

  expect_identical(report$expected_repeats, NULL)
  expect_equal(nrow(report$repeat_checks), 0)
})

test_that("summary returns the unmodified validation-set tibble", {
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

  report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form"
  )

  summary_tbl <- summary(report)
  validation_set <- report$agent$validation_set

  expect_s3_class(summary_tbl, "summary.redcapmissing")
  expect_s3_class(summary_tbl, "tbl_df")
  expect_identical(names(summary_tbl), names(validation_set))
  expect_identical(lapply(summary_tbl, class), lapply(validation_set, class))
  expect_identical(unclass(summary_tbl), unclass(validation_set))
})

test_that("summary print method delegates to tibble printing", {
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

  report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form"
  )
  summary_tbl <- summary(report)

  expect_output(
    print_return <- print(summary_tbl),
    "# A tibble"
  )
  expect_identical(print_return, summary_tbl)
})

test_that("summary and flex helpers reject invalid inputs", {
  expect_error(
    redcapmissing:::summary.redcapmissing(list()),
    "`object` must be a `redcapmissing` object"
  )
  expect_error(
    flex(list()),
    "no applicable method"
  )
  expect_error(
    flex_html(list()),
    "`x` must be a `flextable` object"
  )
})

test_that("flex helpers return formatted table and HTML when optional packages are available", {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")
  testthat::skip_if_not_installed("htmltools")

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

  report <- redcap_missing_report(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form"
  )

  flex_out <- flex(report)
  html_out <- flex_html(flex_out)

  expect_s3_class(flex_out, "flextable")
  expect_type(html_out, "character")
  expect_true(nchar(html_out) > 0)
})
