verified_classic_fixture <- function() {
  metadata <- dplyr::bind_rows(
    meta_row(
      "record_id",
      "baseline_form",
      field_label = "Record ID",
      required = "y"
    ),
    meta_row(
      "branch_flag",
      "baseline_form",
      field_type = "yesno",
      required = "y"
    ),
    meta_row(
      "required_note",
      "baseline_form",
      required = "y"
    ),
    meta_row(
      "conditional_note",
      "baseline_form",
      branching = "[branch_flag] = '1'",
      required = "y"
    )
  )
  list(
    metadata = metadata,
    rcon = fake_rcon(
      metadata = metadata,
      project_information = tibble::tibble(
        project_id = 42L,
        is_longitudinal = 0L
      )
    ),
    data = tibble::tibble(
      record_id = c("r1", "r2"),
      branch_flag = c("1", "0"),
      required_note = c("", "entered"),
      conditional_note = c("", "")
    )
  )
}

verified_longitudinal_fixture <- function(repeat_type = "none") {
  metadata <- dplyr::bind_rows(
    meta_row(
      "record_id",
      "baseline_form",
      field_label = "Record ID",
      required = "y"
    ),
    meta_row("required_note", "baseline_form", required = "y"),
    meta_row("repeat_note", "repeat_form", required = "y"),
    meta_row("repeat_started", "repeat_form")
  )
  events <- tibble::tibble(
    event_id = c(101L, 102L),
    unique_event_name = c("baseline_arm_1", "followup_arm_1"),
    event_name = c("Baseline", "Follow-up")
  )
  mapping <- tibble::tibble(
    arm_num = c(1L, 1L, 1L),
    unique_event_name = c(
      "baseline_arm_1",
      "followup_arm_1",
      "followup_arm_1"
    ),
    form = c("baseline_form", "baseline_form", "repeat_form")
  )
  repeat_structure <- switch(
    repeat_type,
    none = tibble::tibble(),
    instrument = tibble::tibble(
      event_name = "followup_arm_1",
      form_name = "repeat_form",
      custom_form_label = NA_character_
    ),
    event = tibble::tibble(
      event_name = "followup_arm_1",
      form_name = NA_character_,
      custom_form_label = NA_character_
    )
  )
  list(
    metadata = metadata,
    events = events,
    mapping = mapping,
    repeat_structure = repeat_structure,
    rcon = fake_rcon(
      metadata = metadata,
      events = events,
      mapping = mapping,
      repeat_instrument_event = repeat_structure,
      project_information = tibble::tibble(
        project_id = 42L,
        is_longitudinal = 1L
      )
    )
  )
}

verified_issue <- function(
  field_name = "required_note",
  record = "r1",
  event_id = NA_character_,
  repeat_instrument = NA_character_,
  instance = NA_character_,
  current_query_status = "VERIFIED",
  username = "alice",
  project_id = "42"
) {
  data.frame(
    project_id = project_id,
    record = record,
    event_id = event_id,
    field_name = field_name,
    repeat_instrument = repeat_instrument,
    instance = instance,
    current_query_status = current_query_status,
    username = username,
    stringsAsFactors = FALSE
  )
}

verified_find_classic <- function(verified, verified_user = "alice", ...) {
  fixture <- verified_classic_fixture()
  find_missing(
    data = fixture$data,
    rcon = fixture$rcon,
    forms = "baseline_form",
    progress = FALSE,
    verified = verified,
    verified_user = verified_user,
    ...
  )
}

test_that("verified arguments are paired and fail closed on their input contract", {
  fixture <- verified_classic_fixture()
  issue <- verified_issue()

  expect_error(
    find_missing(
      fixture$data,
      fixture$rcon,
      "baseline_form",
      progress = FALSE,
      verified = issue
    ),
    "supplied together",
    fixed = TRUE
  )
  expect_error(
    find_missing(
      fixture$data,
      fixture$rcon,
      "baseline_form",
      progress = FALSE,
      verified_user = "alice"
    ),
    "supplied together",
    fixed = TRUE
  )
  expect_error(
    find_missing(
      fixture$data,
      fixture$rcon,
      "baseline_form",
      progress = FALSE,
      verified = list(),
      verified_user = "alice"
    ),
    "must be a data frame",
    fixed = TRUE
  )

  for (value in list("", "  ", NA_character_, character(), c("a", "b"), 1)) {
    expect_error(
      find_missing(
        fixture$data,
        fixture$rcon,
        "baseline_form",
        progress = FALSE,
        verified = issue,
        verified_user = value
      ),
      "one non-blank character value",
      fixed = TRUE
    )
  }

  expect_error(
    verified_find_classic(issue[, -1, drop = FALSE]),
    "missing required column",
    fixed = TRUE
  )

  for (column in setdiff(names(issue), "repeat_instrument")) {
    invalid <- issue
    invalid[[column]] <- factor(invalid[[column]])
    expect_error(
      verified_find_classic(invalid),
      "must be character",
      fixed = TRUE,
      info = column
    )
  }

  invalid_repeat <- issue
  invalid_repeat$repeat_instrument <- 1L
  expect_error(
    verified_find_classic(invalid_repeat),
    "must be character",
    fixed = TRUE
  )

  issue$ignored_extra <- 1L
  expect_s3_class(verified_find_classic(issue), "redcapmissing")
})

test_that("regular exportDataQuality repeat placeholders are normalized", {
  missing_repeat_values <- list(
    logical = NA,
    integer = NA_integer_,
    double = NA_real_,
    complex = NA_complex_,
    factor = factor(NA_character_),
    date = as.Date(NA)
  )

  for (missing_repeat in missing_repeat_values) {
    issue <- verified_issue(instance = "1")
    issue$repeat_instrument <- missing_repeat
    report <- verified_find_classic(issue, details = TRUE)
    field_check <- report$details$checks[["field-complete"]]
    field_check <- field_check[
      field_check$record_id == "r1" &
        field_check$field_name == "required_note",
      ,
      drop = FALSE
    ]

    expect_true(field_check$validation_passed)
    expect_identical(
      report$diagnostics$verification$overrides_applied,
      1L,
      info = typeof(missing_repeat)
    )
  }

  longitudinal <- verified_longitudinal_fixture()
  longitudinal_metadata <- dplyr::bind_rows(
    longitudinal$metadata,
    meta_row("baseline_started", "baseline_form")
  )
  longitudinal_rcon <- fake_rcon(
    metadata = longitudinal_metadata,
    events = longitudinal$events,
    mapping = longitudinal$mapping,
    repeat_instrument_event = longitudinal$repeat_structure,
    project_information = tibble::tibble(
      project_id = 42L,
      is_longitudinal = 1L
    )
  )
  data <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "baseline_arm_1",
    required_note = "",
    baseline_started = "yes"
  )
  issue <- verified_issue(event_id = "101", instance = "not-a-repeat-instance")
  issue$repeat_instrument <- NA
  report <- find_missing(
    data,
    longitudinal_rcon,
    "baseline_form",
    progress = FALSE,
    verified = issue,
    verified_user = "alice"
  )

  expect_identical(report$diagnostics$verification$overrides_applied, 1L)
})

test_that("regular placeholders and true repeat instances normalize per row", {
  fixture <- verified_longitudinal_fixture("instrument")
  metadata <- dplyr::bind_rows(
    fixture$metadata,
    meta_row("baseline_started", "baseline_form")
  )
  rcon <- fake_rcon(
    metadata = metadata,
    events = fixture$events,
    mapping = fixture$mapping,
    repeat_instrument_event = fixture$repeat_structure,
    project_information = tibble::tibble(
      project_id = 42L,
      is_longitudinal = 1L
    )
  )
  data <- tibble::tibble(
    record_id = c("r1", "r1"),
    redcap_event_name = c("baseline_arm_1", "followup_arm_1"),
    redcap_repeat_instrument = c("", "repeat_form"),
    redcap_repeat_instance = c("", "1"),
    required_note = c("", NA_character_),
    baseline_started = c("yes", NA_character_),
    repeat_note = c(NA_character_, ""),
    repeat_started = c(NA_character_, "yes")
  )
  issues <- dplyr::bind_rows(
    verified_issue(event_id = "101", instance = "1"),
    verified_issue(
      field_name = "repeat_note",
      event_id = "102",
      repeat_instrument = "repeat_form",
      instance = "1"
    )
  )

  report <- find_missing(
    data,
    rcon,
    forms = c("baseline_form", "repeat_form"),
    instances = 1L,
    progress = FALSE,
    verified = issues,
    verified_user = "alice"
  )

  expect_identical(report$diagnostics$verification$verified_rows, 2L)
  expect_identical(report$diagnostics$verification$overrides_applied, 2L)
  expect_false(any(
    report$missing$record_id == "r1" &
      report$missing$field_name %in% c("required_note", "repeat_note")
  ))
})

test_that("zero-row exportDataQuality templates warn and leave results unchanged", {
  fixture <- verified_classic_fixture()
  template <- as.data.frame(stats::setNames(
    rep(list(logical()), length(redcapmissing:::.miss_verified_columns())),
    redcapmissing:::.miss_verified_columns()
  ))
  baseline <- find_missing(
    fixture$data,
    fixture$rcon,
    "baseline_form",
    progress = FALSE
  )
  expect_warning(
    report <- find_missing(
      fixture$data,
      fixture$rcon,
      "baseline_form",
      progress = FALSE,
      verified = template,
      verified_user = "alice"
    ),
    "No rows in `verified` match",
    fixed = TRUE
  )

  expect_equal(report$summary, baseline$summary, ignore_attr = TRUE)
  expect_equal(report$missing, baseline$missing, ignore_attr = TRUE)
  expect_identical(
    report$diagnostics$verification,
    list(
      enabled = TRUE,
      verified_user = "alice",
      input_rows = 0L,
      user_rows = 0L,
      verified_rows = 0L,
      overrides_applied = 0L
    )
  )

  invalid_template <- template
  invalid_template$project_id <- numeric()
  expect_error(
    verified_find_classic(invalid_template),
    "must be character",
    fixed = TRUE
  )
})

test_that("all verified rows are validated before username and status filtering", {
  issue <- verified_issue()

  invalid <- issue
  invalid$project_id <- "999"
  invalid$username <- "other"
  invalid$current_query_status <- "OPEN"
  expect_error(
    verified_find_classic(invalid),
    "must equal the REDCap project ID",
    fixed = TRUE
  )

  fixture <- verified_classic_fixture()
  no_project <- fake_rcon(
    metadata = fixture$metadata,
    project_information = tibble::tibble(is_longitudinal = 0L)
  )
  expect_error(
    find_missing(
      fixture$data,
      no_project,
      "baseline_form",
      progress = FALSE,
      verified = issue,
      verified_user = "alice"
    ),
    "provide one non-blank `project_id`",
    fixed = TRUE
  )

  no_design <- fake_rcon(
    metadata = fixture$metadata,
    project_information = tibble::tibble(project_id = 42L)
  )
  expect_error(
    find_missing(
      fixture$data,
      no_design,
      "baseline_form",
      progress = FALSE,
      verified = issue,
      verified_user = "alice"
    ),
    "provide a valid `is_longitudinal`",
    fixed = TRUE
  )

  duplicate_metadata <- dplyr::bind_rows(
    fixture$metadata,
    fixture$metadata[fixture$metadata$field_name == "required_note", ]
  )
  duplicate_rcon <- fake_rcon(
    metadata = duplicate_metadata,
    project_information = tibble::tibble(
      project_id = 42L,
      is_longitudinal = 0L
    )
  )
  expect_error(
    find_missing(
      fixture$data,
      duplicate_rcon,
      "baseline_form",
      progress = FALSE,
      verified = issue,
      verified_user = "alice"
    ),
    "exactly one row for each verified field",
    fixed = TRUE
  )

  invalid <- issue
  invalid$record <- " "
  expect_error(
    verified_find_classic(invalid),
    "`verified$record`",
    fixed = TRUE
  )
  invalid <- issue
  invalid$field_name <- "unknown"
  expect_error(
    verified_find_classic(invalid),
    "Unknown `verified$field_name`",
    fixed = TRUE
  )
  invalid <- issue
  invalid$event_id <- "101"
  expect_error(
    verified_find_classic(invalid),
    "must be `NA_character_` for a classic project",
    fixed = TRUE
  )
  invalid <- issue
  invalid$repeat_instrument <- ""
  expect_error(
    verified_find_classic(invalid),
    "Blank `verified$repeat_instrument`",
    fixed = TRUE
  )
})

test_that("longitudinal event and form mappings are validated uniquely", {
  fixture <- verified_longitudinal_fixture()
  data <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "baseline_arm_1",
    required_note = ""
  )
  issue <- verified_issue(event_id = "101")

  expect_s3_class(
    find_missing(
      data,
      fixture$rcon,
      "baseline_form",
      progress = FALSE,
      verified = issue,
      verified_user = "alice"
    ),
    "redcapmissing"
  )

  invalid <- issue
  invalid$event_id <- "999"
  expect_error(
    find_missing(
      data,
      fixture$rcon,
      "baseline_form",
      progress = FALSE,
      verified = invalid,
      verified_user = "alice"
    ),
    "Unknown `verified$event_id`",
    fixed = TRUE
  )

  ambiguous_events <- dplyr::bind_rows(fixture$events, fixture$events[1, ])
  ambiguous_rcon <- fake_rcon(
    metadata = fixture$metadata,
    events = ambiguous_events,
    mapping = fixture$mapping,
    repeat_instrument_event = fixture$repeat_structure,
    project_information = tibble::tibble(
      project_id = 42L,
      is_longitudinal = 1L
    )
  )
  expect_error(
    find_missing(
      data,
      ambiguous_rcon,
      "baseline_form",
      progress = FALSE,
      verified = issue,
      verified_user = "alice"
    ),
    "map each verified `event_id` exactly once",
    fixed = TRUE
  )

  wrong_mapping <- fixture$mapping[
    fixture$mapping$unique_event_name != "baseline_arm_1",
    ,
    drop = FALSE
  ]
  mapping_rcon <- fake_rcon(
    metadata = fixture$metadata,
    events = fixture$events,
    mapping = wrong_mapping,
    repeat_instrument_event = fixture$repeat_structure,
    project_information = tibble::tibble(
      project_id = 42L,
      is_longitudinal = 1L
    )
  )
  expect_error(
    find_missing(
      data,
      mapping_rcon,
      "baseline_form",
      progress = FALSE,
      verified = issue,
      verified_user = "alice"
    ),
    "form is not offered",
    fixed = TRUE
  )
})

test_that("regular, repeating-instrument, and repeating-event contexts validate", {
  instrument <- verified_longitudinal_fixture("instrument")
  instrument_data <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "followup_arm_1",
    redcap_repeat_instrument = "repeat_form",
    redcap_repeat_instance = "1",
    repeat_note = "",
    repeat_started = "yes"
  )
  instrument_issue <- verified_issue(
    field_name = "repeat_note",
    event_id = "102",
    repeat_instrument = "repeat_form",
    instance = "1"
  )
  instrument_report <- find_missing(
    instrument_data,
    instrument$rcon,
    "repeat_form",
    instances = 1L,
    progress = FALSE,
    verified = instrument_issue,
    verified_user = "alice"
  )
  expect_identical(
    instrument_report$diagnostics$verification$overrides_applied,
    1L
  )

  invalid_instrument <- instrument_issue
  invalid_instrument$repeat_instrument <- "baseline_form"
  expect_error(
    find_missing(
      instrument_data,
      instrument$rcon,
      "repeat_form",
      instances = 1L,
      progress = FALSE,
      verified = invalid_instrument,
      verified_user = "alice"
    ),
    "Invalid verified repeat context",
    fixed = TRUE
  )

  invalid_instrument_instance <- instrument_issue
  invalid_instrument_instance$instance <- "01"
  expect_error(
    find_missing(
      instrument_data,
      instrument$rcon,
      "repeat_form",
      instances = 1L,
      progress = FALSE,
      verified = invalid_instrument_instance,
      verified_user = "alice"
    ),
    "canonical positive integer",
    fixed = TRUE
  )

  repeating_event <- verified_longitudinal_fixture("event")
  event_data <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "followup_arm_1",
    redcap_repeat_instrument = "",
    redcap_repeat_instance = "1",
    repeat_note = "",
    repeat_started = "yes"
  )
  event_issue <- verified_issue(
    field_name = "repeat_note",
    event_id = "102",
    instance = "1"
  )
  event_report <- find_missing(
    event_data,
    repeating_event$rcon,
    "repeat_form",
    instances = 1L,
    progress = FALSE,
    verified = event_issue,
    verified_user = "alice"
  )
  expect_identical(
    event_report$diagnostics$verification$overrides_applied,
    1L
  )

  invalid_event <- event_issue
  invalid_event$repeat_instrument <- "repeat_form"
  expect_error(
    find_missing(
      event_data,
      repeating_event$rcon,
      "repeat_form",
      instances = 1L,
      progress = FALSE,
      verified = invalid_event,
      verified_user = "alice"
    ),
    "Invalid verified repeat context",
    fixed = TRUE
  )

  invalid_event_instance <- event_issue
  invalid_event_instance$instance <- "not-an-instance"
  expect_error(
    find_missing(
      event_data,
      repeating_event$rcon,
      "repeat_form",
      instances = 1L,
      progress = FALSE,
      verified = invalid_event_instance,
      verified_user = "alice"
    ),
    "canonical positive integer",
    fixed = TRUE
  )
})

test_that("exact verified matches flip only failing assessed field checks", {
  issue <- verified_issue()
  duplicate_issue <- dplyr::bind_rows(issue, issue)
  fixture <- verified_classic_fixture()
  baseline <- find_missing(
    fixture$data,
    fixture$rcon,
    "baseline_form",
    progress = FALSE,
    details = TRUE
  )
  report <- verified_find_classic(duplicate_issue, details = TRUE)

  baseline_check <- baseline$details$checks[["field-complete"]]
  baseline_check <- baseline_check[
    baseline_check$record_id == "r1" &
      baseline_check$field_name == "required_note",
    ,
    drop = FALSE
  ]
  verified_check <- report$details$checks[["field-complete"]]
  verified_check <- verified_check[
    verified_check$record_id == "r1" &
      verified_check$field_name == "required_note",
    ,
    drop = FALSE
  ]
  expect_false(baseline_check$validation_passed)
  expect_true(verified_check$validation_passed)
  expect_false(any(
    get_missing(report)$record_id == "r1" &
      get_missing(report)$field_name == "required_note"
  ))
  expect_identical(
    report$diagnostics$verification,
    list(
      enabled = TRUE,
      verified_user = "alice",
      input_rows = 2L,
      user_rows = 2L,
      verified_rows = 2L,
      overrides_applied = 1L
    )
  )

  baseline_summary <- get_summary(
    baseline,
    validation_check = "field-complete"
  )
  report_summary <- get_summary(
    report,
    validation_check = "field-complete"
  )
  expect_equal(sum(report_summary$failed), sum(baseline_summary$failed) - 1L)
  expect_equal(sum(report_summary$passed), sum(baseline_summary$passed) + 1L)

  baseline_cache <- baseline$spec$.flex_event_forms_field_counts
  report_cache <- report$spec$.flex_event_forms_field_counts
  expect_equal(report_cache$field_assessed, baseline_cache$field_assessed)
  expect_equal(
    sum(report_cache$field_failed),
    sum(baseline_cache$field_failed) - 1L
  )

  compact <- verified_find_classic(issue, details = FALSE)
  expect_equal(compact$summary, report$summary, ignore_attr = TRUE)
  expect_equal(compact$missing, report$missing, ignore_attr = TRUE)
  expect_equal(
    compact$spec$.flex_event_forms_field_counts,
    report$spec$.flex_event_forms_field_counts
  )
})

test_that("username and VERIFIED status matching are exact and warnings are narrow", {
  no_user <- verified_issue(username = "Alice")
  expect_warning(
    report <- verified_find_classic(no_user),
    "No rows in `verified` match",
    fixed = TRUE
  )
  expect_identical(report$diagnostics$verification$user_rows, 0L)
  expect_identical(report$diagnostics$verification$overrides_applied, 0L)

  wrong_status <- verified_issue(current_query_status = "verified")
  expect_no_warning(report <- verified_find_classic(wrong_status))
  expect_identical(report$diagnostics$verification$user_rows, 1L)
  expect_identical(report$diagnostics$verification$verified_rows, 0L)
  expect_identical(report$diagnostics$verification$overrides_applied, 0L)
})

test_that("verified rows do not synthesize or bypass field assessments", {
  issue <- verified_issue(field_name = "conditional_note", record = "r2")
  branch_report <- verified_find_classic(issue, details = TRUE)
  conditional_rows <- branch_report$details$checks[["field-complete"]]
  conditional_rows <- conditional_rows[
    conditional_rows$record_id == "r2" &
      conditional_rows$field_name == "conditional_note",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(conditional_rows), 0L)
  expect_identical(
    branch_report$diagnostics$verification$overrides_applied,
    0L
  )

  ignored <- verified_find_classic(
    verified_issue(),
    ignore_fields = "required_note",
    details = TRUE
  )
  ignored_rows <- ignored$details$checks[["field-complete"]]
  expect_false("required_note" %in% ignored_rows$field_name)
  expect_identical(ignored$diagnostics$verification$overrides_applied, 0L)

  passing <- verified_find_classic(
    verified_issue(record = "r2"),
    details = TRUE
  )
  expect_identical(passing$diagnostics$verification$overrides_applied, 0L)

  unrelated_record <- verified_find_classic(
    verified_issue(record = "not_exported")
  )
  expect_identical(
    unrelated_record$diagnostics$verification$overrides_applied,
    0L
  )

  longitudinal <- verified_longitudinal_fixture()
  scoped_data <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "baseline_arm_1",
    required_note = ""
  )
  scoped <- find_missing(
    scoped_data,
    longitudinal$rcon,
    "baseline_form",
    events = "baseline_arm_1",
    progress = FALSE,
    verified = verified_issue(event_id = "102"),
    verified_user = "alice"
  )
  expect_identical(scoped$diagnostics$verification$overrides_applied, 0L)

  repeating <- verified_longitudinal_fixture("instrument")
  repeat_data <- tibble::tibble(
    record_id = "r1",
    redcap_event_name = "followup_arm_1",
    redcap_repeat_instrument = "repeat_form",
    redcap_repeat_instance = "1",
    repeat_note = "",
    repeat_started = "yes"
  )
  repeat_scope <- find_missing(
    repeat_data,
    repeating$rcon,
    "repeat_form",
    instances = 1L,
    progress = FALSE,
    verified = verified_issue(
      field_name = "repeat_note",
      event_id = "102",
      repeat_instrument = "repeat_form",
      instance = "2"
    ),
    verified_user = "alice"
  )
  expect_identical(
    repeat_scope$diagnostics$verification$overrides_applied,
    0L
  )
})

test_that("verification diagnostics are stable when the feature is disabled", {
  fixture <- verified_classic_fixture()
  report <- find_missing(
    fixture$data,
    fixture$rcon,
    "baseline_form",
    progress = FALSE
  )
  expect_identical(
    report$diagnostics$verification,
    list(
      enabled = FALSE,
      verified_user = NA_character_,
      input_rows = 0L,
      user_rows = 0L,
      verified_rows = 0L,
      overrides_applied = 0L
    )
  )
})

test_that("verification uses cached connection context and never exports records", {
  fixture <- verified_classic_fixture()
  calls <- new.env(parent = emptyenv())
  for (name in c(
    "metadata",
    "instruments",
    "events",
    "mapping",
    "repeat",
    "project"
  )) {
    calls[[name]] <- 0L
  }
  counted <- function(name, value) {
    force(name)
    force(value)
    function() {
      calls[[name]] <- calls[[name]] + 1L
      value
    }
  }
  export_calls <- 0L
  rcon <- list(
    metadata = counted("metadata", fixture$metadata),
    instruments = counted(
      "instruments",
      tibble::tibble(
        instrument_name = "baseline_form",
        instrument_label = "Baseline"
      )
    ),
    events = counted("events", tibble::tibble()),
    mapping = counted("mapping", tibble::tibble()),
    repeatInstrumentEvent = counted("repeat", tibble::tibble()),
    projectInformation = counted(
      "project",
      tibble::tibble(project_id = 42L, is_longitudinal = 0L)
    ),
    exportRecordsTyped = function(...) {
      export_calls <<- export_calls + 1L
      stop("must not export records", call. = FALSE)
    }
  )

  find_missing(
    fixture$data,
    rcon,
    "baseline_form",
    progress = FALSE,
    verified = verified_issue(),
    verified_user = "alice"
  )

  counts <- vapply(
    c("metadata", "instruments", "events", "mapping", "repeat", "project"),
    function(name) calls[[name]],
    integer(1)
  )
  expect_true(all(counts <= 1L))
  expect_identical(export_calls, 0L)
})
