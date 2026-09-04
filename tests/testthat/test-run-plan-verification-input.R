test_that("verification arguments must be supplied together", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  issue <- run_plan_verified_row()
  expect_error(run_plan(plan, data, rcon, verified = issue, progress = FALSE),
               class = "redcapmissing_error_verification")
  expect_error(run_plan(plan, data, rcon, verified_user = "alice", progress = FALSE),
               class = "redcapmissing_error_verification")
})

test_that("verification input requires the nine-column schema", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  issue <- run_plan_verified_row()

  expect_error(run_plan(plan, data, rcon, verified = issue[, -1],
                        verified_user = "alice", progress = FALSE),
               class = "redcapmissing_error_verification")
})

test_that("verification requires each of its nine columns exactly once", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- as.data.frame(run_plan_verified_row(), check.names = FALSE)
  evidence$duplicate_username <- evidence$username
  names(evidence)[ncol(evidence)] <- "username"

  expect_error(
    run_plan(
      plan,
      data,
      rcon,
      verified = evidence,
      verified_user = "alice",
      progress = FALSE
    ),
    class = "redcapmissing_error_verification"
  )
})

test_that("in-plan resolution rows are validated before user filtering", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- dplyr::bind_rows(
    run_plan_verified_row(),
    run_plan_verified_row(ts = "bad timestamp", username = "bob")
  )
  expect_error(
    run_plan(plan, data, rcon, verified = evidence, verified_user = "alice",
             progress = FALSE),
    class = "redcapmissing_error_verification"
  )
})

test_that("verification nullable columns normalize typed missing values", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  accepted_event_ids <- list(
    character_missing = NA_character_,
    integer_missing = NA_integer_,
    double_missing = NA_real_,
    logical_missing = NA,
    blank = "",
    whitespace = " "
  )

  for (case_name in names(accepted_event_ids)) {
    issue <- run_plan_verified_row()
    issue$event_id <- accepted_event_ids[[case_name]]
    issue$repeat_instrument <- NA_real_
    issue$instance <- NA_real_
    result <- run_plan(
      plan, data, rcon,
      ignore_fields = c(
        "record_id", "branch_flag", "checkbox_field", "conditional_note"
      ),
      verified = issue,
      verified_user = "alice",
      progress = FALSE
    )
    expect_identical(
      result$verification$overrides_applied,
      1L,
      info = case_name
    )
  }
})

test_that("classic verification accepts native event IDs and instance placeholders", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  nonmissing_event_ids <- list(
    character = "999",
    integer = 999L,
    double = 999
  )

  for (case_name in names(nonmissing_event_ids)) {
    issue <- run_plan_verified_row()
    issue$event_id <- nonmissing_event_ids[[case_name]]
    issue$instance <- 1L
    result <- run_plan(
        plan,
        data,
        rcon,
        verified = issue,
        verified_user = "alice",
        progress = FALSE
      )
    expect_identical(result$verification$overrides_applied, 1L, info = case_name)
  }
})

test_that("classic event IDs are validated before user and status filtering", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  selected <- run_plan_verified_row()
  selected$event_id <- "101"
  unselected <- run_plan_verified_row(status = "OPEN", username = "bob")
  unselected$event_id <- "999"
  evidence <- dplyr::bind_rows(selected, unselected)

  expect_error(
    run_plan(
      plan,
      data,
      rcon,
      verified = evidence,
      verified_user = "alice",
      progress = FALSE
    ),
    regexp = "consistent",
    class = "redcapmissing_error_verification"
  )
})

test_that("longitudinal verification omits repeat keys when no repeat applies", {
  rcon <- run_plan_rcon(longitudinal = TRUE)
  data <- dplyr::mutate(
    run_plan_data(required_note = ""),
    redcap_event_name = "baseline_arm_1"
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- run_plan_verified_row()
  evidence$project_id <- 77L
  evidence$record <- factor("1")
  evidence$event_id <- 101
  evidence$repeat_instrument <- NA_integer_
  evidence$instance <- ""

  result <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c(
      "record_id", "branch_flag", "checkbox_field", "conditional_note"
    ),
    verified = evidence,
    verified_user = "alice",
    progress = FALSE
  )
  expect_identical(result$verification$verified_rows, 1L)
  expect_identical(result$verification$overrides_applied, 1L)

  invalid <- list(
    missing_event = within(evidence, event_id <- NA_integer_),
    repeat_instrument_context = within(
      evidence,
      {
        repeat_instrument <- "baseline_form"
        instance <- 1L
      }
    ),
    repeat_event_context = within(evidence, instance <- 1L)
  )
  expect_error(
    run_plan(plan, data, rcon, verified = invalid$missing_event,
             verified_user = "alice", progress = FALSE),
    class = "redcapmissing_error_verification"
  )
  for (case_name in setdiff(names(invalid), "missing_event")) {
    expect_silent(result <- run_plan(
      plan, data, rcon, verified = invalid[[case_name]],
      verified_user = "alice", progress = FALSE
    ))
    expected <- if (case_name == "repeat_event_context") 1L else 0L
    expect_identical(result$verification$overrides_applied, expected)
  }
})

test_that("repeating event verification requires an instance and no repeat instrument", {
  rcon <- run_plan_repeat_event_rcon()
  metadata <- dplyr::bind_rows(
    rcon$metadata(),
    meta_row("diary_start", "diary")
  )
  rcon$metadata <- function() metadata
  data <- dplyr::mutate(
    run_plan_repeat_event_data(),
    diary_start = c("", "", "started")
  )
  plan <- plan_from_data(data, rcon, "diary")
  evidence <- run_plan_verified_row(
    record = 2L,
    field_name = "diary_value"
  )
  evidence$project_id <- 77
  evidence$event_id <- "102"
  evidence$repeat_instrument <- ""
  evidence$instance <- "1"

  result <- run_plan(
    plan,
    data,
    rcon,
    verified = evidence,
    verified_user = "alice",
    progress = FALSE
  )
  expect_identical(result$verification$verified_rows, 1L)
  expect_identical(result$verification$overrides_applied, 1L)

  invalid <- list(
    missing_event = within(evidence, event_id <- NA_real_),
    missing_instance = within(evidence, instance <- NA_integer_),
    repeating_instrument_context = within(
      evidence,
      repeat_instrument <- "diary"
    )
  )
  expect_error(
    run_plan(plan, data, rcon, verified = invalid$missing_event,
             verified_user = "alice", progress = FALSE),
    class = "redcapmissing_error_verification"
  )
  for (case_name in setdiff(names(invalid), "missing_event")) {
    expect_silent(result <- run_plan(
      plan, data, rcon, verified = invalid[[case_name]],
      verified_user = "alice", progress = FALSE
    ))
    expected <- if (case_name == "repeat_event_context") 1L else 0L
    expect_identical(result$verification$overrides_applied, expected)
  }
})

test_that("repeating instrument verification requires its instrument and instance", {
  repeat_table <- tibble::tibble(
    event_name = "baseline_arm_1",
    form_name = "baseline_form"
  )
  rcon <- run_plan_rcon(
    longitudinal = TRUE,
    repeat_table = repeat_table
  )
  data <- dplyr::mutate(
    run_plan_data(required_note = ""),
    redcap_event_name = "baseline_arm_1",
    redcap_repeat_instrument = "baseline_form",
    redcap_repeat_instance = 1L
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- run_plan_verified_row(record = 1)
  evidence$event_id <- 101L
  evidence$repeat_instrument <- "baseline_form"
  evidence$instance <- 1

  result <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c(
      "record_id", "branch_flag", "checkbox_field", "conditional_note"
    ),
    verified = evidence,
    verified_user = "alice",
    progress = FALSE
  )
  expect_identical(result$verification$verified_rows, 1L)
  expect_identical(result$verification$overrides_applied, 1L)

  invalid <- list(
    missing_event = within(evidence, event_id <- NA_character_),
    missing_repeat_instrument = within(
      evidence,
      repeat_instrument <- NA_real_
    ),
    missing_instance = within(evidence, instance <- "")
  )
  expect_error(
    run_plan(plan, data, rcon, verified = invalid$missing_event,
             verified_user = "alice", progress = FALSE),
    class = "redcapmissing_error_verification"
  )
  for (case_name in setdiff(names(invalid), "missing_event")) {
    expect_silent(result <- run_plan(
      plan, data, rcon, verified = invalid[[case_name]],
      verified_user = "alice", progress = FALSE
    ))
    expected <- if (case_name == "repeat_event_context") 1L else 0L
    expect_identical(result$verification$overrides_applied, expected)
  }
})

test_that("invalid verification rows are rejected before username and status filtering", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  invalid <- run_plan_verified_row(
    ts = "bad timestamp", status = "OPEN", username = "different-user"
  )
  expect_error(
    run_plan(plan, data, rcon, verified = invalid,
             verified_user = "alice", progress = FALSE),
    class = "redcapmissing_error_verification"
  )
})

test_that("mixed documented timestamp formats are normalized row by row", {
  rcon <- run_plan_rcon(); data <- dplyr::bind_rows(
    run_plan_data(record_id = "1", required_note = ""),
    run_plan_data(record_id = "2", required_note = "")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- dplyr::bind_rows(
    run_plan_verified_row(record = "1", ts = "2026-07-25 12:00:00"),
    run_plan_verified_row(record = "2", ts = "2026-07-25T12:00:00Z")
  )
  result <- run_plan(
    plan, data, rcon,
    ignore_fields = c("record_id", "branch_flag", "checkbox_field", "conditional_note"),
    verified = evidence, verified_user = "alice", progress = FALSE
  )
  expect_identical(result$verification$verified_rows, 2L)
  expect_identical(result$verification$overrides_applied, 2L)
})

test_that("nonfinite POSIXct verification timestamps fail closed", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  issue <- run_plan_verified_row()
  issue$ts <- as.POSIXct(Inf, origin = "1970-01-01", tz = "UTC")
  expect_error(
    run_plan(plan, data, rcon, verified = issue,
             verified_user = "alice", progress = FALSE),
    class = "redcapmissing_error_verification"
  )
})

test_that("verification timestamps preserve offsets and fractional ordering", {
  rcon <- run_plan_rcon()
  data <- dplyr::bind_rows(
    run_plan_data(record_id = "1", required_note = ""),
    run_plan_data(record_id = "2", required_note = "")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- dplyr::bind_rows(
    run_plan_verified_row(
      record = "1",
      ts = "2026-07-25T12:30:00Z",
      status = "OPEN"
    ),
    run_plan_verified_row(
      record = "1",
      ts = "2026-07-25T09:00:00-04:00",
      status = "VERIFIED"
    ),
    run_plan_verified_row(
      record = "2",
      ts = "2026-07-25T12:00:00.100Z",
      status = "OPEN"
    ),
    run_plan_verified_row(
      record = "2",
      ts = "2026-07-25T12:00:00.200Z",
      status = "VERIFIED"
    )
  )

  result <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c(
      "record_id",
      "branch_flag",
      "checkbox_field",
      "conditional_note"
    ),
    verified = evidence,
    verified_user = "alice",
    progress = FALSE
  )

  expect_identical(result$verification$latest_user_rows, 2L)
  expect_identical(result$verification$verified_rows, 2L)
  expect_identical(result$verification$overrides_applied, 2L)
})

test_that("verification rejects factors and NaN in structural nullable columns", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")

  invalid <- list(
    event_factor = list(column = "event_id", value = factor(NA_character_)),
    repeat_factor = list(column = "repeat_instrument", value = factor(NA_character_)),
    instance_factor = list(column = "instance", value = factor(NA_character_)),
    event_nan = list(column = "event_id", value = NaN),
    repeat_nan = list(column = "repeat_instrument", value = NaN),
    instance_nan = list(column = "instance", value = NaN)
  )
  for (case in invalid) {
    issue <- run_plan_verified_row()
    issue[[case$column]] <- case$value
    expect_error(
      run_plan(
        plan,
        data,
        rcon,
        verified = issue,
        verified_user = "alice",
        progress = FALSE
      ),
      class = "redcapmissing_error_verification"
    )
  }
})

test_that("verification extras are ignored and finite POSIXct timestamps normalize", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- run_plan_verified_row()
  evidence$ts <- as.POSIXct(
    "2026-07-25 12:00:00",
    tz = "America/New_York"
  )
  evidence$ignored_extra <- "must not enter the report"
  result <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c(
      "record_id", "branch_flag", "checkbox_field", "conditional_note"
    ),
    verified = evidence,
    verified_user = "alice",
    details = TRUE,
    progress = FALSE
  )

  expect_identical(result$verification$input_rows, 1L)
  expect_identical(result$verification$verified_rows, 1L)
  expect_identical(result$verification$overrides_applied, 1L)
  expect_false("ignored_extra" %in% names(result$details))
})

test_that("verification rejects invalid identity and text columns before filtering", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  base <- run_plan_verified_row(status = "OPEN", username = "bob")
  invalid <- list(
    project_id_missing = list("project_id", NA_character_),
    project_id_blank = list("project_id", ""),
    project_id_whitespace = list("project_id", " "),
    project_id_padded = list("project_id", " 77"),
    project_id_nan = list("project_id", NaN),
    project_id_infinite = list("project_id", Inf),
    project_id_decimal = list("project_id", 77.5),
    project_id_factor = list("project_id", factor("77")),
    project_id_logical = list("project_id", TRUE),
    project_id_list = list("project_id", list("77")),
    project_id_mismatch = list("project_id", "78"),
    record_missing = list("record", NA_character_),
    record_blank = list("record", ""),
    record_whitespace = list("record", " "),
    record_padded = list("record", " 1"),
    record_nan = list("record", NaN),
    record_infinite = list("record", -Inf),
    record_logical = list("record", TRUE),
    record_list = list("record", list("1")),
    field_missing = list("field_name", NA_character_),
    field_blank = list("field_name", ""),
    field_whitespace = list("field_name", " "),
    field_padded = list("field_name", " required_note"),
    field_factor = list("field_name", factor("required_note")),
    field_integer = list("field_name", 1L),
    field_logical = list("field_name", TRUE),
    field_list = list("field_name", list("required_note")),
    status_padded = list("current_query_status", " OPEN"),
    status_factor = list("current_query_status", factor("OPEN")),
    status_integer = list("current_query_status", 1L),
    status_logical = list("current_query_status", TRUE),
    status_list = list("current_query_status", list("OPEN")),
    username_padded = list("username", " bob"),
    username_factor = list("username", factor("bob")),
    username_integer = list("username", 1L),
    username_logical = list("username", TRUE),
    username_list = list("username", list("bob"))
  )

  for (case_name in names(invalid)) {
    issue <- base
    issue[[invalid[[case_name]][[1L]]]] <- invalid[[case_name]][[2L]]
    expect_error(
      run_plan(
        plan,
        data,
        rcon,
        verified = issue,
        verified_user = "alice",
        progress = FALSE
      ),
      class = "redcapmissing_error_verification",
      info = case_name
    )
  }
})


test_that("verification rejects every malformed timestamp before filtering", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  base <- run_plan_verified_row(status = "OPEN", username = "bob")
  invalid <- list(
    character_missing = NA_character_,
    blank = "",
    whitespace = " ",
    leading_whitespace = " 2026-07-25 12:00:00",
    trailing_whitespace = "2026-07-25 12:00:00 ",
    unparseable = "not-a-timestamp",
    incomplete = "2026-07-25",
    numeric = 1,
    integer = 1L,
    logical = TRUE,
    factor = factor("2026-07-25 12:00:00"),
    date = as.Date("2026-07-25"),
    list = list("2026-07-25 12:00:00"),
    posix_missing = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  )

  for (case_name in names(invalid)) {
    issue <- base
    issue$ts <- invalid[[case_name]]
    expect_error(
      run_plan(
        plan,
        data,
        rcon,
        verified = issue,
        verified_user = "alice",
        progress = FALSE
      ),
      class = "redcapmissing_error_verification",
      info = case_name
    )
  }
})


test_that("longitudinal verification rejects malformed event IDs before filtering", {
  rcon <- run_plan_rcon(longitudinal = TRUE)
  data <- dplyr::mutate(
    run_plan_data(required_note = ""),
    redcap_event_name = "baseline_arm_1"
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  base <- run_plan_verified_row(status = "OPEN", username = "bob")
  base$event_id <- 101L
  invalid <- list(
    character_missing = NA_character_,
    integer_missing = NA_integer_,
    blank = "",
    whitespace = " ",
    leading_whitespace = " 101",
    trailing_whitespace = "101 ",
    leading_zero = "0101",
    zero_character = "0",
    negative_character = "-1",
    decimal_character = "101.5",
    invalid_text = "event-101",
    zero = 0L,
    negative = -1L,
    decimal = 101.5,
    nan = NaN,
    infinite = Inf,
    factor = factor("101"),
    logical = TRUE,
    list = list(101L)
  )

  for (case_name in names(invalid)) {
    issue <- base
    issue$event_id <- invalid[[case_name]]
    expect_error(
      run_plan(
        plan,
        data,
        rcon,
        verified = issue,
        verified_user = "alice",
        progress = FALSE
      ),
      class = "redcapmissing_error_verification",
      info = case_name
    )
  }
})


test_that("repeating verification rejects malformed instrument and instance keys", {
  repeat_table <- tibble::tibble(
    event_name = "baseline_arm_1",
    form_name = "baseline_form"
  )
  rcon <- run_plan_rcon(longitudinal = TRUE, repeat_table = repeat_table)
  data <- dplyr::mutate(
    run_plan_data(required_note = ""),
    redcap_event_name = "baseline_arm_1",
    redcap_repeat_instrument = "baseline_form",
    redcap_repeat_instance = 1L
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  base <- run_plan_verified_row(status = "OPEN", username = "bob")
  base$event_id <- 101L
  base$repeat_instrument <- "baseline_form"
  base$instance <- 1L
  invalid_repeat_instrument <- list(
    leading_whitespace = " baseline_form",
    trailing_whitespace = "baseline_form ",
    factor = factor("baseline_form"),
    integer = 1L,
    logical = TRUE,
    list = list("baseline_form")
  )
  invalid_instance <- list(
    leading_whitespace = " 1",
    trailing_whitespace = "1 ",
    leading_zero = "01",
    zero_character = "0",
    negative_character = "-1",
    decimal_character = "1.5",
    invalid_text = "first",
    zero = 0L,
    negative = -1L,
    decimal = 1.5,
    nan = NaN,
    infinite = Inf,
    overflow = as.double(.Machine$integer.max) + 1,
    factor = factor("1"),
    logical = TRUE,
    list = list(1L)
  )

  for (case_name in names(invalid_repeat_instrument)) {
    issue <- base
    issue$repeat_instrument <- invalid_repeat_instrument[[case_name]]
    expect_error(
      run_plan(
        plan,
        data,
        rcon,
        verified = issue,
        verified_user = "alice",
        progress = FALSE
      ),
      class = "redcapmissing_error_verification",
      info = paste("repeat_instrument", case_name)
    )
  }
  for (case_name in names(invalid_instance)) {
    issue <- base
    issue$instance <- invalid_instance[[case_name]]
    expect_error(
      run_plan(
        plan,
        data,
        rcon,
        verified = issue,
        verified_user = "alice",
        progress = FALSE
      ),
      class = "redcapmissing_error_verification",
      info = paste("instance", case_name)
    )
  }
})


test_that("verification preparation batches many native field contexts", {
  record_count <- 500L
  records <- sprintf("r%04d", seq_len(record_count))
  rcon <- run_plan_rcon()
  data <- run_plan_data(record_id = records, required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- run_plan_verified_row(
    record = rep(records, each = 2L),
    ts = rep(
      c("2026-07-25T11:00:00Z", "2026-07-25 12:00:00.250"),
      record_count
    ),
    status = rep(c("OPEN", "VERIFIED"), record_count)
  )
  evidence <- evidence[rev(seq_len(nrow(evidence))), , drop = FALSE]

  prepared <- .verification_prepare_contexts(
    verified = evidence,
    verified_user = "alice",
    snapshot = .project_structure_build_snapshot(rcon),
    plan = plan
  )

  expect_identical(
    names(prepared$contexts),
    c(
      "record_id", "redcap_event_name", "repeat_instrument",
      "repeat_instance", "field_name"
    )
  )
  expect_identical(
    vapply(prepared$contexts, typeof, character(1)),
    c(
      record_id = "character",
      redcap_event_name = "character",
      repeat_instrument = "character",
      repeat_instance = "integer",
      field_name = "character"
    )
  )
  expect_identical(nrow(prepared$contexts), record_count)
  expect_setequal(prepared$contexts$record_id, records)
  expect_true(all(prepared$contexts$field_name == "required_note"))
  expect_identical(prepared$audit$input_rows, record_count * 2L)
  expect_identical(prepared$audit$user_rows, record_count * 2L)
  expect_identical(prepared$audit$latest_user_rows, record_count)
  expect_identical(prepared$audit$verified_rows, record_count)
})

test_that("run_plan accepts zero rows in verified after checking nine required names regardless of storage", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  template <- tibble::tibble(
    project_id = logical(),
    record = integer(),
    event_id = as.Date(character()),
    field_name = complex(),
    repeat_instrument = list(),
    instance = raw(),
    ts = as.POSIXct(character(), tz = "UTC"),
    current_query_status = factor(),
    username = double()
  )

  result <- expect_silent(
    run_plan(
      plan,
      data,
      rcon,
      verified = template,
      verified_user = "alice",
      progress = FALSE
    )
  )
  expect_s3_class(result, "redcapmissing")
})

test_that("verification outside the plan is ignored and counted as supplied evidence", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  expect_silent(result <- run_plan(
    plan, data, rcon, verified = run_plan_verified_row(record = "2"),
    verified_user = "alice", progress = FALSE
  ))
  expect_identical(result$verification$user_rows, 1L)
  expect_identical(result$verification$latest_user_rows, 0L)
  expect_identical(result$verification$overrides_applied, 0L)
})
