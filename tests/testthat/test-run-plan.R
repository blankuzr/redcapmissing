serialized_report_contains <- function(x, text) {
  serialized <- serialize(x, connection = NULL)
  needle <- charToRaw(text)
  if (length(serialized) < length(needle)) {
    return(FALSE)
  }
  starts <- seq_len(length(serialized) - length(needle) + 1L)
  any(vapply(
    starts,
    function(start) {
      identical(
        serialized[seq.int(start, length.out = length(needle))],
        needle
      )
    },
    logical(1)
  ))
}

test_that("run_plan returns a redcapmissing object whose components follow the documented order", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE, details = TRUE)

  expect_s3_class(result, "redcapmissing")
  expect_identical(names(result), c(
    "plan", "target_results", "summary", "missing", "verification",
    "diagnostics", "details"
  ))
})

test_that("run_plan records all twelve diagnostics stages", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(result$diagnostics$stage, 1:12)
  expect_true(all(result$diagnostics$completed))
  expect_identical(result$diagnostics$operation, c(
    "Validate plan, data, and rcon",
    "Validate and normalize verification",
    "Resolve instrument-start fields",
    "Resolve field-complete fields",
    "Join assessible_targets to physical rows",
    "Run event-row-started",
    "Run repeat-instance-row-started",
    "Run instrument-started",
    "Run raw field-complete",
    "Apply verification",
    "Aggregate effective results",
    "Construct the report"
  ))
})

test_that("run_plan retains no source data, verification rows, connection, or token", {
  rcon <- run_plan_rcon()
  sentinels <- c(
    response = "SYNTHETIC_RESPONSE_SENTINEL_7_0_0",
    verification_extra = "SYNTHETIC_VERIFICATION_EXTRA_SENTINEL_7_0_0",
    connection = "SYNTHETIC_CONNECTION_SENTINEL_7_0_0",
    token = "SYNTHETIC_TOKEN_SENTINEL_7_0_0"
  )
  rcon$connection_sentinel <- sentinels[["connection"]]
  rcon$token <- sentinels[["token"]]
  data <- run_plan_data(required_note = sentinels[["response"]])
  plan <- plan_from_data(data, rcon, "baseline_form")
  verified <- run_plan_verified_row()
  verified$ignored_extra <- sentinels[["verification_extra"]]
  run <- function(details) {
    run_plan(
      plan,
      data,
      rcon,
      verified = verified,
      verified_user = "alice",
      details = details,
      progress = FALSE
    )
  }
  compact <- run(FALSE)
  detailed <- run(TRUE)

  for (result in list(compact, detailed)) {
    expect_false(any(vapply(result, identical, logical(1), data)))
    expect_false(any(vapply(result, identical, logical(1), verified)))
    expect_false(any(vapply(result, identical, logical(1), rcon)))
    expect_false(
      any(c("data", "rcon", "token", "verified") %in% names(result))
    )
    expect_false(serialized_report_contains(
      result,
      sentinels[["verification_extra"]]
    ))
    expect_false(serialized_report_contains(
      result,
      sentinels[["connection"]]
    ))
    expect_false(serialized_report_contains(result, sentinels[["token"]]))
  }

  expect_false(serialized_report_contains(compact, sentinels[["response"]]))
  expect_true(serialized_report_contains(detailed, sentinels[["response"]]))
  expect_identical(
    detailed$details$value_summary[
      detailed$details$field_name %in% "required_note"
    ],
    sentinels[["response"]]
  )
})

test_that("progress rejects a missing logical control", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(plan, data, rcon, progress = NA),
    class = "redcapmissing_error_argument"
  )
})

test_that("details enforces a scalar logical control", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  invalid <- list(NA, logical(), c(TRUE, FALSE), 1, "TRUE", NULL)

  for (value in invalid) {
    expect_error(
      run_plan(plan, data, rcon, details = value, progress = FALSE),
      class = "redcapmissing_error_argument"
    )
  }
})

test_that("progress enforces a scalar logical control", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  invalid <- list(NA, logical(), c(TRUE, FALSE), 1, "TRUE", NULL)

  for (value in invalid) {
    expect_error(
      run_plan(plan, data, rcon, progress = value),
      class = "redcapmissing_error_argument"
    )
  }
})

test_that("progress updates every stage without changing report values", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  expect_silent(run_plan(plan, data, rcon, progress = FALSE))
  baseline <- run_plan(plan, data, rcon, progress = FALSE)

  events <- character()
  testthat::local_mocked_bindings(
    cli_progress_bar = function(...) {
      events <<- c(events, "bar")
      "mock-progress"
    },
    cli_progress_update = function(...) {
      events <<- c(events, "update")
      invisible(NULL)
    },
    cli_progress_done = function(...) {
      events <<- c(events, "done")
      invisible(NULL)
    },
    .package = "cli"
  )
  progressed <- run_plan(plan, data, rcon, progress = TRUE)

  expect_identical(sum(events == "update"), 12L)
  expect_identical(tail(events, 1L), "done")
  expect_identical(progressed$target_results, baseline$target_results)
  expect_identical(progressed$summary, baseline$summary)
  expect_identical(progressed$missing, baseline$missing)
})

test_that("progress cleanup runs after validation errors", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  events <- character()
  testthat::local_mocked_bindings(
    cli_progress_bar = function(...) {
      events <<- c(events, "bar")
      "mock-progress"
    },
    cli_progress_update = function(...) {
      events <<- c(events, "update")
      invisible(NULL)
    },
    cli_progress_done = function(...) {
      events <<- c(events, "done")
      invisible(NULL)
    },
    .package = "cli"
  )
  broken <- data[, names(data) != "optional_note", drop = FALSE]

  expect_error(
    run_plan(plan, broken, rcon, progress = TRUE),
    class = "redcapmissing_error_schema"
  )
  expect_identical(tail(events, 1L), "done")
})
