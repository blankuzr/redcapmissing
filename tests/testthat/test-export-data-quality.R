dqr_test_resolution <- function(id = "20", status_id = "10", status = "VERIFIED",
                                username = "alice", ts = "2026-07-25 12:00:00") {
  list(res_id = id, status_id = status_id, ts = ts,
       current_query_status = status, username = username,
       comment = "Synthetic review", response_requested = "0")
}

dqr_test_issue <- function(id = "10", record = "1", resolutions = list(
                            `20` = dqr_test_resolution()
                          )) {
  list(status_id = id, project_id = "77", record = record, event_id = "101",
       field_name = "required_note", repeat_instrument = NULL, instance = "1",
       query_status = "OPEN", assigned_username = "bob", resolutions = resolutions)
}

dqr_test_response <- function(history) {
  list(content = charToRaw(as.character(jsonlite::toJSON(
    history, auto_unbox = TRUE, null = "null"
  ))))
}

test_that("DQR exports use only record filters and preserve full histories", {
  requests <- list()
  issue <- dqr_test_issue(record = "001", resolutions = list(
    `20` = dqr_test_resolution(),
    `21` = dqr_test_resolution("21", status = "OPEN", ts = "2026-07-25 13:00:00"),
    `22` = dqr_test_resolution("22", username = "bob")
  ))
  issue$event_name <- "optional_module_value"
  local_mocked_bindings(makeApiCall = function(rcon, body, url, log) {
    requests[[length(requests) + 1L]] <<- list(body = body, url = url, log = log)
    dqr_test_response(list(`10` = issue))
  }, .package = "redcapAPI")
  result <- export_data_quality(run_plan_rcon())
  selected <- export_data_quality(run_plan_rcon(), c("001", "2"), "custom module")

  expect_identical(result, selected)
  expect_identical(class(result), "data.frame")
  expect_true(all(vapply(result, is.character, logical(1))))
  expect_identical(result$record, rep("001", 3))
  expect_identical(result$status_id, rep("10", 3))
  expect_identical(result$res_id, c("20", "21", "22"))
  expect_identical(result$current_query_status, c("VERIFIED", "OPEN", "VERIFIED"))
  expect_identical(result$username, c("alice", "alice", "bob"))
  expect_identical(result$repeat_instrument, rep(NA_character_, 3))
  expect_identical(result$ts[2], "2026-07-25 13:00:00")
  expect_identical(result$comment, rep("Synthetic review", 3))
  expect_identical(result$event_name, rep("optional_module_value", 3))
  expect_identical(requests[[1]]$body, list(format = "json", returnFormat = "json"))
  expect_identical(requests[[2]]$body, list(
    format = "json", returnFormat = "json", `record[1]` = "001", `record[2]` = "2"
  ))
  expect_identical(requests[[1]]$url,
    "https://example.test/api/?prefix=data_quality_api&page=export&type=module&NOAUTH&pid=77")
  expect_match(requests[[2]]$url, "prefix=custom%20module", fixed = TRUE)
  expect_false(requests[[1]]$log)
})

test_that("empty selections skip retrieval and empty exports retain core columns", {
  rcon <- run_plan_rcon()
  rcon$projectInformation <- function() stop("must not request project information")
  local_mocked_bindings(makeApiCall = function(...) stop("must not request history"),
                       .package = "redcapAPI")
  empty <- export_data_quality(rcon, character())
  expect_identical(nrow(empty), 0L)
  expect_identical(ncol(empty), 24L)
  expect_true(all(c("project_id", "record", "event_id", "field_name",
                    "repeat_instrument", "instance", "ts", "current_query_status",
                    "username", "res_id", "status_id", "comment") %in% names(empty)))
  expect_true(all(vapply(empty, is.character, logical(1))))
  for (json in c("{}", "[]")) {
    local_mocked_bindings(makeApiCall = function(...) list(content = charToRaw(json)),
                         .package = "redcapAPI")
    expect_identical(export_data_quality(run_plan_rcon()), empty)
  }
})

test_that("issues without resolutions and nullable resolution values survive export", {
  issue <- dqr_test_issue(resolutions = NULL)
  issue$resolutions <- NULL
  issue$resolutions_note <- "optional issue detail"
  other <- dqr_test_issue("11", "2", list(
    `30` = dqr_test_resolution("30", "11", status = NULL, username = NULL)
  ))
  local_mocked_bindings(makeApiCall = function(...) {
    dqr_test_response(list(`10` = issue, `11` = other))
  }, .package = "redcapAPI")
  result <- export_data_quality(run_plan_rcon())
  expect_identical(result$record, c("1", "2"))
  expect_identical(result$res_id, c(NA_character_, "30"))
  expect_identical(result$ts, c(NA_character_, "2026-07-25 12:00:00"))
  expect_identical(result$current_query_status, rep(NA_character_, 2))
  expect_identical(result$username, rep(NA_character_, 2))
  expect_identical(result$resolutions_note, c("optional issue detail", NA_character_))
})

test_that("DQR arguments reject unusable connection and selection values", {
  local_mocked_bindings(makeApiCall = function(...) stop("unexpected request"),
                       .package = "redcapAPI")
  expect_error(export_data_quality(list()), class = "redcapmissing_error_export")
  offline <- run_plan_rcon()
  class(offline) <- c("redcapOfflineConnection", class(offline))
  expect_error(export_data_quality(offline), class = "redcapmissing_error_export")
  for (value in list(NA_character_, "", " ", " 1", 1L, list("1"), factor("1"),
                    matrix("1"))) {
    expect_error(export_data_quality(run_plan_rcon(), value),
                 class = "redcapmissing_error_export")
  }
  for (value in list(NULL, character(), c("a", "b"), NA_character_, "", " ", 1L)) {
    expect_error(export_data_quality(run_plan_rcon(), prefix = value),
                 class = "redcapmissing_error_export")
  }
})

test_that("malformed DQR responses fail without revealing their content", {
  base <- list(`10` = dqr_test_issue())
  wrong_parent <- base
  wrong_parent[[1]]$resolutions[[1]]$status_id <- "11"
  wrong_id <- base
  wrong_id[[1]]$status_id <- "11"
  wrong_resolution <- base
  wrong_resolution[[1]]$resolutions[[1]]$res_id <- "99"
  wrong_project <- base
  wrong_project[[1]]$project_id <- "78"
  wrong_record <- base
  wrong_record[[1]]$record <- "2"
  missing_field <- base
  missing_field[[1]]$field_name <- NULL
  nested_field <- base
  nested_field[[1]]$extra <- list(nested = "synthetic-private-content")
  repeated_resolution <- c(base, list(`11` = dqr_test_issue("11", "1", list(
    `20` = dqr_test_resolution("20", "11")
  ))))
  overlap <- base
  overlap[[1]]$resolutions[[1]]$record <- "other"
  for (history in list(wrong_parent, wrong_id, wrong_resolution, wrong_project,
                       wrong_record, missing_field, nested_field,
                       repeated_resolution, overlap, unname(base))) {
    local_mocked_bindings(makeApiCall = function(...) dqr_test_response(history),
                         .package = "redcapAPI")
    expect_error(export_data_quality(run_plan_rcon(), "1"),
                 class = "redcapmissing_error_export")
  }
  for (json in c("not JSON: synthetic-private-content", "null",
                 '{"error":"synthetic-private-content"}', '{"10":{},"10":{}}')) {
    local_mocked_bindings(makeApiCall = function(...) list(content = charToRaw(json)),
                         .package = "redcapAPI")
    error <- tryCatch(export_data_quality(run_plan_rcon()), error = identity)
    expect_s3_class(error, "redcapmissing_error_export")
    expect_false(grepl("synthetic-private-content", conditionMessage(error), fixed = TRUE))
  }
  local_mocked_bindings(makeApiCall = function(...) stop("synthetic-private-content"),
                       .package = "redcapAPI")
  error <- tryCatch(export_data_quality(run_plan_rcon()), error = identity)
  expect_s3_class(error, "redcapmissing_error_export")
  expect_false(grepl("synthetic-private-content", conditionMessage(error), fixed = TRUE))
})

test_that("a complete mocked DQR export feeds directly into run_plan", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  history <- list(
    `10` = dqr_test_issue(),
    `11` = dqr_test_issue("11", "2", list(
      `30` = dqr_test_resolution("30", "11", ts = "not a timestamp")
    )),
    `12` = dqr_test_issue("12", resolutions = NULL)
  )
  local_mocked_bindings(makeApiCall = function(...) dqr_test_response(history),
                       .package = "redcapAPI")
  exported <- export_data_quality(rcon)
  expect_silent(result <- run_plan(plan, data, rcon,
    verified = exported, verified_user = "alice", details = TRUE, progress = FALSE
  ))
  expect_identical(result$verification[c("input_rows", "user_rows", "latest_user_rows",
                                       "verified_rows", "overrides_applied")],
                   list(input_rows = 3L, user_rows = 2L, latest_user_rows = 1L,
                        verified_rows = 1L, overrides_applied = 1L))
  field <- result$details[result$details$field_name %in% "required_note", ]
  expect_identical(field$raw_disposition, "failed")
  expect_identical(field$effective_disposition, "passed")
  expect_false(any(vapply(result, function(x) "comment" %in% names(x), logical(1))))
})
