test_that("latest user status is order independent and only VERIFIED applies", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- dplyr::bind_rows(
    run_plan_verified_row(ts = "2026-07-25 11:00:00", status = "VERIFIED"),
    run_plan_verified_row(ts = "2026-07-25 12:00:00", status = "OPEN")
  )
  run <- function(x) run_plan(
    plan, data, rcon, ignore_fields = c("record_id", "branch_flag", "checkbox_field",
      "conditional_note"), verified = x, verified_user = "alice",
    details = TRUE, progress = FALSE
  )
  first <- run(evidence); second <- run(evidence[2:1, ])
  expect_identical(first$verification, second$verification)
  expect_identical(first$verification$latest_user_rows, 1L)
  expect_identical(first$verification$verified_rows, 0L)
  expect_identical(first$target_results$field_complete, "failed")
})

test_that("conflicting latest verification ties fail closed", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- dplyr::bind_rows(
    run_plan_verified_row(status = "VERIFIED"),
    run_plan_verified_row(status = "OPEN")
  )
  expect_error(
    run_plan(plan, data, rcon, verified = evidence, verified_user = "alice",
             progress = FALSE),
    class = "redcapmissing_error_verification"
  )
})

test_that("identical latest verification duplicates collapse harmlessly", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  issue <- run_plan_verified_row()
  result <- run_plan(plan, data, rcon,
    verified = dplyr::bind_rows(issue, issue), verified_user = "alice",
    progress = FALSE)
  expect_identical(result$verification$user_rows, 2L)
  expect_identical(result$verification$latest_user_rows, 1L)
  expect_identical(result$verification$verified_rows, 1L)
})

test_that("latest duplicates collapse after native context normalization", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  first <- run_plan_verified_row()
  second <- run_plan_verified_row()
  second$event_id <- "101"
  result <- run_plan(plan, data, rcon,
      verified = dplyr::bind_rows(first, second),
      verified_user = "alice", progress = FALSE)
  expect_identical(result$verification$latest_user_rows, 1L)
  expect_identical(result$verification$overrides_applied, 1L)
})

test_that("VERIFIED status matching is exact", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- run_plan_verified_row(status = "verified")
  result <- run_plan(
    plan,
    data,
    rcon,
    verified = evidence,
    verified_user = "alice",
    progress = FALSE
  )

  expect_identical(result$verification$user_rows, 1L)
  expect_identical(result$verification$latest_user_rows, 1L)
  expect_identical(result$verification$verified_rows, 0L)
  expect_identical(result$verification$overrides_applied, 0L)
})

test_that("unmatched users return quiet zero-count contexts", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- run_plan_verified_row(username = "bob")

  expect_silent(
    result <- run_plan(
      plan,
      data,
      rcon,
      verified = evidence,
      verified_user = "alice",
      progress = FALSE
    )
  )
  expect_identical(result$verification$input_rows, 1L)
  expect_identical(result$verification$user_rows, 0L)
  expect_identical(result$verification$latest_user_rows, 0L)
  expect_identical(result$verification$verified_rows, 0L)
  expect_identical(result$verification$overrides_applied, 0L)
})

test_that("verified_user requires one nonblank unpadded character scalar", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- run_plan_verified_row()
  invalid <- list(
    character_missing = NA_character_,
    blank = "",
    whitespace = " ",
    leading_whitespace = " alice",
    trailing_whitespace = "alice ",
    length_zero = character(),
    length_two = c("alice", "bob"),
    factor = factor("alice"),
    integer = 1L,
    numeric = 1,
    logical = TRUE,
    date = as.Date("2026-07-25"),
    list = list("alice")
  )

  for (case_name in names(invalid)) {
    expect_error(
      run_plan(
        plan,
        data,
        rcon,
        verified = evidence,
        verified_user = invalid[[case_name]],
        progress = FALSE
      ),
      class = "redcapmissing_error_verification",
      info = case_name
    )
  }
})


test_that("verified_user matching is case sensitive", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "complete")
  plan <- plan_from_data(data, rcon, "baseline_form")
  issue <- run_plan_verified_row()

  result <- run_plan(
    plan,
    data,
    rcon,
    verified = issue,
    verified_user = "Alice",
    progress = FALSE
  )
  expect_identical(result$verification$user_rows, 0L)
  expect_identical(result$verification$overrides_applied, 0L)
})

test_that("later nonverified or missing statuses never fall back to old verification", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  for (status in c("OPEN", "CLOSED", "DEVERIFIED", NA_character_, "", " ")) {
    evidence <- dplyr::bind_rows(
      run_plan_verified_row(ts = "2026-07-25 11:00:00"),
      run_plan_verified_row(status = status)
    )
    evidence$query_status <- "VERIFIED"
    evidence$assigned_username <- c("alice", "bob")
    expect_silent(result <- run_plan(plan, data, rcon,
      verified = evidence, verified_user = "alice", progress = FALSE
    ))
    expect_identical(result$verification$latest_user_rows, 1L)
    expect_identical(result$verification$verified_rows, 0L)
    expect_identical(result$verification$overrides_applied, 0L)
    expect_identical(result$target_results$field_complete, "failed")
  }
})

test_that("anonymous entries have no reviewer evidence even with unusable timestamps", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  for (username in c(NA_character_, "", " ")) {
    evidence <- dplyr::bind_rows(run_plan_verified_row(),
      run_plan_verified_row(ts = NA_character_, status = NA_character_, username = username)
    )
    expect_silent(result <- run_plan(plan, data, rcon,
      verified = evidence, verified_user = "alice", progress = FALSE
    ))
    expect_identical(result$verification$input_rows, 2L)
    expect_identical(result$verification$user_rows, 1L)
    expect_identical(result$verification$overrides_applied, 1L)
  }
})

test_that("broad history matches filtered history without validating unrelated resolutions", {
  rcon <- run_plan_rcon(longitudinal = TRUE)
  data <- dplyr::mutate(run_plan_data(required_note = ""),
                       redcap_event_name = "baseline_arm_1")
  plan <- plan_from_data(data, rcon, "baseline_form")
  selected <- run_plan_verified_row()
  selected$event_id <- "101"
  selected$instance <- 1L
  outside <- selected[rep(1L, 5L), ]
  outside$record[1] <- "outside"
  outside$event_id[2] <- "999"
  outside$field_name[3] <- "unknown_field"
  outside$repeat_instrument[4] <- "other_form"
  outside$instance[5] <- 2L
  outside$ts <- "unusable timestamp"
  outside$current_query_status <- " padded status "
  run <- function(evidence) run_plan(plan, data, rcon,
    verified = evidence, verified_user = "alice", details = TRUE, progress = FALSE
  )
  filtered <- run(selected)
  expect_silent(broad <- run(dplyr::bind_rows(selected, outside)))
  for (component in c("plan", "target_results", "summary", "missing", "details")) {
    expect_identical(broad[[component]], filtered[[component]])
  }
  expect_identical(broad$verification$input_rows, 6L)
  expect_identical(broad$verification$user_rows, 6L)
  expect_identical(broad$verification$latest_user_rows, 1L)
  expect_identical(broad$verification$overrides_applied, 1L)
  outside$project_id[1] <- "78"
  expect_error(run(outside), class = "redcapmissing_error_verification")
})
