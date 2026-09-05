test_that("comparison keeps full and shared validation strata and their units", {
  reports <- comparison_reports_fixture()
  original <- reports
  comparison <- compare_reports(reports$previous, reports$current)
  expect_s3_class(comparison, "redcapmissing_comparison")
  expect_identical(reports, original)
  expect_identical(
    names(comparison),
    c(
      "plans",
      "settings",
      "target_results",
      "summary",
      "changes",
      "scope_changes",
      "verification"
    )
  )
  full <- get_summary(comparison, population = "full")
  shared <- get_summary(comparison, population = "shared")
  expect_identical(
    full$validation_check,
    rep(.registry_list_validation_checks(), 2)
  )
  expect_identical(full$previous_assessed, c(3L, 0L, 3L, 4L, 3L, 3L, 2L, 4L))
  expect_identical(full$current_assessed, c(4L, 0L, 4L, 6L, 2L, 2L, 2L, 4L))
  expect_identical(full$previous_failed, c(0L, 0L, 1L, 1L, 0L, 1L, 0L, 2L))
  expect_identical(full$current_failed, c(0L, 0L, 1L, 1L, 0L, 0L, 0L, 1L))
  expect_identical(shared$previous_assessed, c(3L, 0L, 3L, 4L, 2L, 2L, 1L, 2L))
  expect_identical(shared$current_assessed, c(3L, 0L, 3L, 6L, 2L, 2L, 2L, 4L))
  expect_identical(shared$previous_failed, c(0L, 0L, 1L, 1L, 0L, 1L, 0L, 1L))
  expect_identical(shared$current_failed, c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L))
  expect_identical(
    full$validation_level,
    rep(c("event:instrument", "event:instrument:instance"), each = 4L)
  )
  expect_true(all(is.na(full$current_fail_rate[full$current_assessed == 0L])))
  expect_identical(
    full$delta_assessed,
    full$current_assessed - full$previous_assessed
  )
  expect_equal(
    full$delta_fail_rate,
    full$current_fail_rate - full$previous_fail_rate
  )
  for (side in c("previous", "current")) {
    for (column in setdiff(
      .summary_list_columns(),
      .comparison_context_keys()
    )) {
      expect_identical(
        full[[paste0(side, "_", column)]],
        reports[[side]]$summary[[column]]
      )
    }
  }
  for (rows in list(full, shared)) {
    expect_identical(
      rows$previous_failed,
      rows$still_missing +
        rows$completed +
        rows$verified +
        rows$no_longer_assessed +
        rows$removed_from_scope
    )
    expect_identical(
      rows$current_failed,
      rows$still_missing + rows$newly_detected + rows$added_to_scope
    )
  }
  expect_true(all(
    shared$added_to_scope == 0L & shared$removed_from_scope == 0L
  ))
  expect_identical(comparison$scope_changes$record_id, c("001", "004"))
  expect_identical(comparison$scope_changes$target_scope, c("removed", "added"))
  expect_true(comparison$verification$setup_changed)
})

test_that("changes distinguish gate progress new assessments verification and scope", {
  reports <- comparison_reports_fixture()
  changes <- get_changes(compare_reports(reports$previous, reports$current))
  expect_identical(
    changes$record_id,
    c("003", "004", "001", "003", "003", "001", "002", "003")
  )
  expect_identical(
    changes$change,
    c(
      "completed",
      "added_to_scope",
      "completed",
      "newly_detected",
      "completed",
      "removed_from_scope",
      "verified",
      "newly_detected"
    )
  )
  expect_identical(
    changes$reason[changes$change == "newly_detected"],
    rep("newly assessed", 2L)
  )
  verified <- changes[changes$change == "verified", ]
  expect_identical(verified$current_raw_disposition, "failed")
  expect_identical(verified$current_effective_disposition, "passed")
  expect_true(verified$current_verification_applied)
  expect_true(all(is.na(changes$current_effective_disposition[
    changes$target_scope == "removed"
  ])))
  expect_true(all(is.na(changes$previous_effective_disposition[
    changes$target_scope == "added"
  ])))
  expect_false("value_summary" %in% names(changes))
})

test_that("branching closure and upstream regression are not field completion", {
  rcon <- run_plan_rcon()
  before_data <- run_plan_data(branch_flag = "1", required_note = "")
  plan <- plan_from_data(before_data, rcon, "baseline_form")
  before <- run_plan(plan, before_data, rcon, details = TRUE, progress = FALSE)
  after <- run_plan(
    plan,
    run_plan_data(branch_flag = "0", required_note = ""),
    rcon,
    details = TRUE,
    progress = FALSE
  )
  changes <- get_changes(
    compare_reports(before, after),
    validation_check = "field-complete"
  )
  branch <- changes[changes$field_name == "conditional_note", ]
  expect_identical(branch$change, "no_longer_assessed")
  expect_identical(branch$current_effective_disposition, "not applicable")
  expect_identical(branch$reason, "branching logic not satisfied")
  expect_true("still_missing" %in% changes$change)
  absent <- run_plan(
    plan,
    before_data[0, ],
    rcon,
    details = TRUE,
    progress = FALSE
  )
  gated <- get_changes(compare_reports(before, absent))
  expect_true(all(
    gated$change[gated$validation_check == "field-complete"] ==
      "no_longer_assessed"
  ))
  expect_true(all(
    gated$current_effective_disposition[
      gated$validation_check == "field-complete"
    ] ==
      "not reached"
  ))
  expect_identical(
    gated$change[gated$validation_check == "instrument-started"],
    "newly_detected"
  )
})

test_that("verification loss is recorded without treating it as a new blank response", {
  reports <- comparison_reports_fixture()
  current <- reports$current
  # Regenerate the exact same assessment with verification disabled.
  unverified <- comparison_reports_fixture(verification = FALSE)$current
  comparison <- compare_reports(current, unverified)
  change <- get_changes(comparison, change = "newly_detected")
  expect_identical(change$field_name, "diary_a")
  expect_identical(change$reason, "verification no longer applies")
  expect_true(comparison$verification$setup_changed)
  expect_identical(
    change$previous_raw_disposition,
    change$current_raw_disposition
  )
})

test_that("settings are canonical and legacy reports remain readable", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  before <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c("record_id", "branch_flag"),
    details = TRUE,
    progress = FALSE
  )
  after <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c("branch_flag", "record_id"),
    details = TRUE,
    progress = FALSE
  )
  expect_identical(before$settings, after$settings)
  expect_s3_class(compare_reports(before, after), "redcapmissing_comparison")
  normalized <- .comparison_normalize_settings(TRUE, NULL, character())
  expect_identical(
    normalized,
    .comparison_normalize_settings(TRUE, character(), NULL)
  )
  legacy <- before
  legacy$settings <- NULL
  expect_identical(get_summary(legacy), get_summary(before))
  expect_identical(get_missing(legacy), get_missing(before))
  expect_error(
    compare_reports(legacy, after),
    "legacy",
    class = "redcapmissing_error_comparison"
  )
  after["details"] <- list(NULL)
  expect_error(
    compare_reports(before, after),
    "details = TRUE",
    class = "redcapmissing_error_comparison"
  )
})

test_that("repeating event gates advance and regress at each validation level", {
  rcon <- run_plan_repeat_event_rcon()
  screening <- run_plan_repeat_event_data()[1, ]
  visit <- screening
  visit$redcap_event_name <- "visit_arm_1"
  visit$redcap_repeat_instance <- 1L
  visit$screen_value <- ""
  exact <- visit
  exact$redcap_repeat_instance <- 2L
  complete <- exact
  complete$diary_value <- "entered"
  data <- list(
    screening,
    dplyr::bind_rows(screening, visit),
    dplyr::bind_rows(screening, exact),
    dplyr::bind_rows(screening, complete)
  )
  schedule <- tibble::tibble(
    record_id = "1",
    instrument = "diary",
    redcap_event_name = "visit_arm_1",
    repeat_instance = 2L
  )
  plan <- plan_explicit(screening, rcon, schedule)
  reports <- lapply(data, function(x) {
    run_plan(plan, x, rcon, details = TRUE, progress = FALSE)
  })
  empty_plan <- plan_explicit(screening, rcon, schedule[0, ])
  empty <- run_plan(
    empty_plan,
    screening,
    rcon,
    details = TRUE,
    progress = FALSE
  )
  checks <- c(
    "event-row-started",
    "repeat-instance-row-started",
    "instrument-started"
  )
  for (i in 1:3) {
    added <- get_changes(compare_reports(empty, reports[[i]]))
    removed <- get_changes(compare_reports(reports[[i]], empty))
    expect_identical(added$validation_check, checks[i])
    expect_identical(added$change, "added_to_scope")
    expect_identical(removed$validation_check, checks[i])
    expect_identical(removed$change, "removed_from_scope")
    same <- get_changes(compare_reports(reports[[i]], reports[[i]]))
    expect_identical(same$validation_check, checks[i])
    expect_identical(same$change, "still_missing")
    forward <- get_changes(compare_reports(reports[[i]], reports[[i + 1L]]))
    expect_identical(
      forward$change[forward$validation_check == checks[i]],
      "completed"
    )
    reverse <- get_changes(compare_reports(reports[[i + 1L]], reports[[i]]))
    expect_identical(
      reverse$change[reverse$validation_check == checks[i]],
      "newly_detected"
    )
    if (i < 3L) {
      expect_identical(
        forward$reason[forward$validation_check == checks[i + 1L]],
        "newly assessed"
      )
      expect_identical(
        reverse$change[reverse$validation_check == checks[i + 1L]],
        "no_longer_assessed"
      )
    }
    expect_true(all(is.na(same$repeat_instrument)))
    expect_identical(same$repeat_instance, 2L)
    expect_identical(same$validation_level, "event:instrument:instance")
  }
})

test_that("current target links are preferred even when that individual field passes", {
  rcon <- run_plan_rcon()
  before_data <- run_plan_data(required_note = "", checkbox_1 = "0")
  after_data <- run_plan_data(required_note = "", checkbox_1 = "1")
  plan <- plan_from_data(before_data, rcon, "baseline_form")
  before <- run_plan(plan, before_data, rcon, details = TRUE, progress = FALSE)
  after <- run_plan(plan, after_data, rcon, details = TRUE, progress = FALSE)
  before$missing$url[] <- "https://previous.example.test/record/1"
  after$missing$url[] <- "https://current.example.test/record/1"
  changes <- get_changes(compare_reports(before, after))
  expect_identical(
    changes$url,
    rep("https://current.example.test/record/1", 2L)
  )
  expect_setequal(changes$change, c("still_missing", "completed"))
  reports <- comparison_reports_fixture()
  reverse <- compare_reports(reports$current, reports$previous)
  expect_identical(
    get_changes(reverse, change = "added_to_scope")$field_name,
    "diary_a"
  )
})

test_that("comparison rejects conflicting physical gates across instruments", {
  reports <- comparison_reports_fixture(shared_event = TRUE)
  invalid <- reports$previous
  index <- which(
    invalid$target_results$instrument == "diary" &
      invalid$target_results$redcap_event_name == "baseline_arm_1"
  )[1]
  invalid$target_results$event_row_started[index] <- "failed"
  expect_error(
    compare_reports(invalid, reports$current),
    "Event gates conflict",
    class = "redcapmissing_error_comparison"
  )
})

test_that("checkbox roots count once and provenance does not determine identity", {
  rcon <- run_plan_rcon()
  before_data <- run_plan_data(
    record_id = "001",
    checkbox_1 = "0",
    checkbox_2 = "0"
  )
  after_data <- run_plan_data(
    record_id = "001",
    checkbox_1 = "1",
    checkbox_2 = "1"
  )
  before <- run_plan(
    plan_from_data(before_data, rcon, "baseline_form"),
    before_data,
    rcon,
    details = TRUE,
    progress = FALSE
  )
  after <- run_plan(
    plan_explicit(after_data, rcon, run_plan_explicit_schedule("001")),
    after_data,
    rcon,
    details = TRUE,
    progress = FALSE
  )
  comparison <- compare_reports(before, after)
  changes <- get_changes(comparison)
  expect_identical(comparison$target_results$target_scope, "shared")
  expect_false(identical(
    comparison$target_results$previous_target_source,
    comparison$target_results$current_target_source
  ))
  expect_identical(changes$record_id, "001")
  expect_identical(changes$field_name, "checkbox_field")
  expect_identical(changes$change, "completed")
  expect_identical(
    get_summary(comparison, "field-complete", population = "shared")$completed,
    1L
  )
  before$missing$url <- "https://previous.example.test/record/001"
  after$details$value_summary[
    !is.na(after$details$field_name)
  ] <- "SYNTHETIC_RESPONSE_ONLY"
  serialized <- rawToChar(serialize(
    compare_reports(before, after),
    NULL,
    ascii = TRUE
  ))
  expect_false(grepl("SYNTHETIC_RESPONSE_ONLY", serialized, fixed = TRUE))
  expect_identical(
    get_changes(compare_reports(before, after))$url,
    before$missing$url
  )
})

test_that("comparison rejects incompatible and inconsistent stored reports", {
  reports <- comparison_reports_fixture()
  expect_error(
    compare_reports(reports$previous),
    class = "redcapmissing_error_comparison"
  )
  mutations <- list(
    function(x) {
      x$settings$required_fields <- FALSE
      x
    },
    function(x) {
      x$plan$structure_fingerprint <- strrep("a", 64)
      x
    },
    function(x) {
      x$details <- dplyr::bind_rows(x$details, x$details[1, ])
      x
    },
    function(x) {
      x$details$effective_disposition[1] <- "unknown"
      x
    },
    function(x) {
      x$target_results$fields_failed[1] <- 100L
      x
    },
    function(x) {
      x$summary$failed[1] <- 100L
      x
    },
    function(x) {
      x$missing <- x$missing[-1, ]
      x
    },
    function(x) {
      x$details$repeat_instance <- as.double(x$details$repeat_instance)
      x
    },
    function(x) {
      x$details$record_id[1] <- "outside"
      x
    }
  )
  for (mutate in mutations) {
    expect_error(
      compare_reports(reports$previous, mutate(reports$current)),
      class = "redcapmissing_error_comparison"
    )
  }
})

test_that("empty and disjoint plans retain typed outputs and scope presence", {
  rcon <- run_plan_rcon()
  data <- dplyr::bind_rows(
    run_plan_data("001", required_note = ""),
    run_plan_data("002")
  )
  run <- function(ids) {
    run_plan(
      plan_explicit(data, rcon, run_plan_explicit_schedule(ids)),
      data,
      rcon,
      details = TRUE,
      progress = FALSE
    )
  }
  before <- run("001")
  after <- run("002")
  comparison <- compare_reports(before, after)
  expect_equal(nrow(get_summary(comparison, population = "shared")), 0L)
  expect_identical(
    sort(comparison$scope_changes$target_scope),
    c("added", "removed")
  )
  expect_equal(nrow(comparison$scope_changes), 2L)
  expect_true(all(get_changes(comparison)$change == "removed_from_scope"))
  empty_plan <- plan_explicit(data, rcon, run_plan_explicit_schedule()[0, ])
  empty <- run_plan(empty_plan, data, rcon, details = TRUE, progress = FALSE)
  added <- compare_reports(empty, before)
  expect_true(all(!get_summary(added)$previous_in_scope))
  expect_true(all(get_summary(added)$previous_assessed == 0L))
  expect_true(all(is.na(get_summary(added)$previous_fail_rate)))
  both <- compare_reports(empty, empty)
  expect_identical(names(get_summary(both)), names(get_summary(comparison)))
  expect_equal(nrow(get_summary(both)), 0L)
  expect_identical(
    vapply(get_changes(both), typeof, character(1)),
    vapply(get_changes(comparison), typeof, character(1))
  )
})

test_that("comparison filters preserve strata ordering labels and serialization", {
  reports <- comparison_reports_fixture()
  comparison <- compare_reports(reports$previous, reports$current)
  selected <- get_summary(
    comparison,
    validation_check = "field-complete",
    instruments = "diary",
    population = "shared"
  )
  expect_equal(nrow(selected), 1L)
  expect_identical(selected$redcap_event_name, "followup_arm_1")
  expect_identical(selected$repeat_instrument, "diary")
  expect_identical(selected$repeat_instance, 2L)
  expect_identical(
    get_changes(comparison, change = "verified")$record_id,
    "002"
  )
  empty <- get_changes(
    comparison,
    events = "baseline_arm_1",
    instruments = "diary"
  )
  expect_equal(nrow(empty), 0L)
  expect_identical(
    attr(empty, "redcapmissing_labels"),
    attr(get_changes(comparison), "redcapmissing_labels")
  )
  for (value in list(character(), NA_character_, "FULL", " full", "unknown")) {
    expect_error(get_summary(comparison, population = value))
  }
  expect_error(get_changes(comparison, change = "unknown"))
  expect_error(get_changes(comparison, instruments = "Diary"))
  expect_identical(unserialize(serialize(comparison, NULL)), comparison)
  printed <- capture.output(print(comparison))
  expect_true(any(grepl("Full scope", printed, fixed = TRUE)))
  expect_true(any(grepl("Shared targets", printed, fixed = TRUE)))
  expect_false(any(grepl("$plans", printed, fixed = TRUE)))
})
