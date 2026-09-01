test_that("run_plan rejects malformed frozen plans", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  candidates <- list(
    schema = plan,
    fingerprint = plan,
    duplicates = plan
  )
  candidates$schema$schema_version <- 2L
  candidates$fingerprint$structure_fingerprint <- paste0(
    substr(plan$structure_fingerprint, 1L, 63L),
    if (substr(plan$structure_fingerprint, 64L, 64L) == "0") "1" else "0"
  )
  candidates$duplicates$assessible_targets <- dplyr::bind_rows(
    plan$assessible_targets,
    plan$assessible_targets
  )

  for (candidate in candidates) {
    expect_error(
      run_plan(candidate, data, rcon, progress = FALSE),
      class = "redcapmissing_error_plan"
    )
  }
})

test_that("run_plan rejects changed project identities", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  changed <- run_plan_rcon()
  info <- changed$projectInformation()
  info$project_id <- "88"
  changed$projectInformation <- function() info

  expect_error(
    run_plan(plan, data, changed, progress = FALSE),
    class = "redcapmissing_error_plan"
  )
})

test_that("run_plan rejects malformed runtime structural identifiers", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  invalid_data <- data
  invalid_data$record_id <- " 1"

  expect_error(
    run_plan(plan, invalid_data, rcon, progress = FALSE),
    class = "redcapmissing_error_schema"
  )
})

test_that("plan_from_data rejects list and matrix columns anywhere in data", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  invalid_columns <- list(
    I(list("unrelated")),
    matrix("unrelated", nrow = 1L)
  )

  for (column in invalid_columns) {
    invalid_data <- data
    invalid_data$unrelated_extra <- column

    expect_error(
      plan_from_data(invalid_data, rcon, "baseline_form"),
      class = "redcapmissing_error_schema"
    )
  }
})

test_that("run_plan rejects list and matrix columns anywhere in data", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  invalid_columns <- list(
    I(list("unrelated")),
    matrix("unrelated", nrow = 1L)
  )

  for (column in invalid_columns) {
    invalid_data <- data
    invalid_data$unrelated_extra <- column

    expect_error(
      run_plan(plan, invalid_data, rcon, progress = FALSE),
      class = "redcapmissing_error_schema"
    )
  }
})

test_that("plan_from_data rejects duplicated names anywhere in data", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  duplicate_data <- data
  duplicate_data$first_duplicate <- "a"
  duplicate_data$second_duplicate <- "b"
  names(duplicate_data)[(ncol(duplicate_data) - 1L):ncol(duplicate_data)] <-
    c("duplicate", "duplicate")

  expect_error(
    plan_from_data(duplicate_data, rcon, "baseline_form"),
    class = "redcapmissing_error_schema"
  )
})

test_that("run_plan rejects duplicated names anywhere in data", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  duplicate_data <- data
  duplicate_data$first_duplicate <- "a"
  duplicate_data$second_duplicate <- "b"
  names(duplicate_data)[(ncol(duplicate_data) - 1L):ncol(duplicate_data)] <-
    c("duplicate", "duplicate")

  expect_error(
    run_plan(plan, duplicate_data, rcon, progress = FALSE),
    class = "redcapmissing_error_schema"
  )
})

test_that("run_plan scopes runtime response requirements to frozen target instruments", {
  rcon <- run_plan_target_scope_rcon()
  planner_data <- run_plan_target_scope_data()
  plan <- plan_from_data(
    planner_data,
    rcon,
    c("active_form", "inactive_form")
  )
  runtime_data <- planner_data[
    ,
    !names(planner_data) %in% c(
      "inactive_note",
      "inactive_checkbox___1",
      "inactive_checkbox___2"
    ),
    drop = FALSE
  ]

  expect_identical(plan$instruments, c("active_form", "inactive_form"))
  expect_identical(
    unique(plan$assessible_targets$instrument),
    "active_form"
  )
  expect_false(any(grepl("^inactive_checkbox___", names(runtime_data))))

  result <- run_plan(
    plan,
    runtime_data,
    rcon,
    details = TRUE,
    progress = FALSE
  )
  expect_identical(result$target_results$instrument, "active_form")
  expect_identical(result$target_results$instrument_started, "passed")
  expect_identical(result$target_results$field_complete, "passed")
  expect_false("inactive_form" %in% result$summary$instrument)
  expect_false("inactive_form" %in% result$missing$instrument)
  expect_false("inactive_form" %in% result$details$instrument)

  missing_detection_field <- runtime_data[
    ,
    names(runtime_data) != "active_start",
    drop = FALSE
  ]
  expect_error(
    run_plan(plan, missing_detection_field, rcon, progress = FALSE),
    regexp = "instrument start detection.*active_start",
    class = "redcapmissing_error_schema"
  )

  missing_target_field <- runtime_data[
    ,
    names(runtime_data) != "active_value",
    drop = FALSE
  ]
  expect_error(
    run_plan(plan, missing_target_field, rcon, progress = FALSE),
    regexp = "active_value",
    class = "redcapmissing_error_schema"
  )
})

test_that("run_plan retains non-target branching dependencies and drops unrelated fields", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "target_form", required = "y"),
    meta_row("target_start", "target_form"),
    meta_row(
      "target_value",
      "target_form",
      branching = "[context_flag] = '1'",
      required = "y"
    ),
    meta_row("context_flag", "context_form"),
    meta_row("context_unused", "context_form")
  )
  rcon <- redcap_api_connection_fixture(list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = c("target_form", "context_form"),
      instrument_label = c("Target form", "Context form")
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77",
      is_longitudinal = 0L
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(),
      form_name = character()
    ),
    version = function() "15.0.0"
  ))
  planner_data <- tibble::tibble(
    record_id = "1",
    target_start = "started",
    target_value = "",
    context_flag = "1",
    context_unused = "not needed"
  )
  plan <- plan_from_data(planner_data, rcon, "target_form")
  runtime_data <- planner_data[
    ,
    names(planner_data) != "context_unused",
    drop = FALSE
  ]

  result <- run_plan(
    plan,
    runtime_data,
    rcon,
    details = TRUE,
    progress = FALSE
  )
  expect_identical(result$target_results$instrument, "target_form")
  expect_identical(result$target_results$field_complete, "failed")
  expect_true("target_value" %in% result$details$field_name)

  missing_dependency <- runtime_data[
    ,
    names(runtime_data) != "context_flag",
    drop = FALSE
  ]
  expect_error(
    run_plan(plan, missing_dependency, rcon, progress = FALSE),
    regexp = "branching logic evaluation.*context_flag",
    class = "redcapmissing_error_schema"
  )
})

test_that("run_plan rejects incomplete metadata before field resolution", {
  rcon <- run_plan_rcon(); data <- run_plan_data()
  incomplete <- rcon
  incomplete$metadata <- function() run_plan_metadata()[, setdiff(
    names(run_plan_metadata()), "field_type"
  )]
  plan <- plan_from_data(data, rcon, "baseline_form")
  expect_error(
    run_plan(plan, data, incomplete, progress = FALSE),
    regexp = "field_type",
    class = "redcapmissing_error_schema"
  )
})

test_that("normalized structural IDs preserve a raw record_id response field", {
  metadata <- dplyr::bind_rows(
    meta_row("study_id", "baseline_form", required = "y"),
    meta_row("record_id", "baseline_form", required = "y"),
    meta_row("start_marker", "baseline_form", required = "y")
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = "baseline_form",
      instrument_label = "Baseline form"
    ),
    projectInformation = function() tibble::tibble(
      project_id = 77L,
      is_longitudinal = 0L,
      record_id_field = "study_id"
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(),
      form_name = character()
    )
  )
  rcon <- redcap_api_connection_fixture(rcon)
  data <- tibble::tibble(
    study_id = "A1",
    record_id = "",
    start_marker = "started"
  )

  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)
  record_field <- result$details[
    result$details$validation_check == "field-complete" &
      result$details$field_name == "record_id",
    ,
    drop = FALSE
  ]

  expect_identical(result$target_results$record_id, "A1")
  expect_identical(result$target_results$instrument_started, "passed")
  expect_identical(record_field$value_summary, "")
  expect_identical(record_field$effective_disposition, "failed")
  expect_true("record_id" %in% result$missing$field_name)
})

test_that("runner reads every project structure surface once", {
  base_rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, base_rcon, "baseline_form")
  counted <- run_plan_rcon()
  counts <- new.env(parent = emptyenv())
  surfaces <- c(
    "metadata", "instruments", "projectInformation", "repeatInstrumentEvent"
  )
  for (surface in surfaces) {
    original <- counted[[surface]]
    counted[[surface]] <- local({
      surface_name <- surface
      surface_function <- original
      function() {
        current <- if (exists(surface_name, counts, inherits = FALSE)) {
          get(surface_name, counts, inherits = FALSE)
        } else {
          0L
        }
        assign(surface_name, current + 1L, counts)
        surface_function()
      }
    })
  }
  counted$exportRecords <- function(...) {
    assign("exportRecords", 1L, counts)
    stop("runner must not export records")
  }

  expect_s3_class(
    run_plan(plan, data, counted, progress = FALSE),
    "redcapmissing"
  )
  expect_identical(
    vapply(surfaces, function(surface) get(surface, counts), integer(1)),
    stats::setNames(rep(1L, length(surfaces)), surfaces)
  )
  expect_false(exists("exportRecords", counts, inherits = FALSE))
})

test_that("newer data snapshots cannot add frozen targets", {
  rcon <- run_plan_rcon()
  planned_data <- run_plan_data(record_id = "1")
  plan <- plan_from_data(planned_data, rcon, "baseline_form")
  newer_data <- dplyr::bind_rows(
    planned_data,
    run_plan_data(record_id = "2")
  )

  newer <- run_plan(plan, newer_data, rcon, progress = FALSE)
  expect_identical(newer$target_results$record_id, "1")
  expect_identical(nrow(newer$target_results), 1L)
})

test_that("explicit plans with no targets are accepted at runtime", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  schedule <- run_plan_explicit_schedule()[0, ]
  plan <- plan_explicit(data, rcon, schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_equal(nrow(result$target_results), 0L)
})

test_that("zero-target classic plans require only the record identifier", {
  rcon <- run_plan_rcon()
  metadata <- rcon$metadata()
  metadata$required_field <- NULL
  rcon$metadata <- function() metadata
  empty_schedule <- run_plan_explicit_schedule()[0, , drop = FALSE]
  plan <- plan_explicit(
    run_plan_data(),
    rcon,
    empty_schedule
  )

  result <- run_plan(
    plan,
    tibble::tibble(record_id = "1"),
    rcon,
    progress = FALSE
  )
  expect_identical(nrow(result$target_results), 0L)
})

test_that("zero-target longitudinal plans require structural identifiers", {
  rcon <- run_plan_target_scope_rcon()
  plan <- plan_from_data(
    run_plan_target_scope_data(),
    rcon,
    "inactive_form"
  )
  expect_identical(nrow(plan$assessible_targets), 0L)

  result <- run_plan(
    plan,
    tibble::tibble(
      record_id = "1",
      redcap_event_name = "baseline_arm_1"
    ),
    rcon,
    progress = FALSE
  )
  expect_identical(nrow(result$target_results), 0L)
})
