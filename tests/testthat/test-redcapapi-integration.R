test_that("genuine redcapAPI offline connections support the plan-and-run workflow", {
  metadata_prototype <- get(
    "REDCAP_METADATA_STRUCTURE",
    envir = asNamespace("redcapAPI")
  )
  metadata <- as.data.frame(
    lapply(metadata_prototype, function(x) rep("", 3L)),
    stringsAsFactors = FALSE
  )
  metadata$field_name <- c("record_id", "start_marker", "value")
  metadata$form_name <- rep("baseline", 3L)
  metadata$field_type <- rep("text", 3L)
  metadata$field_label <- c("Record ID", "Start marker", "Value")
  metadata$required_field <- c("y", "", "y")

  project_structure <- get(
    "redcapProjectInformationStructure",
    envir = asNamespace("redcapAPI")
  )
  project_prototype <- project_structure("14.4.0")
  project <- as.data.frame(
    lapply(project_prototype, function(x) ""),
    stringsAsFactors = FALSE
  )
  project$project_id <- "77"
  project$is_longitudinal <- "0"
  project$has_repeating_instruments_or_events <- "0"

  repeat_configuration <- get(
    "REDCAP_REPEAT_INSTRUMENT_STRUCTURE",
    envir = asNamespace("redcapAPI")
  )
  rcon <- redcapAPI::offlineConnection(
    meta_data = metadata,
    project_info = project,
    repeat_instrument = repeat_configuration,
    url = "https://example.test/api/"
  )
  data <- tibble::tibble(record_id = "1", start_marker = "started", value = "")

  plan <- plan_from_data(data, rcon, "baseline")
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_s3_class(plan, "redcapmissing_plan")
  expect_s3_class(result, "redcapmissing")
  expect_identical(plan$assessible_targets$record_id, "1")
  expect_identical(result$target_results$field_complete, "failed")
})
