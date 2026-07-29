test_that("fingerprint table normalization ignores row and column order", {
  original <- tibble::tibble(
    second = c(2L, 1L),
    first = factor(c("b", "a"))
  )
  reordered <- original[2:1, c("first", "second")]

  expect_identical(
    redcapmissing:::.structure_fingerprint_encode_table(original),
    redcapmissing:::.structure_fingerprint_encode_table(reordered)
  )
})

test_that("project fingerprints are stable to table row order with explicit record identity", {
  rcon <- .plan_longitudinal_rcon()
  rcon$projectInformation <- function() tibble::tibble(
    project_id = 41L,
    is_longitudinal = 1L,
    record_id_field = "record_id"
  )
  shuffled <- rcon
  original_metadata <- rcon$metadata()
  original_instruments <- rcon$instruments()
  original_arms <- rcon$arms()
  original_events <- rcon$events()
  original_mapping <- rcon$mapping()
  shuffled$metadata <- function() original_metadata[rev(seq_len(nrow(original_metadata))), ]
  shuffled$instruments <- function() original_instruments[rev(seq_len(nrow(original_instruments))), ]
  shuffled$arms <- function() original_arms[rev(seq_len(nrow(original_arms))), ]
  shuffled$events <- function() original_events[rev(seq_len(nrow(original_events))), ]
  shuffled$mapping <- function() original_mapping[rev(seq_len(nrow(original_mapping))), ]

  first <- redcapmissing:::.project_structure_build_snapshot(rcon)
  second <- redcapmissing:::.project_structure_build_snapshot(shuffled)
  expect_identical(first$project, second$project)
  expect_identical(first$structure_fingerprint, second$structure_fingerprint)
  fingerprint_input <- list(
    project = first$project,
    metadata = redcapmissing:::.structure_fingerprint_encode_table(first$metadata),
    instruments = redcapmissing:::.structure_fingerprint_encode_table(first$instruments),
    arms = redcapmissing:::.structure_fingerprint_encode_table(first$arms),
    events = redcapmissing:::.structure_fingerprint_encode_table(first$events),
    mapping = redcapmissing:::.structure_fingerprint_encode_table(first$mapping),
    repeat_configuration = redcapmissing:::.structure_fingerprint_encode_table(first$repeat_configuration)
  )
  expect_identical(
    first$structure_fingerprint,
    digest::digest(fingerprint_input, algo = "sha256", serialize = TRUE)
  )
  expect_match(first$structure_fingerprint, "^[0-9a-f]{64}$")

  data <- .plan_longitudinal_data()
  first_plan <- plan_from_data(data, rcon, c("diary", "notes"))
  second_plan <- plan_from_data(data, shuffled, c("diary", "notes"))
  expect_identical(first_plan, second_plan)
  expect_silent(
    redcapmissing:::.plan_validate_object(first_plan, second)
  )
  expect_s3_class(
    run_plan(
      first_plan,
      data,
      shuffled,
      required_fields = FALSE,
      exclude_types = NULL,
      progress = FALSE
    ),
    "redcapmissing"
  )
})

test_that("fingerprints distinguish missing values and structured delimiter values", {
  missing_metadata <- .plan_metadata()
  missing_metadata$fingerprint_value <- c(
    NA_character_,
    rep("same", nrow(missing_metadata) - 1L)
  )
  literal_metadata <- missing_metadata
  literal_metadata$fingerprint_value[[1L]] <- "<NA>"

  missing_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(metadata = missing_metadata)
  )
  literal_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(metadata = literal_metadata)
  )
  expect_false(identical(
    missing_snapshot$structure_fingerprint,
    literal_snapshot$structure_fingerprint
  ))

  separator <- intToUtf8(31L)
  first_metadata <- .plan_metadata()
  first_values <- rep(list("same"), nrow(first_metadata))
  first_values[[1L]] <- c(paste0("a", separator, "b"), "c")
  first_metadata$fingerprint_value <- first_values
  second_metadata <- first_metadata
  second_metadata$fingerprint_value[[1L]] <- c(
    "a",
    paste0("b", separator, "c")
  )

  first_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(
      metadata = first_metadata,
      record_id_field = "record_id"
    )
  )
  second_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(
      metadata = second_metadata,
      record_id_field = "record_id"
    )
  )
  shuffled_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(
      metadata = first_metadata[rev(seq_len(nrow(first_metadata))), ],
      record_id_field = "record_id"
    )
  )
  expect_false(identical(
    first_snapshot$structure_fingerprint,
    second_snapshot$structure_fingerprint
  ))
  expect_identical(
    first_snapshot$structure_fingerprint,
    shuffled_snapshot$structure_fingerprint
  )
})

test_that("numeric identifiers and fingerprints are option independent", {
  old <- options()
  on.exit(options(old), add = TRUE)
  rcon <- .plan_fake_rcon(project_id = 100000)

  options(scipen = -9L, OutDec = ",")
  first <- plan_from_data(
    tibble::tibble(record_id = 100000),
    rcon,
    "demographics"
  )

  options(scipen = 999L, OutDec = ".")
  second <- plan_from_data(
    tibble::tibble(record_id = 100000L),
    rcon,
    "demographics"
  )

  expect_identical(first$project$project_id, "100000")
  expect_identical(first$assessible_targets$record_id, "100000")
  expect_identical(second$assessible_targets$record_id, "100000")
  expect_identical(first$structure_fingerprint, second$structure_fingerprint)

  special <- redcapmissing:::.schema_format_numeric(c(
    NA_real_, NaN, Inf, -Inf, 0, -0
  ))
  expect_identical(
    special,
    c("NA", "NaN", "Inf", "-Inf", "0", "0")
  )
  extrema <- c(.Machine$double.xmax, -.Machine$double.xmax)
  normalized_extrema <- redcapmissing:::.schema_format_numeric(extrema)
  expect_identical(
    as.numeric(normalized_extrema),
    extrema
  )
  expect_false(any(grepl("[eE]", normalized_extrema)))

  precise_ids <- c(1.234567890123456, 1.234567890123457)
  precise <- plan_from_data(
    tibble::tibble(record_id = precise_ids),
    rcon,
    "demographics"
  )
  expect_identical(
    length(unique(precise$assessible_targets$record_id)),
    2L
  )
  expect_equal(
    as.numeric(precise$assessible_targets$record_id),
    precise_ids,
    tolerance = 0
  )
})
