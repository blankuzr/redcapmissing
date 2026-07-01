test_that("startup message includes banner and release metadata", {
  build_message <- getFromNamespace(
    ".redcapmissing_startup_build_message",
    "redcapmissing"
  )

  message <- cli::ansi_strip(build_message(version = "1.2.3"))
  message_lines <- strsplit(message, "\n", fixed = TRUE)[[1]]

  expect_identical(
    message_lines,
    c(
      "> redcapmissing",
      "  Forever-searching  {v1.2.3}",
      "  :: Improved scope reporting"
    )
  )
  expect_false(any(grepl("\u2588", message_lines, fixed = TRUE)))
  expect_false(any(grepl("Release:", message_lines, fixed = TRUE)))
  expect_false(any(grepl("Latest:", message_lines, fixed = TRUE)))
})

test_that("startup version helper resolves package versions safely", {
  get_version <- getFromNamespace(
    ".redcapmissing_startup_get_version",
    "redcapmissing"
  )

  expected_version <- utils::packageDescription(
    "redcapmissing",
    fields = "Version"
  )
  missing_version <- NULL

  expect_identical(get_version("redcapmissing"), expected_version)
  expect_warning(
    missing_version <- get_version("redcapmissing.not.a.real.package"),
    NA
  )
  expect_identical(missing_version, "unknown")
})

test_that("startup hook emits message when enabled", {
  on_attach <- getFromNamespace(".onAttach", "redcapmissing")
  old_options <- options(redcapmissing.startup_message = TRUE)
  on.exit(options(old_options), add = TRUE)

  expect_message(
    on_attach(NULL, "redcapmissing"),
    "Forever-searching"
  )
})

test_that("startup hook is suppressible", {
  on_attach <- getFromNamespace(".onAttach", "redcapmissing")
  old_options <- options(redcapmissing.startup_message = FALSE)
  on.exit(options(old_options), add = TRUE)

  expect_message(
    on_attach(NULL, "redcapmissing"),
    NA
  )
})
