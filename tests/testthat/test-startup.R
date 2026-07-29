test_that("startup message includes banner and release metadata", {
  build_message <- getFromNamespace(
    ".startup_build_message",
    "redcapmissing"
  )

  message <- cli::ansi_strip(build_message(version = "1.2.3"))
  message_lines <- strsplit(message, "\n", fixed = TRUE)[[1]]

  expect_identical(
    message_lines,
    c(
      "             \u25c9",
      "            \u2571 \u2572",
      "           \u25cf   \u25cf",
      "          \u2571\u2572   \u2571\u2572",
      "         \u00b7  \u00b7 \u00b7  \u00b7",
      "",
      "       redcapmissing",
      "       v1.2.3 \u00b7 eye-spy"
    )
  )
  text_tokens <- regmatches(
    message,
    gregexpr("[[:alnum:]][[:alnum:].-]*", message, perl = TRUE)
  )[[1]]

  expect_identical(
    text_tokens,
    c("redcapmissing", "v1.2.3", "eye-spy")
  )
})

test_that("startup message includes branch spectrum ANSI styling", {
  old_options <- options(cli.num_colors = 256)
  on.exit(options(old_options), add = TRUE)

  build_message <- getFromNamespace(
    ".startup_build_message",
    "redcapmissing"
  )

  message <- build_message(version = "1.2.3")
  message_lines <- strsplit(message, "\n", fixed = TRUE)[[1]]
  styled_lines <- message_lines[c(1:5, 7:8)]
  ansi_codes <- regmatches(
    as.character(message),
    gregexpr("\033\\[[0-9;]+m", as.character(message), perl = TRUE)
  )[[1]]
  foreground_codes <- unique(ansi_codes[
    grepl("38;", ansi_codes, fixed = TRUE)
  ])

  expect_true(cli::ansi_has_any(message))
  expect_true(all(vapply(
    styled_lines,
    cli::ansi_has_any,
    logical(1)
  )))
  expect_gte(length(foreground_codes), 7L)
  expect_identical(
    cli::ansi_strip(message),
    paste(
      c(
        "             \u25c9",
        "            \u2571 \u2572",
        "           \u25cf   \u25cf",
        "          \u2571\u2572   \u2571\u2572",
        "         \u00b7  \u00b7 \u00b7  \u00b7",
        "",
        "       redcapmissing",
        "       v1.2.3 \u00b7 eye-spy"
      ),
      collapse = "\n"
    )
  )
})

test_that("startup version helper resolves package versions safely", {
  get_version <- getFromNamespace(
    ".startup_get_version",
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
    "eye-spy"
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
