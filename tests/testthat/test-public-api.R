test_that("the public API is exactly the documented twelve-function surface", {
  expect_identical(
    sort(getNamespaceExports("redcapmissing")),
    sort(c(
      "all_instruments", "build_explicit_schedule", "build_extended_schedule",
      "flex_event_instruments", "flex_html", "flexify", "get_missing",
      "get_summary", "plan_explicit", "plan_from_data", "registry", "run_plan"
    ))
  )

  expect_identical(
    as.list(formals(all_instruments)),
    alist(rcon =)
  )
  expect_identical(
    as.list(formals(build_explicit_schedule)),
    alist(data =, rcon =, explicit_spec =)
  )
  expect_identical(
    as.list(formals(build_extended_schedule)),
    alist(rcon =, instruments =, n_repeat_instances = 1L)
  )
  expect_identical(
    as.list(formals(plan_from_data)),
    alist(data =, rcon =, instruments =, extended_schedule = NULL)
  )
  expect_identical(
    as.list(formals(plan_explicit)),
    alist(data =, rcon =, explicit_schedule =)
  )
  expect_identical(
    as.list(formals(run_plan)),
    alist(
      plan =,
      data =,
      rcon =,
      required_fields = TRUE,
      ignore_fields = NULL,
      exclude_types = "descriptive",
      verified = NULL,
      verified_user = NULL,
      details = FALSE,
      progress = interactive()
    )
  )
  expect_identical(
    as.list(formals(get_summary)),
    alist(
      report =,
      validation_check = NULL,
      events = NULL,
      instruments = NULL
    )
  )
  expect_identical(
    as.list(formals(get_missing)),
    alist(
      report =,
      validation_check = NULL,
      events = NULL,
      instruments = NULL
    )
  )
  expect_identical(as.list(formals(registry)), alist())
  expect_identical(as.list(formals(flexify)), alist(x =))
  expect_identical(
    as.list(formals(flex_event_instruments)),
    alist(x =, missing_threshold = 0.10, ... =)
  )
  expect_identical(as.list(formals(flex_html)), alist(x =))
})

test_that("retired entry points are absent from exports and the namespace", {
  exports <- getNamespaceExports("redcapmissing")

  expect_false(any(c("find_missing", "flex_event_forms") %in% exports))
  expect_false(exists(
    "find_missing",
    envir = asNamespace("redcapmissing"),
    inherits = FALSE
  ))
  expect_false(exists(
    "flex_event_forms",
    envir = asNamespace("redcapmissing"),
    inherits = FALSE
  ))
})

test_that("documented S3 methods are registered and public generics dispatch", {
  expect_identical(
    utils::getS3method("flex_event_instruments", "redcapmissing"),
    getFromNamespace(
      "flex_event_instruments.redcapmissing",
      "redcapmissing"
    )
  )
  expect_identical(
    utils::getS3method("print", "redcapmissing_plan"),
    getFromNamespace("print.redcapmissing_plan", "redcapmissing")
  )
  expect_identical(
    utils::getS3method("print", "redcapmissing_registry"),
    getFromNamespace("print.redcapmissing_registry", "redcapmissing")
  )

  skip_if_not_installed("flextable")
  skip_if_not_installed("glue")

  rcon <- run_plan_rcon()
  data <- run_plan_data()
  report <- run_plan(
    plan_from_data(data, rcon, "baseline_form"),
    data,
    rcon,
    progress = FALSE
  )
  result <- flex_event_instruments(report)
  expect_s3_class(result, "flextable")
})
