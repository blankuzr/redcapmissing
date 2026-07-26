# Synthetic smoke benchmark for the plan-and-run workflow.
#
# Run from the package root with:
#   Rscript tools/benchmark-plan-run.R

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install `devtools` before running this benchmark.", call. = FALSE)
}

devtools::load_all(".", quiet = TRUE, export_all = FALSE)

field_count <- as.integer(Sys.getenv("REDCAPMISSING_BENCH_FIELDS", "40"))
record_count <- as.integer(Sys.getenv("REDCAPMISSING_BENCH_RECORDS", "150"))
iterations <- as.integer(Sys.getenv("REDCAPMISSING_BENCH_ITERATIONS", "3"))

if (
  anyNA(c(field_count, record_count, iterations)) ||
    any(c(field_count, record_count, iterations) < 1L)
) {
  stop("Benchmark counts must be positive integers.", call. = FALSE)
}

instruments <- c("baseline", "followup")
field_names <- unlist(lapply(instruments, function(instrument) {
  paste0(instrument, "_field_", seq_len(field_count))
}), use.names = FALSE)
field_instruments <- rep(instruments, each = field_count)

metadata <- data.frame(
  field_name = c("record_id", field_names),
  form_name = c("baseline", field_instruments),
  field_type = "text",
  field_label = c("Record ID", field_names),
  select_choices_or_calculations = "",
  text_validation_type_or_show_slider_number = "",
  branching_logic = "",
  required_field = "y",
  stringsAsFactors = FALSE
)

records <- data.frame(
  record_id = sprintf("R%05d", seq_len(record_count)),
  stringsAsFactors = FALSE
)
for (field in field_names) {
  records[[field]] <- "entered"
}
records[[field_names[[length(field_names)]]]][seq(1L, record_count, by = 10L)] <- ""

rcon <- list(
  metadata = function() metadata,
  instruments = function() data.frame(
    instrument_name = instruments,
    instrument_label = c("Baseline", "Follow-up"),
    stringsAsFactors = FALSE
  ),
  projectInformation = function() data.frame(
    project_id = 999L,
    is_longitudinal = 0L
  ),
  repeatInstrumentEvent = function() data.frame()
)

elapsed <- numeric(iterations)
for (iteration in seq_len(iterations)) {
  elapsed[[iteration]] <- system.time({
    plan <- plan_from_data(
      data = records,
      rcon = rcon,
      instruments = instruments
    )
    report <- run_plan(
      plan = plan,
      data = records,
      rcon = rcon,
      required_fields = TRUE,
      exclude_types = NULL,
      details = FALSE,
      progress = FALSE
    )
  })[["elapsed"]]

  stopifnot(
    inherits(plan, "redcapmissing_plan"),
    inherits(report, "redcapmissing"),
    nrow(report$target_results) == record_count * length(instruments),
    identical(names(report), c(
      "plan", "target_results", "summary", "missing", "verification",
      "diagnostics", "details"
    ))
  )
}

result <- data.frame(
  records = record_count,
  instruments = length(instruments),
  fields_per_instrument = field_count,
  iterations = iterations,
  median_seconds = stats::median(elapsed),
  min_seconds = min(elapsed),
  max_seconds = max(elapsed)
)

print(result, row.names = FALSE)