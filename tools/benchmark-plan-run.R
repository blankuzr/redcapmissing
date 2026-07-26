# Synthetic tiered benchmark for the plan-and-run workflow.
#
# The default smoke tier is deliberately fast:
#   Rscript tools/benchmark-plan-run.R
#
# Opt into one or more representative workloads with, for example:
#   REDCAPMISSING_BENCH_TIER=ordinary,verified Rscript tools/benchmark-plan-run.R
# Workload families also include constructors (character and numeric observed
# IDs, extended multi-arm repeat with no, partial, and full observed overlap,
# and explicit), branching
# (cross-event plus checkbox), failure-density (0%, 10%, and 100%),
# verification-history (timestamp history plus identical
# latest ties), detail-allocation (paired compact and detailed runs on identical
# sparse-failure inputs), and formatter-cardinality (three context counts).
# Each scenario performs one unmeasured warm-up before measured iterations. Use
# REDCAPMISSING_BENCH_TIER=all to run every tier. Counts can be overridden
# with REDCAPMISSING_BENCH_RECORDS, REDCAPMISSING_BENCH_INSTRUMENTS,
# REDCAPMISSING_BENCH_FIELDS, and REDCAPMISSING_BENCH_ITERATIONS. Set
# REDCAPMISSING_BENCH_MEMORY=true to collect approximate allocation totals with
# Rprofmem(); detail-allocation enables it automatically. Timings include actual
# GC-time deltas and result sizes. No benchmarking package is required.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install `pkgload` before running the source-tree benchmark.", call. = FALSE)
}
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

.benchmark_env_integer <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(as.integer(default))
  if (!grepl("^[1-9][0-9]*$", value)) {
    stop(name, " must be a canonical positive integer.", call. = FALSE)
  }
  parsed <- suppressWarnings(as.double(value))
  if (!is.finite(parsed) || parsed > .Machine$integer.max) {
    stop(name, " is outside the supported integer range.", call. = FALSE)
  }
  as.integer(parsed)
}

.benchmark_env_logical <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = if (default) "true" else "false"))
  if (!value %in% c("true", "false", "1", "0")) {
    stop(name, " must be true, false, 1, or 0.", call. = FALSE)
  }
  value %in% c("true", "1")
}

.benchmark_tiers <- list(
  smoke = list(
    records = 20L, instruments = 2L, fields = 6L, iterations = 1L,
    verified = FALSE, details = FALSE, family = "plan-run"
  ),
  ordinary = list(
    records = 200L, instruments = 2L, fields = 25L, iterations = 3L,
    verified = FALSE, details = FALSE, family = "plan-run"
  ),
  verified = list(
    records = 200L, instruments = 2L, fields = 25L, iterations = 3L,
    verified = TRUE, details = FALSE, family = "plan-run"
  ),
  detailed = list(
    records = 100L, instruments = 2L, fields = 20L, iterations = 2L,
    verified = FALSE, details = TRUE, family = "plan-run"
  ),
  wide = list(
    records = 75L, instruments = 2L, fields = 150L, iterations = 2L,
    verified = FALSE, details = FALSE, family = "plan-run"
  ),
  constructors = list(
    records = 200L, instruments = 2L, fields = 10L, iterations = 2L,
    verified = FALSE, details = FALSE, family = "constructors"
  ),
  branching = list(
    records = 160L, instruments = 1L, fields = 3L, iterations = 2L,
    verified = FALSE, details = TRUE, family = "branching"
  ),
  `failure-density` = list(
    records = 200L, instruments = 2L, fields = 25L, iterations = 2L,
    verified = FALSE, details = FALSE, family = "failure-density"
  ),
  `verification-history` = list(
    records = 200L, instruments = 2L, fields = 25L, iterations = 2L,
    verified = TRUE, details = FALSE, family = "verification-history",
    verification_prior = 6L, verification_ties = 3L
  ),
  `detail-allocation` = list(
    records = 100L, instruments = 2L, fields = 100L, iterations = 3L,
    verified = FALSE, details = FALSE, family = "detail-allocation",
    failure_percent = 5
  ),
  `formatter-cardinality` = list(
    records = 100L, instruments = 1L, fields = 1L, iterations = 2L,
    verified = FALSE, details = FALSE, family = "formatter-cardinality"
  )
)

selected_tiers <- trimws(strsplit(
  Sys.getenv("REDCAPMISSING_BENCH_TIER", unset = "smoke"),
  ",",
  fixed = TRUE
)[[1L]])
if (identical(selected_tiers, "all")) selected_tiers <- names(.benchmark_tiers)
unknown_tiers <- setdiff(selected_tiers, names(.benchmark_tiers))
if (!length(selected_tiers) || any(!nzchar(selected_tiers)) || length(unknown_tiers)) {
  stop(
    paste0(
      "REDCAPMISSING_BENCH_TIER must select one or more of: ",
      paste(names(.benchmark_tiers), collapse = ", "),
      ", or all."
    ),
    call. = FALSE
  )
}
collect_memory <- .benchmark_env_logical("REDCAPMISSING_BENCH_MEMORY")

.benchmark_case <- function(specification) {
  record_count <- specification$records
  instrument_count <- specification$instruments
  field_count <- specification$fields
  failure_percent <- specification$failure_percent
  if (is.null(failure_percent)) failure_percent <- 10
  verification_prior <- specification$verification_prior
  if (is.null(verification_prior)) verification_prior <- 0L
  verification_ties <- specification$verification_ties
  if (is.null(verification_ties)) verification_ties <- 1L
  instruments <- sprintf("instrument_%02d", seq_len(instrument_count))
  field_suffixes <- sprintf("_field_%03d", seq_len(field_count))
  field_matrix <- outer(instruments, field_suffixes, paste0)
  field_names <- as.vector(t(field_matrix))
  field_instruments <- rep(instruments, each = field_count)

  metadata <- data.frame(
    field_name = c("record_id", field_names),
    form_name = c(instruments[[1L]], field_instruments),
    field_type = rep("text", length(field_names) + 1L),
    field_label = c("Record ID", field_names),
    select_choices_or_calculations = rep("", length(field_names) + 1L),
    text_validation_type_or_show_slider_number = rep("", length(field_names) + 1L),
    branching_logic = rep("", length(field_names) + 1L),
    required_field = rep("y", length(field_names) + 1L),
    stringsAsFactors = FALSE
  )
  record_ids <- sprintf("R%05d", seq_len(record_count))
  response_matrix <- matrix(
    "entered",
    nrow = record_count,
    ncol = length(field_names),
    dimnames = list(NULL, field_names)
  )
  failure_count <- as.integer(round(record_count * failure_percent / 100))
  missing_record_index <- if (!failure_count) {
    integer()
  } else if (failure_count == record_count) {
    seq_len(record_count)
  } else {
    unique(as.integer(round(seq(1, record_count, length.out = failure_count))))
  }
  last_field_index <- seq.int(field_count, length(field_names), by = field_count)
  response_matrix[missing_record_index, last_field_index] <- ""
  records <- data.frame(
    record_id = record_ids,
    as.data.frame(response_matrix, stringsAsFactors = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  instrument_table <- data.frame(
    instrument_name = instruments,
    instrument_label = paste("Instrument", seq_len(instrument_count)),
    stringsAsFactors = FALSE
  )
  project <- data.frame(
    project_id = 999L,
    is_longitudinal = 0L,
    stringsAsFactors = FALSE
  )
  repeat_configuration <- data.frame(
    event_name = character(),
    form_name = character(),
    stringsAsFactors = FALSE
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() instrument_table,
    projectInformation = function() project,
    repeatInstrumentEvent = function() repeat_configuration
  )

  verification <- NULL
  if (isTRUE(specification$verified)) {
    verification_contexts <- expand.grid(
      record_index = missing_record_index,
      instrument_index = seq_len(instrument_count),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    history_count <- verification_prior + verification_ties
    verification_grid <- expand.grid(
      context_index = seq_len(nrow(verification_contexts)),
      history_index = seq_len(history_count),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    context <- verification_contexts[
      verification_grid$context_index,
      ,
      drop = FALSE
    ]
    latest <- verification_grid$history_index > verification_prior
    prior_second <- pmin(verification_grid$history_index, 59L)
    timestamps <- sprintf("2026-07-25T11:00:%02dZ", prior_second)
    timestamps[latest] <- "2026-07-25T12:00:00.250Z"
    verification <- data.frame(
      project_id = rep("999", nrow(verification_grid)),
      record = record_ids[context$record_index],
      event_id = rep(NA_character_, nrow(verification_grid)),
      field_name = field_matrix[cbind(
        context$instrument_index,
        rep(field_count, nrow(verification_grid))
      )],
      repeat_instrument = rep(NA_character_, nrow(verification_grid)),
      instance = rep(NA_integer_, nrow(verification_grid)),
      ts = timestamps,
      current_query_status = ifelse(latest, "VERIFIED", "OPEN"),
      username = rep("benchmark-user", nrow(verification_grid)),
      stringsAsFactors = FALSE
    )
  }

  list(
    records = records,
    rcon = rcon,
    instruments = instruments,
    verification = verification,
    expected_targets = as.integer(record_count * instrument_count),
    expected_failed_targets = as.integer(
      length(missing_record_index) * instrument_count
    ),
    expected_missing_rows = as.integer(
      length(missing_record_index) * instrument_count
    ),
    expected_raw_failures = as.integer(
      length(missing_record_index) * instrument_count
    ),
    expected_verification_input = if (is.null(verification)) 0L else nrow(verification)
  )
}

.benchmark_branching_case <- function(specification) {
  record_count <- specification$records
  record_ids <- sprintf("R%05d", seq_len(record_count))
  trigger_values <- rep(c("1", "0"), length.out = record_count)
  checkbox_missing <- seq_len(record_count) %% 3L == 0L
  row_count <- record_count * 2L
  baseline_rows <- seq.int(1L, row_count, by = 2L)
  followup_rows <- baseline_rows + 1L

  metadata <- data.frame(
    field_name = c(
      "record_id", "trigger", "follow_start", "checkbox_field", "conditional"
    ),
    form_name = c("baseline", "baseline", rep("followup", 3L)),
    field_type = c("text", "text", "text", "checkbox", "text"),
    field_label = c(
      "Record ID", "Trigger", "Follow-up started", "Checklist", "Conditional"
    ),
    select_choices_or_calculations = c("", "", "", "1, First | 2, Second", ""),
    text_validation_type_or_show_slider_number = rep("", 5L),
    branching_logic = c(
      "", "", "", "", "[baseline_arm_1][trigger] = '1'"
    ),
    required_field = c("y", "", "y", "y", "y"),
    stringsAsFactors = FALSE
  )
  instruments <- data.frame(
    instrument_name = c("baseline", "followup"),
    instrument_label = c("Baseline", "Follow-up"),
    stringsAsFactors = FALSE
  )
  project <- data.frame(
    project_id = 999L,
    is_longitudinal = 1L,
    stringsAsFactors = FALSE
  )
  arms <- data.frame(arm_num = 1L, name = "Arm 1")
  events <- data.frame(
    event_id = c(101L, 102L),
    unique_event_name = c("baseline_arm_1", "followup_arm_1"),
    event_name = c("Baseline", "Follow-up"),
    arm_num = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  mapping <- data.frame(
    arm_num = c(1L, 1L),
    unique_event_name = c("baseline_arm_1", "followup_arm_1"),
    form = c("baseline", "followup"),
    stringsAsFactors = FALSE
  )
  repeats <- data.frame(
    event_name = character(),
    form_name = character(),
    stringsAsFactors = FALSE
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() instruments,
    projectInformation = function() project,
    arms = function() arms,
    events = function() events,
    mapping = function() mapping,
    repeatInstrumentEvent = function() repeats
  )

  data <- data.frame(
    record_id = rep(record_ids, each = 2L),
    redcap_event_name = rep(c("baseline_arm_1", "followup_arm_1"), record_count),
    trigger = rep("", row_count),
    follow_start = rep("", row_count),
    checkbox_field___1 = rep("", row_count),
    checkbox_field___2 = rep("", row_count),
    conditional = rep("", row_count),
    stringsAsFactors = FALSE
  )
  data$trigger[baseline_rows] <- trigger_values
  data$follow_start[followup_rows] <- "started"
  data$checkbox_field___1[followup_rows] <- ifelse(
    checkbox_missing,
    "0",
    "1"
  )
  data$checkbox_field___2[followup_rows] <- "0"
  conditional_missing <- trigger_values == "1"

  list(
    records = data,
    rcon = rcon,
    instruments = "followup",
    verification = NULL,
    expected_targets = record_count,
    expected_failed_targets = sum(conditional_missing | checkbox_missing),
    expected_missing_rows = sum(conditional_missing) + sum(checkbox_missing),
    expected_raw_failures = sum(conditional_missing | checkbox_missing),
    expected_verification_input = 0L
  )
}

.benchmark_allocated_bytes <- function(path) {
  lines <- readLines(path, warn = FALSE)
  allocation_lines <- grepl("^[0-9]+", lines)
  if (!any(allocation_lines)) return(0)
  sum(as.double(sub(" .*", "", lines[allocation_lines])), na.rm = TRUE)
}

.benchmark_measure <- function(operation, collect_memory) {
  profile <- NULL
  gc()
  gc.time(on = TRUE)
  gc_before <- gc.time()
  if (collect_memory) {
    profile <- tempfile("redcapmissing-rprofmem-", fileext = ".out")
    on.exit({
      utils::Rprofmem(NULL)
      if (file.exists(profile)) unlink(profile)
    }, add = TRUE)
    utils::Rprofmem(profile)
  }
  elapsed <- system.time(value <- operation())[["elapsed"]]
  gc_elapsed_seconds <- sum(gc.time() - gc_before, na.rm = TRUE)
  allocated_bytes <- NA_real_
  if (collect_memory) {
    utils::Rprofmem(NULL)
    allocated_bytes <- .benchmark_allocated_bytes(profile)
    unlink(profile)
  }
  list(
    value = value,
    elapsed = unname(elapsed),
    allocated_bytes = allocated_bytes,
    gc_elapsed_seconds = gc_elapsed_seconds,
    result_size_mb = as.numeric(object.size(value)) / 1024^2
  )
}
.benchmark_constructor_inputs <- function(specification) {
  observed <- .benchmark_case(specification)
  numeric_observed_records <- observed$records
  numeric_observed_records$record_id <- as.double(
    seq_len(nrow(numeric_observed_records))
  )
  record_count <- specification$records
  arm_one_count <- ceiling(record_count / 2)
  arm_two_count <- record_count - arm_one_count
  arm_one_ids <- sprintf("A%05d", seq_len(arm_one_count))
  arm_two_ids <- sprintf("B%05d", seq_len(arm_two_count))

  metadata <- data.frame(
    field_name = c("record_id", "diary_value"),
    form_name = c("demographics", "diary"),
    field_type = c("text", "text"),
    stringsAsFactors = FALSE
  )
  instrument_table <- data.frame(
    instrument_name = c("demographics", "diary"),
    instrument_label = c("Demographics", "Diary"),
    stringsAsFactors = FALSE
  )
  project <- data.frame(project_id = 1001L, is_longitudinal = 1L)
  arms <- data.frame(
    arm_num = c(1L, 2L),
    name = c("Treatment", "Comparator"),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    event_id = c(101L, 102L, 201L),
    unique_event_name = c(
      "baseline_arm_1", "visit_arm_1", "baseline_arm_2"
    ),
    event_name = c("Baseline", "Visit", "Baseline"),
    arm_num = c(1L, 1L, 2L),
    stringsAsFactors = FALSE
  )
  mapping <- data.frame(
    arm_num = c(1L, 1L, 1L, 2L),
    unique_event_name = c(
      "baseline_arm_1", "baseline_arm_1", "visit_arm_1", "baseline_arm_2"
    ),
    form = c("demographics", "diary", "diary", "demographics"),
    stringsAsFactors = FALSE
  )
  repeats <- data.frame(
    event_name = "visit_arm_1",
    form_name = "diary",
    stringsAsFactors = FALSE
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() instrument_table,
    projectInformation = function() project,
    arms = function() arms,
    events = function() events,
    mapping = function() mapping,
    repeatInstrumentEvent = function() repeats
  )
  baseline_one <- data.frame(
    record_id = arm_one_ids,
    redcap_event_name = "baseline_arm_1",
    redcap_repeat_instrument = NA_character_,
    redcap_repeat_instance = NA_integer_,
    stringsAsFactors = FALSE
  )
  visit_one <- data.frame(
    record_id = arm_one_ids,
    redcap_event_name = "visit_arm_1",
    redcap_repeat_instrument = "diary",
    redcap_repeat_instance = 1L,
    stringsAsFactors = FALSE
  )
  baseline_two <- data.frame(
    record_id = arm_two_ids,
    redcap_event_name = "baseline_arm_2",
    redcap_repeat_instrument = NA_character_,
    redcap_repeat_instance = NA_integer_,
    stringsAsFactors = FALSE
  )
  longitudinal_data <- rbind(baseline_one, visit_one, baseline_two)
  partial_overlap_count <- floor(arm_one_count / 2L)
  visit_two_partial <- data.frame(
    record_id = arm_one_ids[seq_len(partial_overlap_count)],
    redcap_event_name = rep("visit_arm_1", partial_overlap_count),
    redcap_repeat_instrument = rep("diary", partial_overlap_count),
    redcap_repeat_instance = rep(2L, partial_overlap_count),
    stringsAsFactors = FALSE
  )
  longitudinal_partial_data <- rbind(longitudinal_data, visit_two_partial)
  extension_no_overlap <- data.frame(
    instrument = "diary",
    redcap_event_name = "visit_arm_1",
    repeat_instance = 2L,
    stringsAsFactors = FALSE
  )
  extension_full_overlap <- data.frame(
    instrument = "diary",
    redcap_event_name = "visit_arm_1",
    repeat_instance = 1L,
    stringsAsFactors = FALSE
  )
  explicit_count <- max(1L, floor(record_count / 4L))
  explicit <- data.frame(
    record_id = sprintf("ABSENT%05d", seq_len(explicit_count)),
    instrument = "diary",
    redcap_event_name = "visit_arm_1",
    repeat_instance = 3L,
    stringsAsFactors = FALSE
  )
  list(
    observed = list(
      operation = function() plan_from_data(
        observed$records,
        observed$rcon,
        observed$instruments
      ),
      expected_targets = observed$expected_targets,
      expected_sources = c(observed = observed$expected_targets)
    ),
    `observed-numeric-ids` = list(
      operation = function() plan_from_data(
        numeric_observed_records,
        observed$rcon,
        observed$instruments
      ),
      expected_targets = observed$expected_targets,
      expected_sources = c(observed = observed$expected_targets),
      validate = function(plan) {
        identical(
          sort(unique(plan$assessible_targets$record_id)),
          sort(as.character(seq_len(record_count)))
        )
      }
    ),
    `extended-no-overlap` = list(
      operation = function() plan_from_data(
        longitudinal_data,
        rcon,
        "diary",
        extension_no_overlap
      ),
      expected_targets = arm_one_count * 3L,
      expected_sources = c(
        observed = arm_one_count * 2L,
        extended = arm_one_count
      )
    ),
    `extended-partial-overlap` = list(
      operation = function() plan_from_data(
        longitudinal_partial_data,
        rcon,
        "diary",
        extension_no_overlap
      ),
      expected_targets = arm_one_count * 3L,
      expected_sources = c(
        observed = arm_one_count * 2L,
        extended = arm_one_count - partial_overlap_count,
        `observed+extended` = partial_overlap_count
      )
    ),
    `extended-full-overlap` = list(
      operation = function() plan_from_data(
        longitudinal_data,
        rcon,
        "diary",
        extension_full_overlap
      ),
      expected_targets = arm_one_count * 2L,
      expected_sources = c(
        observed = arm_one_count,
        `observed+extended` = arm_one_count
      )
    ),
    explicit = list(
      operation = function() plan_explicit(
        longitudinal_data,
        rcon,
        "diary",
        explicit
      ),
      expected_targets = explicit_count,
      expected_sources = c(explicit = explicit_count)
    )
  )
}

.benchmark_formatter_report <- function(record_count, context_count) {
  grid <- expand.grid(
    record_id = sprintf("R%05d", seq_len(record_count)),
    repeat_instance = seq_len(context_count),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  failed <- as.integer(sub("R", "", grid$record_id)) %% 10L == 0L
  targets <- data.frame(
    record_id = grid$record_id,
    instrument = rep("diary", nrow(grid)),
    redcap_event_name = rep("visit_arm_1", nrow(grid)),
    repeat_instrument = rep("diary", nrow(grid)),
    repeat_instance = as.integer(grid$repeat_instance),
    target_source = rep("observed", nrow(grid)),
    event_row_started = rep("passed", nrow(grid)),
    repeat_instance_row_started = rep("passed", nrow(grid)),
    instrument_started = rep("passed", nrow(grid)),
    field_complete = ifelse(failed, "failed", "passed"),
    fields_assessed = rep(2L, nrow(grid)),
    fields_failed = as.integer(failed),
    field_applicability_reason = rep(NA_character_, nrow(grid)),
    stringsAsFactors = FALSE
  )
  plan <- structure(list(
    instruments = "diary",
    assessible_targets = targets[1:6],
    project = list(
      event_labels = c(visit_arm_1 = "Visit"),
      instrument_labels = c(diary = "Diary")
    )
  ), class = "redcapmissing_plan")
  list(plan = plan, target_results = targets)
}

.benchmark_once <- function(case, specification, collect_memory) {
  gc()
  plan_elapsed <- system.time({
    plan <- plan_from_data(
      data = case$records,
      rcon = case$rcon,
      instruments = case$instruments
    )
  })[["elapsed"]]

  verification_elapsed <- NA_real_
  if (isTRUE(specification$verified)) {
    verification_elapsed <- system.time({
      snapshot <- redcapmissing:::.rcm_project_snapshot(case$rcon)
      prepared <- redcapmissing:::.rcm_prepare_verified(
        case$verification,
        "benchmark-user",
        snapshot,
        plan
      )
      stopifnot(
        nrow(prepared$contexts) == case$expected_failed_targets,
        prepared$audit$input_rows == case$expected_verification_input,
        prepared$audit$latest_user_rows == case$expected_failed_targets
      )
    })[["elapsed"]]
  }

  runner_arguments <- list(
    plan = plan,
    data = case$records,
    rcon = case$rcon,
    required_fields = TRUE,
    exclude_types = NULL,
    details = specification$details,
    progress = FALSE
  )
  if (isTRUE(specification$verified)) {
    runner_arguments$verified <- case$verification
    runner_arguments$verified_user <- "benchmark-user"
  }

  profile <- NULL
  gc()
  gc.time(on = TRUE)
  gc_before <- gc.time()
  if (collect_memory) {
    profile <- tempfile("redcapmissing-rprofmem-", fileext = ".out")
    on.exit({
      utils::Rprofmem(NULL)
      if (file.exists(profile)) unlink(profile)
    }, add = TRUE)
    utils::Rprofmem(profile)
  }
  run_elapsed <- system.time({
    report <- do.call(run_plan, runner_arguments)
  })[["elapsed"]]
  gc_elapsed_seconds <- sum(gc.time() - gc_before, na.rm = TRUE)
  allocated_bytes <- NA_real_
  if (collect_memory) {
    utils::Rprofmem(NULL)
    allocated_bytes <- .benchmark_allocated_bytes(profile)
    unlink(profile)
  }

  expected_failed_targets <- if (isTRUE(specification$verified)) {
    0L
  } else {
    case$expected_failed_targets
  }
  expected_missing_rows <- if (isTRUE(specification$verified)) {
    0L
  } else {
    case$expected_missing_rows
  }
  stopifnot(
    inherits(plan, "redcapmissing_plan"),
    inherits(report, "redcapmissing"),
    nrow(report$target_results) == case$expected_targets,
    sum(report$target_results$field_complete == "failed") ==
      expected_failed_targets,
    nrow(report$missing) == expected_missing_rows,
    identical(names(report), c(
      "plan", "target_results", "summary", "missing", "verification",
      "diagnostics", "details"
    )),
    if (isTRUE(specification$details)) {
      is.data.frame(report$details) && nrow(report$details) > 0L
    } else {
      is.null(report$details)
    },
    if (isTRUE(specification$verified)) {
      report$verification$overrides_applied == case$expected_missing_rows &&
        report$verification$input_rows == case$expected_verification_input &&
        report$verification$latest_user_rows == case$expected_failed_targets
    } else {
      !isTRUE(report$verification$enabled)
    }
  )

  result_size_mb <- as.numeric(object.size(list(plan = plan, report = report))) /
    1024^2
  list(
    plan_elapsed = unname(plan_elapsed),
    run_elapsed = unname(run_elapsed),
    verification_elapsed = unname(verification_elapsed),
    allocated_bytes = allocated_bytes,
    gc_elapsed_seconds = gc_elapsed_seconds,
    result_size_mb = result_size_mb,
    diagnostics = report$diagnostics
  )
}
.benchmark_apply_overrides <- function(specification) {
  specification$records <- .benchmark_env_integer(
    "REDCAPMISSING_BENCH_RECORDS",
    specification$records
  )
  specification$instruments <- .benchmark_env_integer(
    "REDCAPMISSING_BENCH_INSTRUMENTS",
    specification$instruments
  )
  specification$fields <- .benchmark_env_integer(
    "REDCAPMISSING_BENCH_FIELDS",
    specification$fields
  )
  specification$iterations <- .benchmark_env_integer(
    "REDCAPMISSING_BENCH_ITERATIONS",
    specification$iterations
  )
  specification
}

.benchmark_median <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

.benchmark_summary_row <- function(
  tier,
  family,
  scenario,
  specification,
  targets,
  constructor = NA_real_,
  runner = NA_real_,
  verification = NA_real_,
  formatter = NA_real_,
  allocated = NA_real_,
  gc_elapsed = NA_real_,
  result_size = NA_real_
) {
  data.frame(
    tier = tier,
    family = family,
    scenario = scenario,
    records = specification$records,
    instruments = specification$instruments,
    fields_per_instrument = specification$fields,
    targets = as.integer(targets),
    verified = isTRUE(specification$verified),
    details = isTRUE(specification$details),
    iterations = specification$iterations,
    median_constructor_seconds = .benchmark_median(constructor),
    median_runner_seconds = .benchmark_median(runner),
    median_verification_seconds = .benchmark_median(verification),
    median_formatter_seconds = .benchmark_median(formatter),
    median_total_seconds = .benchmark_median(
      rowSums(cbind(constructor, runner, formatter), na.rm = TRUE)
    ),
    median_allocated_mb = .benchmark_median(allocated) / 1024^2,
    compact_allocation_reduction_percent = NA_real_,
    median_gc_seconds = .benchmark_median(gc_elapsed),
    median_result_size_mb = .benchmark_median(result_size),
    stringsAsFactors = FALSE
  )
}

.benchmark_run_plan_scenario <- function(
  tier,
  scenario,
  specification,
  case,
  collect_memory
) {
  iteration_count <- specification$iterations
  invisible(.benchmark_once(case, specification, collect_memory = collect_memory))
  constructor <- runner <- verification <- allocated <- gc_elapsed <-
    result_size <- numeric(iteration_count)
  diagnostics <- vector("list", iteration_count)
  for (iteration in seq_len(iteration_count)) {
    result <- .benchmark_once(case, specification, collect_memory)
    constructor[[iteration]] <- result$plan_elapsed
    runner[[iteration]] <- result$run_elapsed
    verification[[iteration]] <- result$verification_elapsed
    allocated[[iteration]] <- result$allocated_bytes
    gc_elapsed[[iteration]] <- result$gc_elapsed_seconds
    result_size[[iteration]] <- result$result_size_mb
    diagnostics[[iteration]] <- transform(
      result$diagnostics,
      iteration = iteration,
      scenario = scenario
    )
  }
  diagnostics <- do.call(rbind, diagnostics)
  stages <- aggregate(
    elapsed_seconds ~ scenario + stage + operation,
    data = diagnostics,
    FUN = stats::median
  )
  names(stages)[names(stages) == "elapsed_seconds"] <- "median_seconds"
  stages <- stages[order(stages$scenario, stages$stage), , drop = FALSE]
  list(
    summary = .benchmark_summary_row(
      tier = tier,
      family = specification$family,
      scenario = scenario,
      specification = specification,
      targets = case$expected_targets,
      constructor = constructor,
      runner = runner,
      verification = verification,
      allocated = allocated,
      gc_elapsed = gc_elapsed,
      result_size = result_size
    ),
    stages = stages
  )
}

.benchmark_run_constructors <- function(tier, specification, collect_memory) {
  inputs <- .benchmark_constructor_inputs(specification)
  summaries <- vector("list", length(inputs))
  for (scenario_index in seq_along(inputs)) {
    scenario <- names(inputs)[[scenario_index]]
    input <- inputs[[scenario_index]]
    invisible(.benchmark_measure(input$operation, collect_memory = collect_memory))
    elapsed <- allocated <- gc_elapsed <- result_size <-
      numeric(specification$iterations)
    for (iteration in seq_len(specification$iterations)) {
      measured <- .benchmark_measure(input$operation, collect_memory)
      plan <- measured$value
      source_counts <- table(plan$assessible_targets$target_source)
      observed_counts <- as.integer(source_counts[names(input$expected_sources)])
      observed_counts[is.na(observed_counts)] <- 0L
      stopifnot(
        inherits(plan, "redcapmissing_plan"),
        nrow(plan$assessible_targets) == input$expected_targets,
        identical(observed_counts, as.integer(input$expected_sources)),
        !anyDuplicated(plan$assessible_targets[1:5]),
        is.null(input$validate) || isTRUE(input$validate(plan))
      )
      elapsed[[iteration]] <- measured$elapsed
      allocated[[iteration]] <- measured$allocated_bytes
      gc_elapsed[[iteration]] <- measured$gc_elapsed_seconds
      result_size[[iteration]] <- measured$result_size_mb
    }
    summaries[[scenario_index]] <- .benchmark_summary_row(
      tier = tier,
      family = specification$family,
      scenario = scenario,
      specification = specification,
      targets = input$expected_targets,
      constructor = elapsed,
      allocated = allocated,
      gc_elapsed = gc_elapsed,
      result_size = result_size
    )
  }
  list(
    summary = do.call(rbind, summaries),
    stages = data.frame(
      scenario = character(), stage = integer(), operation = character(),
      median_seconds = numeric(), stringsAsFactors = FALSE
    )
  )
}

.benchmark_run_formatter <- function(tier, specification, collect_memory) {
  context_counts <- c(10L, 100L, 500L)
  summaries <- vector("list", length(context_counts))
  for (context_index in seq_along(context_counts)) {
    context_count <- context_counts[[context_index]]
    report <- .benchmark_formatter_report(specification$records, context_count)
    invisible(.benchmark_measure(
      function() {
        redcapmissing:::.redcapmissing_flex_event_instruments_build(report)
      },
      collect_memory = collect_memory
    ))
    elapsed <- allocated <- gc_elapsed <- result_size <-
      numeric(specification$iterations)
    for (iteration in seq_len(specification$iterations)) {
      measured <- .benchmark_measure(
        function() {
          redcapmissing:::.redcapmissing_flex_event_instruments_build(report)
        },
        collect_memory
      )
      parts <- measured$value
      stopifnot(
        nrow(parts$data) == context_count + 2L,
        parts$data$.denominator[[1L]] == specification$records * context_count,
        identical(
          parts$data$`Repeat Instance`[-c(1L, 2L)],
          as.character(seq_len(context_count))
        )
      )
      elapsed[[iteration]] <- measured$elapsed
      allocated[[iteration]] <- measured$allocated_bytes
      gc_elapsed[[iteration]] <- measured$gc_elapsed_seconds
      result_size[[iteration]] <- measured$result_size_mb
    }
    summaries[[context_index]] <- .benchmark_summary_row(
      tier = tier,
      family = specification$family,
      scenario = paste0(context_count, "-contexts"),
      specification = specification,
      targets = specification$records * context_count,
      formatter = elapsed,
      allocated = allocated,
      gc_elapsed = gc_elapsed,
      result_size = result_size
    )
  }
  list(
    summary = do.call(rbind, summaries),
    stages = data.frame(
      scenario = character(), stage = integer(), operation = character(),
      median_seconds = numeric(), stringsAsFactors = FALSE
    )
  )
}

.benchmark_run_tier <- function(name, specification, collect_memory) {
  specification <- .benchmark_apply_overrides(specification)
  family <- specification$family
  if (identical(family, "constructors")) {
    return(.benchmark_run_constructors(name, specification, collect_memory))
  }
  if (identical(family, "formatter-cardinality")) {
    return(.benchmark_run_formatter(name, specification, collect_memory))
  }
  if (identical(family, "failure-density")) {
    density <- c(`0-percent` = 0, `10-percent` = 10, `100-percent` = 100)
    results <- lapply(names(density), function(scenario) {
      scenario_specification <- specification
      scenario_specification$failure_percent <- density[[scenario]]
      .benchmark_run_plan_scenario(
        name,
        scenario,
        scenario_specification,
        .benchmark_case(scenario_specification),
        collect_memory
      )
    })
    return(list(
      summary = do.call(rbind, lapply(results, `[[`, "summary")),
      stages = do.call(rbind, lapply(results, `[[`, "stages"))
    ))
  }
  if (identical(family, "detail-allocation")) {
    case <- .benchmark_case(specification)
    compact_specification <- specification
    compact_specification$details <- FALSE
    detailed_specification <- specification
    detailed_specification$details <- TRUE
    compact <- .benchmark_run_plan_scenario(
      name, "compact", compact_specification, case, collect_memory = TRUE
    )
    detailed <- .benchmark_run_plan_scenario(
      name, "detailed", detailed_specification, case, collect_memory = TRUE
    )
    compact_allocation <- compact$summary$median_allocated_mb
    detailed_allocation <- detailed$summary$median_allocated_mb
    stopifnot(is.finite(compact_allocation), is.finite(detailed_allocation))
    reduction <- 100 * (1 - compact_allocation / detailed_allocation)
    compact$summary$compact_allocation_reduction_percent <- reduction
    detailed$summary$compact_allocation_reduction_percent <- reduction
    return(list(
      summary = rbind(compact$summary, detailed$summary),
      stages = rbind(compact$stages, detailed$stages)
    ))
  }
  if (identical(family, "branching")) {
    case <- .benchmark_branching_case(specification)
    return(.benchmark_run_plan_scenario(
      name, "cross-event-checkbox", specification, case, collect_memory
    ))
  }
  scenario <- if (identical(family, "verification-history")) {
    "timestamp-history-and-latest-ties"
  } else {
    "default"
  }
  .benchmark_run_plan_scenario(
    name,
    scenario,
    specification,
    .benchmark_case(specification),
    collect_memory
  )
}
cat("redcapmissing plan/run benchmark\n")
cat("R:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("Package:", as.character(utils::packageVersion("redcapmissing")), "\n")
cat("Tiers:", paste(selected_tiers, collapse = ", "), "\n")
allocation_mode <- if (collect_memory) {
  "enabled for all selected tiers"
} else if (any(vapply(
  .benchmark_tiers[selected_tiers],
  function(specification) identical(specification$family, "detail-allocation"),
  logical(1)
))) {
  "enabled for detail-allocation"
} else {
  "disabled"
}
cat("Allocation profiling:", allocation_mode, "\n")

results <- vector("list", length(selected_tiers))
for (tier_index in seq_along(selected_tiers)) {
  tier_name <- selected_tiers[[tier_index]]
  cat("\nRunning tier:", tier_name, "\n")
  result <- .benchmark_run_tier(
    tier_name,
    .benchmark_tiers[[tier_name]],
    collect_memory
  )
  results[[tier_index]] <- result$summary
  print(result$stages, row.names = FALSE)
}

cat("\nTier summary\n")
print(do.call(rbind, results), row.names = FALSE)
cat("\nSession information\n")
print(utils::sessionInfo())
