# Synthetic tiered benchmark for planning and assessment.
#
# The default smoke tier is deliberately fast:
#   Rscript tools/benchmark-plan-run.R
#
# Opt into one or more representative workloads with, for example:
#   REDCAPMISSING_BENCH_TIER=ordinary,verified Rscript tools/benchmark-plan-run.R
# Workload families also include `high-cardinality` (U2/U10 plus branching,
# instrument-start, failure, details, verification, dependency-width,
# data-width, and progress controls), constructors (character and numeric observed
# IDs, extended repeats across multiple arms with zero, partial, or full
# observed overlap, and explicit targets); branching (cross event plus
# checkbox); `failure-density` (0%, 10%, and 100%),
# `verification-history` (timestamp history plus identical
# latest ties), `detail-allocation` (paired compact and detailed runs on
# identical inputs with few failures), and `formatter-cardinality` (three
# context counts).
# Each scenario performs one unmeasured warmup before measured iterations. Use
# REDCAPMISSING_BENCH_TIER=all to run every tier. Counts can be overridden
# with REDCAPMISSING_BENCH_RECORDS, REDCAPMISSING_BENCH_INSTRUMENTS,
# REDCAPMISSING_BENCH_FIELDS, and REDCAPMISSING_BENCH_ITERATIONS. Set
# REDCAPMISSING_BENCH_MEMORY=true to collect approximate allocation totals with
# Rprofmem(); `detail-allocation` enables it automatically. Allocation profiles
# run separately from timed expressions. Select high-cardinality scenarios with
# REDCAPMISSING_BENCH_SCENARIO=U2,U10. Optionally save an untracked RDS artifact
# with REDCAPMISSING_BENCH_OUTPUT and compare it with
# REDCAPMISSING_BENCH_BASELINE. Exact report hashes sanitize only
# `diagnostics$elapsed_seconds`.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install `pkgload` before running the source tree benchmark.", call. = FALSE)
}
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

.benchmark_connection <- function(x) {
  structure(x, class = c("redcapApiConnection", "redcapConnection"))
}

.benchmark_env_integer <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(as.integer(default))
  if (!grepl("^[1-9][0-9]*$", value)) {
    stop(name, " must contain character digits matching `[1-9][0-9]*`.", call. = FALSE)
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
  ),
  `high-cardinality` = list(
    records = 100L, instruments = 600L, fields = 10L, iterations = 1L,
    verified = FALSE, details = FALSE, family = "high-cardinality"
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
selected_high_cardinality_scenarios <- trimws(strsplit(
  Sys.getenv("REDCAPMISSING_BENCH_SCENARIO", unset = ""), ",", fixed = TRUE
)[[1L]])
if (identical(selected_high_cardinality_scenarios, "")) {
  selected_high_cardinality_scenarios <- character()
}
if (any(!nzchar(selected_high_cardinality_scenarios)) ||
    anyDuplicated(selected_high_cardinality_scenarios)) {
  stop("REDCAPMISSING_BENCH_SCENARIO must contain unique, nonblank names.",
       call. = FALSE)
}
benchmark_output_path <- Sys.getenv("REDCAPMISSING_BENCH_OUTPUT", unset = "")
benchmark_baseline_path <- Sys.getenv("REDCAPMISSING_BENCH_BASELINE", unset = "")


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
  branch_mode <- specification$branch_mode
  if (is.null(branch_mode)) branch_mode <- "none"
  instrument_start_mode <- specification$instrument_start_mode
  if (is.null(instrument_start_mode)) instrument_start_mode <- "first"
  verification_mode <- specification$verification_mode
  if (is.null(verification_mode)) {
    verification_mode <- if (isTRUE(specification$verified)) "dense" else "disabled"
  }
  extra_columns <- specification$extra_columns
  if (is.null(extra_columns)) extra_columns <- 0L
  stopifnot(
    branch_mode %in% c("none", "shared", "per-instrument"),
    instrument_start_mode %in% c("first", "last", "none"),
    verification_mode %in% c("disabled", "empty", "sparse", "dense"),
    length(extra_columns) == 1L,
    extra_columns >= 0L
  )

  instruments <- sprintf("instrument_%02d", seq_len(instrument_count))
  field_suffixes <- sprintf("_field_%03d", seq_len(field_count))
  field_matrix <- outer(instruments, field_suffixes, paste0)
  field_names <- as.vector(t(field_matrix))
  field_instruments <- rep(instruments, each = field_count)
  gate_names <- character()
  gate_instruments <- character()
  field_logic <- rep.int("", length(field_names))
  if (identical(branch_mode, "shared")) {
    gate_names <- "benchmark_shared_gate"
    gate_instruments <- instruments[[1L]]
    field_logic[] <- "[benchmark_shared_gate] = '1'"
  } else if (identical(branch_mode, "per-instrument")) {
    gate_names <- paste0(instruments, "_benchmark_gate")
    gate_instruments <- instruments
    field_logic <- rep(
      paste0("[", gate_names, "] = '1'"),
      each = field_count
    )
  }

  metadata <- data.frame(
    field_name = c("record_id", field_names),
    form_name = c(instruments[[1L]], field_instruments),
    field_type = rep("text", length(field_names) + 1L),
    field_label = c("Record ID", field_names),
    select_choices_or_calculations = rep("", length(field_names) + 1L),
    text_validation_type_or_show_slider_number = rep("", length(field_names) + 1L),
    branching_logic = c("", field_logic),
    required_field = rep("y", length(field_names) + 1L),
    stringsAsFactors = FALSE
  )
  if (length(gate_names)) {
    metadata <- rbind(metadata, data.frame(
      field_name = gate_names,
      form_name = gate_instruments,
      field_type = rep("text", length(gate_names)),
      field_label = gate_names,
      select_choices_or_calculations = rep("", length(gate_names)),
      text_validation_type_or_show_slider_number = rep("", length(gate_names)),
      branching_logic = rep("", length(gate_names)),
      required_field = rep("", length(gate_names)),
      stringsAsFactors = FALSE
    ))
  }

  record_ids <- sprintf("R%05d", seq_len(record_count))
  response_matrix <- matrix(
    "entered",
    nrow = record_count,
    ncol = length(field_names),
    dimnames = list(NULL, field_names)
  )
  if (instrument_start_mode %in% c("last", "none")) {
    response_matrix[] <- ""
  }
  last_field_index <- seq.int(field_count, length(field_names), by = field_count)
  if (identical(instrument_start_mode, "last")) {
    response_matrix[, last_field_index] <- "entered"
  }
  failure_count <- as.integer(round(record_count * failure_percent / 100))
  missing_record_index <- if (!failure_count) {
    integer()
  } else if (failure_count == record_count) {
    seq_len(record_count)
  } else {
    unique(as.integer(round(seq(1, record_count, length.out = failure_count))))
  }
  response_matrix[missing_record_index, last_field_index] <- ""
  records <- data.frame(
    record_id = record_ids,
    as.data.frame(response_matrix, stringsAsFactors = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  branch_open <- rep.int(TRUE, record_count)
  if (length(gate_names)) {
    branch_open <- seq_len(record_count) %% 2L == 1L
    gate_values <- ifelse(branch_open, "1", "0")
    for (gate_name in gate_names) records[[gate_name]] <- gate_values
  }
  if (extra_columns > 0L) {
    extra_names <- sprintf("benchmark_unused_%05d", seq_len(extra_columns))
    records[extra_names] <- rep(
      list(rep.int("unused", record_count)),
      extra_columns
    )
  }

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
  rcon <- .benchmark_connection(list(
    metadata = function() metadata,
    instruments = function() instrument_table,
    projectInformation = function() project,
    repeatInstrumentEvent = function() repeat_configuration
  ))

  applicable_missing <- missing_record_index[branch_open[missing_record_index]]
  raw_failed_targets <- length(applicable_missing) * instrument_count
  raw_missing_rows <- raw_failed_targets
  if (identical(instrument_start_mode, "last")) {
    raw_failed_targets <- record_count * instrument_count
    raw_missing_rows <- record_count * instrument_count * max(0L, field_count - 1L)
  } else if (identical(instrument_start_mode, "none")) {
    raw_failed_targets <- 0L
    raw_missing_rows <- record_count * instrument_count
  }

  verification <- NULL
  verification_context_count <- 0L
  expected_verification_input <- 0L
  if (identical(verification_mode, "empty")) {
    verification <- data.frame(
      project_id = character(), record = character(), event_id = character(),
      field_name = character(), repeat_instrument = character(),
      instance = integer(), ts = character(), current_query_status = character(),
      username = character(), stringsAsFactors = FALSE
    )
  } else if (verification_mode %in% c("sparse", "dense")) {
    verification_contexts <- expand.grid(
      record_index = applicable_missing,
      instrument_index = seq_len(instrument_count),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    if (identical(verification_mode, "sparse") && nrow(verification_contexts)) {
      verification_contexts <- verification_contexts[
        seq.int(1L, nrow(verification_contexts), by = 10L),
        ,
        drop = FALSE
      ]
    }
    verification_context_count <- nrow(verification_contexts)
    history_count <- if (identical(verification_mode, "dense")) {
      verification_prior + verification_ties
    } else {
      1L
    }
    verification_grid <- expand.grid(
      context_index = seq_len(verification_context_count),
      history_index = seq_len(history_count),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    context <- verification_contexts[
      verification_grid$context_index,
      ,
      drop = FALSE
    ]
    latest <- if (identical(verification_mode, "dense")) {
      verification_grid$history_index > verification_prior
    } else {
      rep.int(TRUE, nrow(verification_grid))
    }
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
    expected_verification_input <- nrow(verification)
  }

  expected_overrides <- verification_context_count
  expected_effective_failed_targets <- raw_failed_targets - expected_overrides
  expected_effective_missing_rows <- raw_missing_rows - expected_overrides
  nonblank_logic <- field_logic[nzchar(field_logic)]
  list(
    records = records,
    rcon = rcon,
    instruments = instruments,
    verification = verification,
    verification_mode = verification_mode,
    expected_targets = as.integer(record_count * instrument_count),
    expected_failed_targets = as.integer(raw_failed_targets),
    expected_missing_rows = as.integer(raw_missing_rows),
    expected_effective_failed_targets = as.integer(expected_effective_failed_targets),
    expected_effective_missing_rows = as.integer(expected_effective_missing_rows),
    expected_raw_failures = as.integer(raw_failed_targets),
    expected_verification_input = as.integer(expected_verification_input),
    expected_verification_user_rows = as.integer(expected_verification_input),
    expected_verification_contexts = as.integer(verification_context_count),
    expected_overrides = as.integer(expected_overrides),
    metrics = list(
      physical_rows = nrow(records),
      metadata_rows = nrow(metadata),
      data_columns = ncol(records),
      selected_fields = length(field_names),
      total_branch_expressions = length(nonblank_logic),
      unique_branch_expressions = length(unique(nonblank_logic)),
      branch_dependency_columns = length(gate_names)
    )
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
      "Record ID", "Trigger", "Follow up started", "Checklist", "Conditional"
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
    instrument_label = c("Baseline", "Follow up"),
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
    event_name = c("Baseline", "Follow up"),
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
  rcon <- .benchmark_connection(list(
    metadata = function() metadata,
    instruments = function() instruments,
    projectInformation = function() project,
    arms = function() arms,
    events = function() events,
    mapping = function() mapping,
    repeatInstrumentEvent = function() repeats
  ))

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
    verification_mode = "disabled",
    expected_targets = record_count,
    expected_failed_targets = sum(conditional_missing | checkbox_missing),
    expected_missing_rows = sum(conditional_missing) + sum(checkbox_missing),
    expected_effective_failed_targets = sum(conditional_missing | checkbox_missing),
    expected_effective_missing_rows = sum(conditional_missing) + sum(checkbox_missing),
    expected_raw_failures = sum(conditional_missing | checkbox_missing),
    expected_verification_input = 0L,
    expected_verification_user_rows = 0L,
    expected_verification_contexts = 0L,
    expected_overrides = 0L,
    metrics = list(
      physical_rows = nrow(data), metadata_rows = nrow(metadata),
      data_columns = ncol(data), selected_fields = 3L,
      total_branch_expressions = 1L, unique_branch_expressions = 1L,
      branch_dependency_columns = 1L
    )
  )
}

.benchmark_allocated_bytes <- function(path) {
  lines <- readLines(path, warn = FALSE)
  allocation_lines <- grepl("^[0-9]+", lines)
  if (!any(allocation_lines)) return(0)
  sum(as.double(sub(" .*", "", lines[allocation_lines])), na.rm = TRUE)
}

.benchmark_sanitize_report <- function(value) {
  if (inherits(value, "redcapmissing") &&
      is.data.frame(value$diagnostics) &&
      "elapsed_seconds" %in% names(value$diagnostics)) {
    value$diagnostics$elapsed_seconds <- rep.int(
      0,
      nrow(value$diagnostics)
    )
  }
  value
}

.benchmark_equivalence_hash <- function(value) {
  digest::digest(
    .benchmark_sanitize_report(value),
    algo = "sha256",
    serialize = TRUE
  )
}

.benchmark_profile_allocation <- function(operation) {
  profile <- NULL
  gc()
  profile <- tempfile("redcapmissing-rprofmem-", fileext = ".out")
  on.exit({
    utils::Rprofmem(NULL)
    if (file.exists(profile)) unlink(profile)
  }, add = TRUE)
  utils::Rprofmem(profile)
  value <- operation()
  utils::Rprofmem(NULL)
  allocated_bytes <- .benchmark_allocated_bytes(profile)
  unlink(profile)
  list(value = value, allocated_bytes = allocated_bytes)
}

.benchmark_measure <- function(
  operation,
  collect_memory,
  normalize = identity
) {
  gc()
  gc.time(on = TRUE)
  gc_before <- gc.time()
  elapsed <- system.time(value <- operation())[["elapsed"]]
  gc_elapsed_seconds <- sum(gc.time() - gc_before, na.rm = TRUE)
  allocated_bytes <- NA_real_
  if (collect_memory) {
    allocation <- .benchmark_profile_allocation(operation)
    if (!identical(normalize(value), normalize(allocation$value))) {
      stop("Timed and allocation-profile executions returned different values.",
           call. = FALSE)
    }
    allocated_bytes <- allocation$allocated_bytes
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
  rcon <- .benchmark_connection(list(
    metadata = function() metadata,
    instruments = function() instrument_table,
    projectInformation = function() project,
    arms = function() arms,
    events = function() events,
    mapping = function() mapping,
    repeatInstrumentEvent = function() repeats
  ))
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
  if (!is.null(case$verification)) {
    verification_snapshot <- redcapmissing:::.project_structure_build_snapshot(
      case$rcon
    )
    verification_elapsed <- system.time({
      prepared <- redcapmissing:::.verification_prepare_contexts(
        case$verification,
        "benchmark-user",
        verification_snapshot,
        plan
      )
      stopifnot(
        nrow(prepared$contexts) == case$expected_verification_contexts,
        prepared$audit$input_rows == case$expected_verification_input,
        prepared$audit$user_rows == case$expected_verification_user_rows,
        prepared$audit$latest_user_rows == case$expected_verification_contexts,
        prepared$audit$verified_rows == case$expected_verification_contexts
      )
    })[["elapsed"]]
  }

  runner_arguments <- list(
    plan = plan,
    data = case$records,
    rcon = case$rcon,
    required_fields = TRUE,
    exclude_types = NULL,
    details = isTRUE(specification$details),
    progress = isTRUE(specification$progress)
  )
  if (!is.null(case$verification)) {
    runner_arguments$verified <- case$verification
    runner_arguments$verified_user <- "benchmark-user"
  }
  measured <- .benchmark_measure(
    function() do.call(run_plan, runner_arguments),
    collect_memory = collect_memory,
    normalize = .benchmark_sanitize_report
  )
  report <- measured$value

  stopifnot(
    inherits(plan, "redcapmissing_plan"),
    inherits(report, "redcapmissing"),
    nrow(report$target_results) == case$expected_targets,
    sum(report$target_results$field_complete == "failed") ==
      case$expected_effective_failed_targets,
    nrow(report$missing) == case$expected_effective_missing_rows,
    identical(names(report), c(
      "plan", "target_results", "summary", "missing", "verification",
      "diagnostics", "details"
    )),
    if (isTRUE(specification$details)) {
      is.data.frame(report$details) &&
        (nrow(report$details) > 0L || !case$expected_targets)
    } else {
      is.null(report$details)
    },
    if (!is.null(case$verification)) {
      isTRUE(report$verification$enabled) &&
        report$verification$input_rows == case$expected_verification_input &&
        report$verification$user_rows == case$expected_verification_user_rows &&
        report$verification$latest_user_rows ==
          case$expected_verification_contexts &&
        report$verification$verified_rows ==
          case$expected_verification_contexts &&
        report$verification$overrides_applied == case$expected_overrides
    } else {
      !isTRUE(report$verification$enabled)
    }
  )

  list(
    plan_elapsed = unname(plan_elapsed),
    run_elapsed = measured$elapsed,
    verification_elapsed = unname(verification_elapsed),
    allocated_bytes = measured$allocated_bytes,
    gc_elapsed_seconds = measured$gc_elapsed_seconds,
    result_size_mb = measured$result_size_mb,
    diagnostics = report$diagnostics,
    equivalence_hash = .benchmark_equivalence_hash(report),
    sanitized_report = .benchmark_sanitize_report(report)
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
  branch_mode <- specification$branch_mode
  if (is.null(branch_mode)) branch_mode <- "none"
  instrument_start_mode <- specification$instrument_start_mode
  if (is.null(instrument_start_mode)) instrument_start_mode <- "first"
  verification_mode <- specification$verification_mode
  if (is.null(verification_mode)) {
    verification_mode <- if (isTRUE(specification$verified)) "dense" else "disabled"
  }
  extra_columns <- specification$extra_columns
  if (is.null(extra_columns)) extra_columns <- 0L
  progress <- specification$progress
  if (is.null(progress)) progress <- FALSE
  data.frame(
    tier = tier,
    family = family,
    scenario = scenario,
    records = specification$records,
    instruments = specification$instruments,
    fields_per_instrument = specification$fields,
    targets = as.integer(targets),
    verified = !identical(verification_mode, "disabled"),
    details = isTRUE(specification$details),
    progress = isTRUE(progress),
    branch_mode = branch_mode,
    instrument_start_mode = instrument_start_mode,
    verification_mode = verification_mode,
    extra_columns = as.integer(extra_columns),
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
  invisible(.benchmark_once(case, specification, collect_memory = FALSE))
  constructor <- runner <- verification <- allocated <- gc_elapsed <-
    result_size <- numeric(iteration_count)
  diagnostics <- vector("list", iteration_count)
  raw <- vector("list", iteration_count)
  equivalence_hash <- character(iteration_count)
  sanitized_reports <- vector("list", iteration_count)
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
    equivalence_hash[[iteration]] <- result$equivalence_hash
    sanitized_reports[[iteration]] <- result$sanitized_report
    stage_sum <- sum(result$diagnostics$elapsed_seconds, na.rm = TRUE)
    raw[[iteration]] <- data.frame(
      tier = tier,
      family = specification$family,
      scenario = scenario,
      iteration = as.integer(iteration),
      records = as.integer(specification$records),
      instruments = as.integer(specification$instruments),
      fields_per_instrument = as.integer(specification$fields),
      targets = as.integer(case$expected_targets),
      physical_rows = as.integer(case$metrics$physical_rows),
      metadata_rows = as.integer(case$metrics$metadata_rows),
      data_columns = as.integer(case$metrics$data_columns),
      selected_fields = as.integer(case$metrics$selected_fields),
      total_branch_expressions = as.integer(
        case$metrics$total_branch_expressions
      ),
      unique_branch_expressions = as.integer(
        case$metrics$unique_branch_expressions
      ),
      branch_dependency_columns = as.integer(
        case$metrics$branch_dependency_columns
      ),
      verification_rows = as.integer(case$expected_verification_input),
      constructor_seconds = result$plan_elapsed,
      runner_seconds = result$run_elapsed,
      verification_seconds = result$verification_elapsed,
      stage_sum_seconds = stage_sum,
      unaccounted_runner_seconds = result$run_elapsed - stage_sum,
      allocated_mb = result$allocated_bytes / 1024^2,
      gc_seconds = result$gc_elapsed_seconds,
      result_size_mb = result$result_size_mb,
      equivalence_hash = result$equivalence_hash,
      stringsAsFactors = FALSE
    )
  }
  if (length(unique(equivalence_hash)) != 1L) {
    stop("Repeated benchmark executions produced non-equivalent reports.",
         call. = FALSE)
  }
  if (length(sanitized_reports) > 1L &&
      !all(vapply(
        sanitized_reports[-1L],
        function(value) identical(value, sanitized_reports[[1L]]),
        logical(1)
      ))) {
    stop("Repeated benchmark executions failed exact report equivalence.",
         call. = FALSE)
  }
  diagnostics <- do.call(rbind, diagnostics)
  raw <- do.call(rbind, raw)
  stages <- aggregate(
    elapsed_seconds ~ scenario + stage + operation,
    data = diagnostics,
    FUN = stats::median
  )
  names(stages)[names(stages) == "elapsed_seconds"] <- "median_seconds"
  stages <- stages[order(stages$scenario, stages$stage), , drop = FALSE]
  workload_descriptor <- list(
    tier = tier,
    family = specification$family,
    scenario = scenario,
    specification = specification,
    case_metrics = case$metrics,
    expected_targets = case$expected_targets,
    expected_verification_input = case$expected_verification_input
  )
  workload_hash <- digest::digest(
    workload_descriptor,
    algo = "sha256",
    serialize = TRUE
  )
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
    stages = stages,
    raw = raw,
    stage_raw = diagnostics,
    equivalence = data.frame(
      tier = tier,
      family = specification$family,
      scenario = scenario,
      workload_hash = workload_hash,
      equivalence_hash = equivalence_hash[[1L]],
      stringsAsFactors = FALSE
    ),
    reports = list(list(
      tier = tier,
      family = specification$family,
      scenario = scenario,
      workload_hash = workload_hash,
      report = sanitized_reports[[1L]]
    ))
  )
}

.benchmark_run_constructors <- function(tier, specification, collect_memory) {
  inputs <- .benchmark_constructor_inputs(specification)
  summaries <- vector("list", length(inputs))
  for (scenario_index in seq_along(inputs)) {
    scenario <- names(inputs)[[scenario_index]]
    input <- inputs[[scenario_index]]
    invisible(.benchmark_measure(input$operation, collect_memory = FALSE))
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
        redcapmissing:::.flex_event_instruments_build_table(report)
      },
      collect_memory = FALSE
    ))
    elapsed <- allocated <- gc_elapsed <- result_size <-
      numeric(specification$iterations)
    for (iteration in seq_len(specification$iterations)) {
      measured <- .benchmark_measure(
        function() {
          redcapmissing:::.flex_event_instruments_build_table(report)
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

.benchmark_combine_run_results <- function(results) {
  bind_component <- function(name) {
    out <- do.call(rbind, lapply(results, `[[`, name))
    rownames(out) <- NULL
    out
  }
  list(
    summary = bind_component("summary"),
    stages = bind_component("stages"),
    raw = bind_component("raw"),
    stage_raw = bind_component("stage_raw"),
    equivalence = bind_component("equivalence"),
    reports = unlist(lapply(results, `[[`, "reports"), recursive = FALSE)
  )
}

.benchmark_high_cardinality_specifications <- function(specification) {
  base <- utils::modifyList(specification, list(
    fields = 10L,
    failure_percent = 10,
    details = FALSE,
    verified = FALSE,
    verification_mode = "disabled",
    verification_prior = 0L,
    verification_ties = 1L,
    branch_mode = "none",
    instrument_start_mode = "first",
    extra_columns = 0L,
    progress = FALSE
  ))
  change <- function(...) utils::modifyList(base, list(...))
  list(
    U2 = change(fields = 2L),
    U10 = change(fields = 10L),
    `branch-shared` = change(branch_mode = "shared"),
    `branch-per-instrument` = change(branch_mode = "per-instrument"),
    `instrument-start-first` = change(failure_percent = 0),
    `instrument-start-last` = change(
      failure_percent = 0,
      instrument_start_mode = "last"
    ),
    `instrument-start-none` = change(
      failure_percent = 0,
      instrument_start_mode = "none"
    ),
    `failure-0` = change(failure_percent = 0),
    `failure-100` = change(failure_percent = 100),
    `details-compact` = change(details = FALSE),
    `details-detailed` = change(details = TRUE),
    `verification-off` = change(verification_mode = "disabled"),
    `verification-empty` = change(verification_mode = "empty"),
    `verification-sparse` = change(verification_mode = "sparse"),
    `verification-dense` = change(
      verification_mode = "dense",
      verification_prior = 6L,
      verification_ties = 3L
    ),
    `dependency-wide` = change(
      fields = 2L,
      branch_mode = "per-instrument"
    ),
    `data-wide` = change(fields = 2L, extra_columns = 4800L),
    `progress-off` = change(progress = FALSE),
    `progress-on` = change(progress = TRUE)
  )
}

.benchmark_run_high_cardinality <- function(
  tier,
  specification,
  collect_memory
) {
  scenarios <- .benchmark_high_cardinality_specifications(specification)
  requested <- selected_high_cardinality_scenarios
  if (!length(requested)) requested <- c("U2", "U10")
  if (identical(requested, "all")) requested <- names(scenarios)
  unknown <- setdiff(requested, names(scenarios))
  if (length(unknown)) {
    stop(
      paste0(
        "Unknown high-cardinality scenario(s): ",
        paste(unknown, collapse = ", "),
        ". Available scenarios are: ",
        paste(names(scenarios), collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  results <- lapply(requested, function(scenario) {
    scenario_specification <- scenarios[[scenario]]
    .benchmark_run_plan_scenario(
      tier = tier,
      scenario = scenario,
      specification = scenario_specification,
      case = .benchmark_case(scenario_specification),
      collect_memory = collect_memory
    )
  })
  .benchmark_combine_run_results(results)
}

.benchmark_run_tier <- function(name, specification, collect_memory) {
  specification <- .benchmark_apply_overrides(specification)
  family <- specification$family
  if (identical(family, "high-cardinality")) {
    return(.benchmark_run_high_cardinality(name, specification, collect_memory))
  }
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
    return(.benchmark_combine_run_results(results))
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
    return(.benchmark_combine_run_results(list(compact, detailed)))
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
.benchmark_bind_component <- function(results, name) {
  pieces <- lapply(results, function(result) result[[name]])
  pieces <- Filter(
    function(piece) is.data.frame(piece) && nrow(piece),
    pieces
  )
  if (!length(pieces)) return(data.frame())
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

.benchmark_collect_reports <- function(results) {
  pieces <- lapply(results, function(result) result$reports)
  pieces <- Filter(function(piece) !is.null(piece) && length(piece), pieces)
  if (!length(pieces)) return(list())
  unlist(pieces, recursive = FALSE)
}

.benchmark_report_key <- function(tier, family, scenario, workload_hash) {
  paste(tier, family, scenario, workload_hash, sep = "\u001f")
}

.benchmark_compare_equivalence <- function(
  current,
  current_reports,
  baseline_path
) {
  if (!file.exists(baseline_path)) {
    stop("REDCAPMISSING_BENCH_BASELINE does not exist.", call. = FALSE)
  }
  baseline <- readRDS(baseline_path)
  if (!is.list(baseline) || !identical(baseline$schema_version, 2L) ||
      !is.data.frame(baseline$equivalence) ||
      !is.list(baseline$reports)) {
    stop("The benchmark baseline artifact has an unsupported schema.",
         call. = FALSE)
  }
  columns <- c(
    "tier", "family", "scenario", "workload_hash", "equivalence_hash"
  )
  if (!all(columns %in% names(current)) ||
      !all(columns %in% names(baseline$equivalence))) {
    stop("The benchmark equivalence table is malformed.", call. = FALSE)
  }
  key <- function(data) {
    .benchmark_report_key(
      data$tier,
      data$family,
      data$scenario,
      data$workload_hash
    )
  }
  current_key <- key(current)
  baseline_key <- key(baseline$equivalence)
  baseline_index <- match(current_key, baseline_key)
  if (anyNA(baseline_index)) {
    stop(
      "The baseline artifact does not contain every selected workload.",
      call. = FALSE
    )
  }
  hash_mismatch <- current$equivalence_hash !=
    baseline$equivalence$equivalence_hash[baseline_index]

  report_key <- function(entry) {
    .benchmark_report_key(
      entry$tier,
      entry$family,
      entry$scenario,
      entry$workload_hash
    )
  }
  current_report_key <- vapply(current_reports, report_key, character(1))
  baseline_report_key <- vapply(baseline$reports, report_key, character(1))
  report_index <- match(current_report_key, baseline_report_key)
  if (length(report_index) != nrow(current) || anyNA(report_index)) {
    stop(
      "The baseline artifact does not contain every sanitized report.",
      call. = FALSE
    )
  }
  report_equal <- vapply(
    seq_along(current_reports),
    function(i) identical(
      current_reports[[i]]$report,
      baseline$reports[[report_index[[i]]]]$report
    ),
    logical(1)
  )
  mismatch <- hash_mismatch | !report_equal
  if (any(mismatch)) {
    stop(
      paste0(
        "Benchmark report equivalence failed for: ",
        paste(current$scenario[mismatch], collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  cat("Baseline report equivalence: passed for", nrow(current), "workload(s)\n")
  invisible(TRUE)
}

.benchmark_git_commit <- function() {
  value <- tryCatch(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(error) character()
  )
  if (!length(value) || !nzchar(trimws(value[[1L]]))) return(NA_character_)
  trimws(value[[1L]])
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
  results[[tier_index]] <- result
  print(result$stages, row.names = FALSE)
}

summary_output <- .benchmark_bind_component(results, "summary")
stages_output <- .benchmark_bind_component(results, "stages")
raw_output <- .benchmark_bind_component(results, "raw")
stage_raw_output <- .benchmark_bind_component(results, "stage_raw")
equivalence_output <- .benchmark_bind_component(results, "equivalence")
reports_output <- .benchmark_collect_reports(results)
cat("\nTier summary\n")
print(summary_output, row.names = FALSE)
if (nrow(raw_output)) {
  cat("\nRaw iteration rows\n")
  print(raw_output, row.names = FALSE)
}
cat("\nSession information\n")
print(utils::sessionInfo())

artifact <- list(
  schema_version = 2L,
  created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  git_commit = .benchmark_git_commit(),
  selected_tiers = selected_tiers,
  selected_high_cardinality_scenarios = selected_high_cardinality_scenarios,
  allocation_profiled_separately = isTRUE(collect_memory) ||
    "detail-allocation" %in% selected_tiers,
  summary = summary_output,
  stages = stages_output,
  raw = raw_output,
  stage_raw = stage_raw_output,
  equivalence = equivalence_output,
  reports = reports_output,
  session_info = capture.output(utils::sessionInfo())
)
if (nzchar(benchmark_baseline_path)) {
  if (!nrow(equivalence_output)) {
    stop("No run_plan equivalence rows were produced for comparison.",
         call. = FALSE)
  }
  .benchmark_compare_equivalence(
    equivalence_output,
    reports_output,
    benchmark_baseline_path
  )
}
if (nzchar(benchmark_output_path)) {
  output_parent <- dirname(benchmark_output_path)
  if (!dir.exists(output_parent)) {
    stop("The parent directory for REDCAPMISSING_BENCH_OUTPUT does not exist.",
         call. = FALSE)
  }
  saveRDS(artifact, benchmark_output_path, version = 3L)
  cat("Benchmark artifact:", normalizePath(benchmark_output_path), "\n")
}
