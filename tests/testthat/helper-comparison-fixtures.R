comparison_reports_fixture <- function(
  verification = TRUE,
  shared_event = FALSE
) {
  metadata <- dplyr::bind_rows(
    meta_row('record_id', 'visit'),
    meta_row('visit_start', 'visit'),
    meta_row('visit_a', 'visit', required = 'y'),
    meta_row('visit_b', 'visit', required = 'y'),
    meta_row('diary_start', 'diary'),
    meta_row('diary_a', 'diary', required = 'y'),
    meta_row('diary_b', 'diary', required = 'y')
  )
  mapping <- tibble::tibble(
    arm_num = 1L,
    unique_event_name = c('baseline_arm_1', 'followup_arm_1'),
    form = c('visit', 'diary')
  )
  if (shared_event) {
    mapping <- dplyr::bind_rows(
      mapping,
      tibble::tibble(
        arm_num = 1L,
        unique_event_name = 'baseline_arm_1',
        form = 'diary'
      )
    )
  }
  rcon <- redcap_api_connection_fixture(list(
    url = 'https://example.test/api/',
    metadata = function() metadata,
    instruments = function() {
      tibble::tibble(
        instrument_name = c('visit', 'diary'),
        instrument_label = c('Visit', 'Diary')
      )
    },
    projectInformation = function() {
      tibble::tibble(
        project_id = '77',
        is_longitudinal = 1L,
        has_repeating_instruments_or_events = 1L
      )
    },
    arms = function() tibble::tibble(arm_num = 1L, name = 'Arm 1'),
    events = function() {
      tibble::tibble(
        event_id = c(101L, 102L),
        unique_event_name = c('baseline_arm_1', 'followup_arm_1'),
        event_name = c('Baseline', 'Follow up'),
        arm_num = 1L
      )
    },
    mapping = function() mapping,
    repeatInstrumentEvent = function() {
      tibble::tibble(event_name = 'followup_arm_1', form_name = 'diary')
    },
    version = function() '15.0.0'
  ))
  record_row <- function(
    id,
    event,
    instance = NA_integer_,
    start = 'yes',
    a = 'ok',
    b = 'ok'
  ) {
    diary <- identical(event, 'followup_arm_1')
    tibble::tibble(
      record_id = id,
      redcap_event_name = event,
      redcap_repeat_instrument = if (diary) 'diary' else NA_character_,
      redcap_repeat_instance = instance,
      visit_start = if (!diary) start else '',
      visit_a = if (!diary) a else '',
      visit_b = if (!diary) b else '',
      diary_start = if (diary) start else '',
      diary_a = if (diary) a else '',
      diary_b = if (diary) b else ''
    )
  }
  previous_data <- dplyr::bind_rows(
    record_row('001', 'baseline_arm_1', a = ''),
    record_row('002', 'baseline_arm_1'),
    record_row('003', 'baseline_arm_1', start = '', a = '', b = ''),
    record_row('001', 'followup_arm_1', 2L, a = ''),
    record_row('002', 'followup_arm_1', 2L, a = ''),
    record_row('003', 'followup_arm_1', 1L)
  )
  current_data <- dplyr::bind_rows(
    record_row('001', 'baseline_arm_1'),
    record_row('002', 'baseline_arm_1'),
    record_row('003', 'baseline_arm_1', a = ''),
    record_row('004', 'baseline_arm_1', start = '', a = '', b = ''),
    record_row('002', 'followup_arm_1', 2L, a = ''),
    record_row('003', 'followup_arm_1', 2L, a = '')
  )
  schedule <- function(visit_ids, diary_ids) {
    dplyr::bind_rows(
      tibble::tibble(
        record_id = visit_ids,
        instrument = 'visit',
        redcap_event_name = 'baseline_arm_1',
        repeat_instance = NA_integer_
      ),
      tibble::tibble(
        record_id = diary_ids,
        instrument = 'diary',
        redcap_event_name = 'followup_arm_1',
        repeat_instance = 2L
      ),
      if (shared_event) {
        tibble::tibble(
          record_id = visit_ids,
          instrument = 'diary',
          redcap_event_name = 'baseline_arm_1',
          repeat_instance = NA_integer_
        )
      }
    )
  }
  previous_plan <- plan_explicit(
    previous_data,
    rcon,
    schedule(c('001', '002', '003'), c('001', '002', '003'))
  )
  current_plan <- plan_explicit(
    current_data,
    rcon,
    schedule(c('001', '002', '003', '004'), c('002', '003'))
  )
  verified <- tibble::tibble(
    project_id = '77',
    record = '002',
    event_id = '102',
    field_name = 'diary_a',
    repeat_instrument = 'diary',
    instance = 2L,
    ts = '2026-01-02 10:00:00',
    current_query_status = 'VERIFIED',
    username = 'reviewer_a'
  )
  previous <- run_plan(
    previous_plan,
    previous_data,
    rcon,
    details = TRUE,
    progress = FALSE
  )
  current <- run_plan(
    current_plan,
    current_data,
    rcon,
    verified = if (verification) verified else NULL,
    verified_user = if (verification) 'reviewer_a' else NULL,
    details = TRUE,
    progress = FALSE
  )

  list(previous = previous, current = current)
}
