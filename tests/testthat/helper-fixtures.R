meta_row <- function(
  field_name,
  form_name,
  field_type = "text",
  field_label = field_name,
  choices = "",
  validation = "",
  branching = "",
  required = ""
) {
  tibble::tibble(
    field_name = field_name,
    form_name = form_name,
    field_type = field_type,
    field_label = field_label,
    select_choices_or_calculations = choices,
    text_validation_type_or_show_slider_number = validation,
    branching_logic = branching,
    required_field = required
  )
}

fake_rcon <- function(
  metadata,
  instruments = NULL,
  mapping = NULL,
  repeat_instrument_event = NULL,
  project_information = NULL
) {
  if (is.null(instruments)) {
    forms <- unique(as.character(metadata$form_name))
    instruments <- tibble::tibble(
      instrument_name = forms,
      instrument_label = paste(forms, "label")
    )
  }

  list(
    metadata = function() metadata,
    instruments = function() instruments,
    mapping = function() mapping,
    mappings = function() mapping,
    repeatInstrumentEvent = function() repeat_instrument_event,
    projectInformation = function() project_information
  )
}

baseline_form_meta <- function() {
  dplyr::bind_rows(
    meta_row("record_id", "baseline_form", field_label = "Record ID", required = "y"),
    meta_row("branch_flag", "baseline_form", field_type = "yesno", field_label = "Branch flag", required = "y"),
    meta_row("required_note", "baseline_form", field_label = "Required note", required = "y"),
    meta_row("optional_note", "baseline_form", field_label = "Optional note"),
    meta_row(
      "checkbox_field",
      "baseline_form",
      field_type = "checkbox",
      field_label = "Checkbox field",
      choices = "1, First | 2, Second",
      required = "y"
    ),
    meta_row(
      "checkbox_other",
      "baseline_form",
      field_label = "Checkbox other",
      branching = "[checkbox_field(2)] = '1'",
      required = "y"
    ),
    meta_row(
      "conditional_note",
      "baseline_form",
      field_label = "Conditional note",
      branching = "[branch_flag] = '1'",
      required = "y"
    ),
    meta_row(
      "descriptive_text",
      "baseline_form",
      field_type = "descriptive",
      field_label = "Descriptive text",
      required = "y"
    )
  )
}
