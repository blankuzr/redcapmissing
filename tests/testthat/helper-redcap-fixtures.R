redcap_api_connection_fixture <- function(x) {
  structure(x, class = c("redcapApiConnection", "redcapConnection"))
}

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
