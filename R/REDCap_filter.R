#' Filter REDCap dataset
#'
#' @description Reruens a filtered dataset based on forms and variables arguments.
#'
#' @param data Dataset to filter
#' @param dictionary Data dictionary
#' @param id_variable Name of the identifier variable
#' @param forms Forms to include; defaults to all
#' @param variables Variables to include; defaults to all
#'
#' @returns A filtered dataframe
#' @export
#'
#' @examples REDCap_filter(data = REDCap_data, dictionary = REDCap_dd, id_variable = "record_id")
#' @examples REDCap_filter(data = REDCap_data, dictionary = REDCap_dd, id_variable = "record_id", forms = c("form1", "form2", "..."), variables = c("var1", "var2", "..."))

REDCap_filter <- function(data, dictionary, id_variable, forms = NULL, variables = NULL) {
  library(dplyr)
  library(xml2)
  library(tidyr)
  library(stringr)
data <- project$data
dictionary <- project$dictionary
variables = c("iec_q005", "pld_q001", "pld_q003___0", "pld_q003___1", "pld_q003___2", "pld_q003___3", "pld_q003___4", "pld_q003___5", "pld_q003___6", "pld_q003___7", "pld_q003____7777")
  # Clean data
  # Data dictionnary
  dictionary <- dictionary %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    rename_with(~ ifelse(str_detect(., regex("var|field name|field_name", ignore_case = TRUE)), "var_name", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("branching|logic", ignore_case = TRUE)), "branching_logic", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("field type", ignore_case = TRUE)), "field_type", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("header|section", ignore_case = TRUE)), "section_header", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("field label", ignore_case = TRUE)), "field_label", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("required", ignore_case = TRUE)), "required_field", .)) %>%

    filter(field_type != "descriptive") %>%

    mutate(section_header = ifelse(str_trim(section_header) == "", NA, section_header)) %>%

    group_by(form_name) %>%
    fill(section_header, .direction = "down") %>%
    ungroup()

  metadata_columns <- names(data)[!sapply(names(data), function(x) {
    base_var <- strsplit(x, "___")[[1]][1]
    base_var %in% dictionary$var_name | grepl("_complete$", x)
  })]


  # Define the id_variable
  id_variable <- dictionary$var_name[1]

  # Fill the forms if empty
  if (is.null(forms)) {
    forms <- c(NA, unique(dictionary$form_name))
  }

  if (is.null(variables) || length(variables) == 0 || any(variables %in% c("All", "all"))) {
    variables <- names(data)
  }


  # Data
  data_filtered <- data %>%
    mutate(across(where(is.character), ~na_if(., ""))) %>%
    select({{id_variable}}, all_of(metadata_columns), starts_with(dictionary$var_name[dictionary$form_name %in% forms])) %>%
    filter(redcap_repeat_instrument %in% forms) %>%
    filter(!if_all(-c({{id_variable}}, all_of(metadata_columns)), is.na)) %>%
    select({{id_variable}}, all_of(metadata_columns), any_of(variables))

  return(data_filtered)
}
