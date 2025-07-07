#' Filter REDCap dataset
#'
#' @description Reruens a filtered dataset based on forms and variables arguments.
#'
#' @param data Dataset to filter
#' @param dictionary Data dictionary
#' @param id_variable Name of the identifier variable
#' @param forms Forms to include; defaults to all
#' @param variables Variables to include; defaults to all
#' @param remove_na Optional argument to remove rows where all data is missing/NA
#'
#' @returns A filtered dataframe
#' @export
#'
#' @examples REDCap_filter(data = REDCap_data, dictionary = REDCap_dd, id_variable = "record_id")
#' @examples REDCap_filter(data = REDCap_data, dictionary = REDCap_dd, id_variable = "record_id", forms = c("form1", "form2", "..."), variables = c("var1", "var2", "..."))

REDCap_filter <- function(data, dictionary, id_variable, forms = NULL, variables = NULL, remove_na = FALSE) {
  library(dplyr)
  library(xml2)
  library(tidyr)
  library(stringr)

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
  metadata_columns <- metadata_columns[grepl("redcap", metadata_columns)]

  # Define the id_variable
  id_variable <- dictionary$var_name[1]

  # Fill the forms if empty
  if (is.null(forms)) {
    forms <- c(NA, unique(dictionary$form_name))
  }

  if (any(!forms %in% data$redcap_repeat_instrument)) {
    forms <- c(forms, "NA", NA)
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

  if (isTRUE(remove_na)){
    data_filtered <- data_filtered %>%
      filter(rowSums(!is.na(select(., -all_of(metadata_columns)))) > 0)
  }

  return(data_filtered)
}
