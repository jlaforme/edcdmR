REDCap_ds_filtering <- function(data, data_dictionary, id_variable, forms = NULL, variables = NULL) {
  #import dependencies
  library(tidyverse)
  
  # Clean data
  # Data dictionnary
  data_dictionary <- data_dictionary %>% 
    mutate(across(where(is.character), ~ na_if(., ""))) %>% 
    rename_with(~ ifelse(str_detect(., regex("var|field name", ignore_case = TRUE)), "var_name", .)) %>% 
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
    ungroup() %>%
    
    mutate(
      section_header = ifelse(
        !is.na(section_header) & grepl("<.*?>", section_header), 
        sapply(section_header, function(x) {
          tryCatch(xml_text(read_html(x)), error = function(e) NA)
        }), 
        section_header
      ),
      field_label = ifelse(
        !is.na(field_label) & grepl("<.*?>", field_label), 
        sapply(field_label, function(x) {
          tryCatch(xml_text(read_html(x)), error = function(e) NA)
        }), 
        field_label
      )
    )
  
  metadata_columns <- names(data)[!sapply(names(data), function(x) {
    base_var <- strsplit(x, "___")[[1]][1]
    base_var %in% data_dictionary$var_name | grepl("_complete$", x)
  })]
  
  
  # Fill the forms if empty
  if (is.null(forms)) {
    forms = unique(data_dictionary$form_name)
  }
  
  if (variables %in% c("All", "all") | is.null(variables)) {
    variables = names(data)
  }
  
  
  # Data
  data_filtered <- data %>% 
    mutate(across(where(is.character), ~na_if(., ""))) %>% 
    select({{id_variable}}, metadata_columns, starts_with(data_dictionary$var_name[data_dictionary$form_name %in% forms])) %>% 
    filter(redcap_repeat_instrument %in% forms) %>%
    filter(!if_all(-c({{id_variable}}, metadata_columns), is.na)) %>% 
    select({{id_variable}}, all_of(metadata_columns), any_of(variables))
  
  return(data_filtered)
}