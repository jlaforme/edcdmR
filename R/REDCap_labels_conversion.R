REDCap_labels_conversion <- function(data, data_dictionary, variables = "All") {
  # Load dependencies
  library(dplyr)
    
  # Clean the dd
  data_dictionary <- data_dictionary %>% 
    mutate(across(where(is.character), ~ na_if(., ""))) %>% 
    rename_with(~ ifelse(str_detect(., regex("var|field name", ignore_case = TRUE)), "var_name", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("branching|logic", ignore_case = TRUE)), "branching_logic", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("field type", ignore_case = TRUE)), "field_type", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("header|section", ignore_case = TRUE)), "section_header", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("field label", ignore_case = TRUE)), "field_label", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("required", ignore_case = TRUE)), "required_field", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("choice|coding", ignore_case = TRUE)), "coding", .)) %>%
    
    filter(field_type %in% c("radio", "dropdown")) %>% 
    filter(nchar(coding) > 2) %>% 
    
    mutate(coding = gsub("(?<=\\d),(?= )", "` = \"", coding, perl = TRUE),
           coding = gsub("\\|", '", `', coding),
           coding = paste0("`", coding, '"'),  # Add " at the end
           coding = gsub('=\\s+"\\s*([^"]+?)\\s*"', '= "\\1"', coding))  # Trim spaces inside quotes
  
  # Clean the variables argument
  if (any(variables %in% c("All", "all", "ALL"))) {
    variables <- data_dictionary$var_name
  }

  data_dictionary <- data_dictionary %>% 
    filter(var_name %in% c(variables))

  
  
  valid_vars <- intersect(data_dictionary$var_name, names(data))
  
  
  data_labels_conv <- data %>%
    mutate(across(all_of(valid_vars), 
                  ~ {
                    # Check the column class and convert to character if logical
                    if (is.logical(.)) {
                      . <- as.character(.)
                    }
                    eval(parse(text = paste0("recode(", data_dictionary$var_name[data_dictionary$var_name == cur_column()], ", ", data_dictionary$coding[data_dictionary$var_name == cur_column()], ")")))
                  }))
  
  return(data_labels_conv)
}
  



