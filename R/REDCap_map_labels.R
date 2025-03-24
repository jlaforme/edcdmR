#' Map metadata labels to data values
#'
#'@description Allows to map caterogical values of a REDCap dataset to their respective data dictionary labels. Only applicable to radio and dropdown variables.
#'
#' @param data The REDCap data to map
#' @param dictionary The REDCap data dictionary
#' @param variables Variable(s) to map; defaults to all
#'
#' @returns A dataset with mapped values
#' @export
#'
#' @examples

REDCap_map_labels <- function(data, dictionary, variables = "All") {
dictionary <- project$dictionary
data <- project$data
  # Clean the dd
  dictionary <- dictionary %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    rename_with(~ ifelse(str_detect(., regex("var|field name|field_name", ignore_case = TRUE)), "var_name", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("branching|logic", ignore_case = TRUE)), "branching_logic", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("field type", ignore_case = TRUE)), "field_type", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("header|section", ignore_case = TRUE)), "section_header", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("field label", ignore_case = TRUE)), "field_label", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("required", ignore_case = TRUE)), "required_field", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("choice|coding", ignore_case = TRUE)), "coding", .)) %>%

    filter(field_type %in% c("radio", "dropdown", "checkbox")) %>%
    filter(nchar(coding) > 2) %>%

    mutate(coding = gsub("(?<=\\d),(?= )", "` = \"", coding, perl = TRUE),
           coding = gsub("\\| ", '", `', coding),
           coding = paste0("`", coding, '"'),  # Add " at the end
           coding = gsub('=\\s+"\\s*([^"]+?)\\s*"', '= "\\1"', coding),
           coding = str_trim(coding))  # Trim spaces inside quotes

  new_rows <- dictionary %>%
    # Filter to process only the rows where field_type is "checkbox"
    filter(field_type == "checkbox") %>%
    # Separate the 'coding' text into multiple rows by splitting by commas
    mutate(coding = str_extract_all(coding, "`[0-9-]+` = \"[^\"]+\"")) %>%
    unnest(coding) %>%
    # Extract the numeric code and corresponding description (without quotes)
    mutate(
      num = str_extract(coding, "`([0-9-]+)`"),  # Extract the numbers between the backticks
      description = str_extract(coding, "(?<= = \").*(?=\")"),  # Extract the description after '=' without quotes
      var_name = paste(var_name, num, sep = "___"),
      var_name = gsub("`", "", var_name),
      var_name = gsub("_-", "__", var_name),
      coding = str_replace_all(coding, "\`[^\"]*\`", "\`1\`")) %>%
    select(-c(num, description))

  dictionary <- dictionary %>%
    filter(field_type != "checkbox") %>%
    rbind(new_rows)



  # Clean the variables argument
  if (is.null(variables) || any(variables %in% c("All", "all", "ALL"))) {
    variables <- names(data)
  }

  dictionary <- dictionary %>%
    filter(var_name %in% c(variables))



  valid_vars <- intersect(dictionary$var_name, names(data))


  data_labels_conv <- data %>%
    mutate(across(all_of(valid_vars),
                  ~ {
                    # Check the column class and convert to character if logical
                    if (is.logical(.)) {
                      . <- as.character(.)
                    }
                    eval(parse(text = paste0("recode(", dictionary$var_name[dictionary$var_name == cur_column()], ", ", dictionary$coding[dictionary$var_name == cur_column()], ", .default = NA_character_)")))
                  }))

  return(data_labels_conv)
}




