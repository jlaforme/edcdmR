
#' Map variable label to variable name
#'
#' @param data The REDCap data to map
#' @param dictionary The REDCap data dictionary
#' @param variables Variable(s) to map; defaults to all
#'
#' @returns A dataset with mapped labels
#' @export
#'
#' @examples

REDCap_map_names <- function(data, dictionary, variables = "All"){
  library(dplyr)
  library(tidyr)


  # Clean the dd
  dictionary <- dictionary %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    rename_with(~ case_when(
      str_detect(., regex("var|field name|field_name", ignore_case = TRUE)) ~ "var_name",
      str_detect(., regex("form", ignore_case = TRUE)) ~ "form_name",
      str_detect(., regex("branching|logic", ignore_case = TRUE)) ~ "branching_logic",
      str_detect(., regex("field type", ignore_case = TRUE)) ~ "field_type",
      str_detect(., regex("header|section", ignore_case = TRUE)) ~ "section_header",
      str_detect(., regex("field label", ignore_case = TRUE)) ~ "field_label",
      str_detect(., regex("required", ignore_case = TRUE)) ~ "required_field",
      TRUE ~ .
    )) %>%

    filter(field_type != "descriptive") %>%

    mutate(section_header = ifelse(str_trim(section_header) == "", NA, section_header)) %>%

    group_by(form_name) %>%
    fill(section_header, .direction = "down") %>%
    ungroup() %>%

    mutate(
      section_header = ifelse(
        !is.na(section_header) & grepl("<.*?>", section_header),
        purrr::map_chr(section_header, ~ tryCatch(xml_text(read_html(.x)), error = function(e) NA)),
        section_header
      ),
      field_label = ifelse(
        !is.na(field_label) & grepl("<.*?>", field_label),
        purrr::map_chr(field_label, ~ tryCatch(xml_text(read_html(.x)), error = function(e) NA)),
        field_label
      )
    )


  dictionary <- dictionary %>%
    select(var_name, section_header, field_label) %>%
    mutate(section_header = str_remove(section_header, ":$")) %>%
    mutate(var_full_name = ifelse(is.na(section_header), field_label, paste0(section_header, ": ", field_label))) %>%
    select(var_name, var_full_name)



  base_names <- sub("___.*$", "", names(data))

  # Match against dictionary
  matched <- match(base_names, dictionary$var_name)

  # Build new names (drop suffix completely)
  new_names <- ifelse(
    is.na(matched),
    names(data),  # keep old if no match
    dictionary$var_full_name[matched]  # just the label
  )

  # Assign
  names(data) <- new_names

  return(data)
}
