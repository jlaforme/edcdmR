#' This function generates a missing variables report
#'
#' @param ... List containing the REDCap data, dictionary and event form mapping (if required). This should be the output of the `REDCap_import` function.
#' @param data Data frame containing the data read from REDCap. If a list (...) is specified, this argument is not required.
#' @param dictionary Data frame containing the REDCap dictionary. If a list (...) is specified, this argument is not required.
#' @param event_form Data frame containing the mapping of each form with each event. If a list (...) is specified, this argument is not required.
#' @param InstrumentSkip Data frame containing the mapping of each form with each event. If a list (...) is specified, this argument is not required.
#' @param event_description Data frame containing the description of each event. If a list (...) is specified, this argument is not required.
#' @param id_variable Character vector of lenght 1 containing the name of the ID variable (identifier).
#' @param start_of_tx_variable Character vector of lenght 1 containing the name of the variable containing the start of treatment date.
#'
#' @returns
#' @export
#'
#' @examples

REDCap_missing_value_report <- function(..., data = NULL, dictionary = NULL, event_form = NULL, InstrumentSkip = NULL, event_description = NULL, id_variable = "record_id", start_of_tx_variable = NULL) {

  library(dplyr)
  library(xml2)
  library(progress)
  library(tidyr)
  library(stringr)
  library(purrr)

  # Assign project
  project <- c(...)

  # Checks
  if(!is.null(project)){

    if(!is.null(data)){
      warning("Data has been specified twice so the function will not use the information in the data argument.")
    }

    if(!is.null(dictionary)){
      warning("Dictionary has been specified twice so the function will not use the information in the dictionary argument.")
    }

    data <- project$data
    dictionary <- project$dictionary

    if("event_form" %in% names(project)) {
      if(!is.null(event_form)){
        warning("Event form has been specified twice so the function will not use the information in the event_form argument.")
      }
      event_form <- as.data.frame(project$event_form)
    }

    if("event_description" %in% names(project)) {
      if(!is.null(event_description)){
        warning("Event description form has been specified twice so the function will not use the information in the event_description argument.")
      }
      event_description <- as.data.frame(project$event_description)
    }
  }

  # Assign project object
  if(!is.null(project)){
    dictionary <- project$dictionary
    data <- project$data
    event_form <- project$event_form
    event_description <- project$event_description
  }


  # Declare variables and objects
  if (is.null(start_of_tx_variable)) {
    t0_variable = Sys.Date() - 1
  } else {
    t0_variable <- start_of_tx_variable # define which variable (date) refers as the start of the study data collection
  }

  id_variable <- id_variable # define which variable contains the participant's ID

  if (!t0_variable %in% names(data) && is.Date(t0_variable)) {
    data$t0_variable <- t0_variable
    t0_variable = "t0_variable"
  }



  # Clean data and data_dictionnary
  # data
  data <- data %>%
    mutate(across(where(function(x) inherits(x, "labelled")), as.character)) %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    rename(record_id = !!sym(id_variable))
  colnames(data) <- gsub("____", "___", colnames(data))

  # dictionary
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
    ) %>%

    filter(!var_name == id_variable)



  # Create empty dataset if the dataset is not available
  # InstrumentSkip
  if (is.null(InstrumentSkip)) {
    InstrumentSkip <- data.frame(
      form_name = character(),
      event_name = character(),
      control_condition = character(),
      stringsAsFactors = FALSE
    )
  }

  # EventDescription missing and event_form not missing
  if (is.null(event_description) & exists("event_form")) {
    event_form <- event_form %>%
      mutate(across(where(is.character), ~na_if(., ""))) %>%
      rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>%
      rename_with(~ ifelse(str_detect(., regex("event", ignore_case = TRUE)), "event_name", .))

    event_description <- event_form %>%
      rename_with(~ ifelse(str_detect(., regex("event", ignore_case = TRUE)), "unique_event_name", .)) %>%
      select(unique_event_name) %>%
      distinct() %>%
      mutate(day_offset = 0,
             offset_min = 0,
             offset_max = 0)

  }

  # event_description and event_form missing
  if (is.null(event_description) & is.null(event_form)) {
    event_description <- data.frame(
      event_name = "Baseline",
      day_offset = 0,
      offset_min = 0,
      offset_max = 0,
      unique_even_name = "baseline",
      stringsAsFactors = FALSE
    )

    event_form <- dictionary %>%
      select(form_name) %>%
      distinct() %>%
      mutate(unique_event_name = "baseline")

    data$redcap_event_name <- "baseline"
  }

  # Create redcap_repeat_instrument if does not exist in data
  if (!"redcap_repeat_instrument" %in% names(data)) {
    data <- data %>%
      mutate(redcap_repeat_instrument = NA_character_) %>%
      relocate(redcap_repeat_instrument, .after = 1)
  }


  # Clean InstrumentSkip, and event_form
  # InstrumentSkip
  InstrumentSkip <- InstrumentSkip %>%
    mutate(across(where(is.character), ~na_if(., ""))) %>%
    mutate(across(where(is.character), ~na_if(., ""))) %>%
    rename_with(~ ifelse(str_detect(., regex("event", ignore_case = TRUE)), "event_name", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("control|condition", ignore_case = TRUE)), "control_condition", .)) %>%
    mutate(control_condition = gsub("\n", "", control_condition))

  # event_form
  event_form <- event_form %>%
    mutate(across(where(is.character), ~na_if(., ""))) %>%
    rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>%
    rename_with(~ ifelse(str_detect(., regex("event", ignore_case = TRUE)), "event_name", .))



  # Process redcap conditions
  validate_redcap_condition <- function(condition_dataset, branching_logic_column) {
    # Function to validate each branching logic expression

    # Apply the validation to each cell in the branching logic column
    validate_logic <- function(cell, row_index) {
      if (!is.na(cell)) {
        # Try to parse the branching logic as an R expression
        result <- tryCatch({
          # Replace [i] with the current row index inside the expression
          cell_with_i <- str_replace_all(cell, "\\[i\\]", paste0("[", row_index, "]"))

          # Substitute the row index (i) dynamically in the branching logic
          expr <- parse(text = cell_with_i)  # Parse the modified expression

          # Evaluate the expression in the context of the current row
          eval_result <- eval(expr, envir = data[row_index, , drop = FALSE])

          # If the expression includes na.omit, strip the "omit" class
          if (inherits(eval_result, "omit")) {
            eval_result <- unclass(eval_result)  # Remove "omit" class
          }

          # Return TRUE if the evaluation is successful and logical (TRUE/FALSE)
          if (is.logical(eval_result)) {
            all(eval_result, na.rm = TRUE)  # Ensure the result is logical and NA values are removed
          } else {
            message("Invalid logical expression in row ", row_index)
            FALSE
          }

        }, error = function(e) {
          message("Error in branching logic (row ", row_index, "): ", cell, " - ", e$message)
          FALSE  # If an error occurs, return FALSE
        })

        return(result)
      }
      return(TRUE)  # Return TRUE if the cell is NA (not a problem)
    }

    # Iterate over the rows in the data and apply the validation
    validation_results <- sapply(1:nrow(data), function(i) {
      validate_logic(condition_dataset[[branching_logic_column]][i], i)
    })

    return(validation_results)
  }

  # Branching logic
  branching_logic <- dictionary %>%
    select(form_name, var_name, branching_logic)

  branching_logic <- REDCap_logic_parser(data = data, dictionary = dictionary, logic = branching_logic, column_name = "branching_logic")
  validation_results <- validate_redcap_condition(branching_logic, "branching_logic")

  # Control Condition (InstrumentSkip)
  if (nrow(InstrumentSkip) > 0) {
    InstrumentSkip <- REDCap_logic_parser(data = data, dictionary = dictionary, logic = InstrumentSkip, column_name = "control_condition")
    validation_results <- validate_redcap_condition(InstrumentSkip, "control_condition")
  }



  # Generate the matrix of missing variables
  matrix_missing <- data.frame(matrix(0,
                                      nrow = nrow(data),
                                      ncol = ncol(data)))

  # Identify metadata columns as those not present in the data dictionary and exclude '_complete' columns
  metadata_columns <- names(data)[!sapply(names(data), function(x) {
    base_var <- strsplit(x, "___")[[1]][1]
    base_var %in% dictionary$var_name | grepl("_complete$", x)
  })]

  # Dynamically determine the starting column for looping
  start_col <- which(!names(data) %in% metadata_columns)[1]

  # Ensure start_col is not 1L, if it is, shift to the next valid column
  if (start_col == 1L) {
    start_col <- which(!names(data)[2:ncol(data)] %in% metadata_columns)[1] + 1  # Shift to next non-metadata column
  }


  # Loop through the matrix_missing
  # Set a progression bar
  pb <- progress::progress_bar$new(
    total = nrow(data),
    format = "  [:bar] :percent Estimated time left: :eta",
    clear = FALSE, width = 150
  )


  # Initiate the table to store the excluded form for InstrumentSkip
  for (i in 1:nrow(data)) {
    for (ii in start_col:ncol(data)) {

      # Extract the base variable name from the column name (before the "___")
      variable_name <- strsplit(names(data)[ii], "___")[[1]][1]

      # Check if the base variable is present in the `branching_logic` dataset
      if (variable_name %in% branching_logic$var_name) {

        # Fetch the corresponding branching logic from the `branching_logic` dataset
        logic <- branching_logic$branching_logic[branching_logic$var_name == variable_name]

        # Fetch the form and event name associated with the base variable
        form_name <- dictionary$form_name[dictionary$var_name == variable_name]
        event_name <- data$redcap_event_name[i]

        form_logic <- InstrumentSkip$control_condition[InstrumentSkip$form_name == form_name & InstrumentSkip$event_name == event_name]
        if (length(form_logic) == 0 || is.na(form_logic)) {
          form_logic <- InstrumentSkip$control_condition[InstrumentSkip$form_name == form_name]
        }


        if (length(form_logic) == 0) {
          form_logic <- NA
        }

        # Check if the event matches the form
        if (data$redcap_event_name[i] %in% event_form$event_name[event_form$form_name == form_name]) {

          if (dictionary$form_name[dictionary$var_name == variable_name] == data$redcap_repeat_instrument[i] | is.na(data$redcap_repeat_instrument[i])) {


            # Check if the form is a repeated instrument
            is_repeated <- form_name %in% data$redcap_repeat_instrument

            if (is_repeated) {
              if (length(eval(parse(text = form_logic))) > 0) {
                if (!is.na(eval(parse(text = form_logic))) & isFALSE(eval(parse(text = form_logic)))) {
                  matrix_missing[i, ii] <- 7
                  next  # Skip to next iteration if form_logic is FALSE
                }
                else if (length(eval(parse(text = logic))) > 0) {
                  if (!is.na(data[i, ii]) & ((!is.na(eval(parse(text = logic))) & eval(parse(text = logic))) | is.na(logic))) {
                    matrix_missing[i, ii] <- 0
                    next

                  } else if ((is.na(data[i, ii]) & is.na(logic) & (is.na(form_logic) | (!is.na(eval(parse(text = form_logic))) & eval(parse(text = form_logic)))))  & (!is.na(data$redcap_repeat_instrument[i]) &&
                                                                                                                                                                       data$redcap_repeat_instrument[i] == form_name)) {
                    matrix_missing[i, ii] <- 1
                    next

                  } else if ((is.na(data[i, ii]) & (!is.na(eval(parse(text = logic))) & eval(parse(text = logic)))) & (!is.na(data$redcap_repeat_instrument[i]) &&
                                                                                                                       data$redcap_repeat_instrument[i] == form_name)) {
                    matrix_missing[i, ii] <- 1
                    next

                  } else {
                    matrix_missing[i, ii] <- 0
                    next
                  }
                } else {
                  # If form_logic is not FALSE, assign 99
                  matrix_missing[i, ii] <- 99
                  next
                }
              }
              else {
                # If form_logic is empty or NA, mark as irrelevant (99)
                matrix_missing[i, ii] <- 99
                next  # Skip to next iteration
              }
            }

            # Check if the logic result is an empty vector
            if (length(eval(parse(text = logic))) > 0) {
              if (length(eval(parse(text = form_logic))) > 0) {

                data[i, ii]
                # Evaluate condition based on the branching logic
                if (!is.na(eval(parse(text = form_logic))) & isFALSE(eval(parse(text = form_logic)))) {
                  # Append i and ii to the dataset when the condition is true
                  matrix_missing[i, ii] <- 7

                } else if (is.na(data[i, ii]) & is.na(logic) & (is.na(form_logic) | (!is.na(eval(parse(text = form_logic))) & eval(parse(text = form_logic))))) {
                  matrix_missing[i, ii] <- 1

                } else if (is.na(data[i, ii]) & (!is.na(eval(parse(text = logic))) & eval(parse(text = logic)))) {
                  matrix_missing[i, ii] <- 1

                } else if (!is.na(data[i, ii]) &
                           ((!is.na(eval(parse(text = logic))) & eval(parse(text = logic))) | is.na(logic))) {
                  matrix_missing[i, ii] <- 0
                }

              } else {
                # Mark as complete if eval form_logic is empty (that means that the form_logic contained a NA var)
                matrix_missing[i, ii] <- 0
              }

            } else {
              # Mark as complete if eval logic is empty (that means that the logic contained a NA var)
              matrix_missing[i, ii] <- 0
            }

          } else {
            # Mark as irrelevant if the variable form does not match the data form
            matrix_missing[i, ii] <- 99
          }

        } else {
          # Mark as irrelevant if the event does not match the form
          matrix_missing[i, ii] <- 99
        }

      } else {
        # If the variable is not present in branching_logic, assign 99
        matrix_missing[i, ii] <- 99
      }
    }

    # Update progress bar after processing each row (i), only if it's not finished
    if (!pb$finished) {
      pb$tick()
    }
  }
  rm(pb, i, ii, form_logic, logic, is_repeated, variable_name, validation_results)


  # Set back to the original data names
  names(matrix_missing) <- names(data)
  matrix_missing[, 1:(start_col-1)] <- data[, 1:(start_col-1)]



  # Create an index of rows and columns where matrix_missing values are 1 (real missing) and create a table of missing variables
  idx_missing <- which(matrix_missing[, start_col:ncol(matrix_missing)] == 1, arr.ind = TRUE)
  idx_missing[, "col"] <- idx_missing[, "col"] + (start_col-1)
  idx_missing <- as_tibble(idx_missing, .name_repair = "minimal")

  table_missing <- data.frame(matrix(NA, nrow = nrow(idx_missing), ncol = 5))
  names(table_missing) <- c("record_id", "redcap_event_name", "redcap_repeat_instance", "form_name", "var_name")

  variable_names_missing <- sapply(names(matrix_missing)[idx_missing$col], function(x) strsplit(x, "___")[[1]][1])
  variable_exists_missing <- variable_names_missing %in% dictionary$var_name

  table_missing$record_id <- matrix_missing[idx_missing$row, which(names(matrix_missing) == "record_id")]
  table_missing$redcap_event_name <- matrix_missing[idx_missing$row, which(names(matrix_missing) == "redcap_event_name")]
  table_missing$redcap_repeat_instance <- matrix_missing[idx_missing$row, which(names(matrix_missing) == "redcap_repeat_instance")]
  # If there is no redcap_repeat_instance in data, create an empty one
  if ("redcap_repeat_instance" %in% colnames(table_missing) && (all(is.na(table_missing$redcap_repeat_instance))|all(table_missing$redcap_repeat_instance == "")) && nrow(table_missing) > 0) {
    table_missing$redcap_repeat_instance <- NA
  }
  table_missing$var_name <- names(matrix_missing)[idx_missing$col]
  table_missing$form_name <- ifelse(variable_exists_missing, dictionary$form_name[match(variable_names_missing, dictionary$var_name)], NA)

  rm(idx_missing, variable_names_missing, variable_exists_missing)


  # Create an index of rows and columns where matrix_missing values are 0 (complete) and create the table of complete questionnaires
  idx_complete <- which(matrix_missing[, start_col:ncol(matrix_missing)] == 0, arr.ind = TRUE)
  idx_complete[, "col"] <- idx_complete[, "col"] + (start_col-1)
  idx_complete <- as_tibble(idx_complete, .name_repair = "minimal")

  table_complete <- data.frame(matrix(NA, nrow = nrow(idx_complete), ncol = 5))
  names(table_complete) <- c("record_id", "redcap_event_name", "redcap_repeat_instance", "form_name", "var_name")

  variable_names_complete <- sapply(names(matrix_missing)[idx_complete$col], function(x) strsplit(x, "___")[[1]][1])
  variable_exists_complete <- variable_names_complete %in% dictionary$var_name

  table_complete$record_id <- matrix_missing[idx_complete$row, which(names(matrix_missing) == "record_id")]
  table_complete$redcap_event_name <- matrix_missing[idx_complete$row, which(names(matrix_missing) == "redcap_event_name")]
  table_complete$redcap_repeat_instance <- matrix_missing[idx_complete$row, which(names(matrix_missing) == "redcap_repeat_instance")]
  # If there is no redcap_repeat_instance in data, create an empty one
  if ("redcap_repeat_instance" %in% colnames(table_complete) && (all(is.na(table_complete$redcap_repeat_instance))|all(table_complete$redcap_repeat_instance == "")) && nrow(table_complete) > 0) {
    table_complete$redcap_repeat_instance <- NA
  }
  table_complete$var_name <- names(matrix_missing)[idx_complete$col]
  table_complete$form_name <- ifelse(variable_exists_complete, dictionary$form_name[match(variable_names_complete, dictionary$var_name)], NA)

  rm(idx_complete, variable_names_complete, variable_exists_complete)

  # Clean table_complete
  table_complete <- table_complete %>%
    mutate(var_name = str_remove(var_name, "___.*")) %>%
    unique() %>%
    group_by(record_id, form_name, redcap_event_name) %>%
    summarise(
      all_present = all(dictionary$var_name[dictionary$form_name == unique(form_name)] %in% var_name),
      .groups = "drop") %>%
    filter(all_present == TRUE)



  # Create the table containing the table_missing (missing variables) and the "unstarted questionnaires" (InstrumentDesignation - table_missing - table_complete)
  # Create an index of rows and columns where matrix_missing values are 7 (excluded form for a specific ID)
  idx_excluded_form <- which(matrix_missing[, start_col:ncol(matrix_missing)] == 7, arr.ind = TRUE)
  idx_excluded_form[, "col"] <- idx_excluded_form[, "col"] + (start_col-1)
  idx_excluded_form <- as_tibble(idx_excluded_form, .name_repair = "minimal")


  table_excluded_form <- data.frame(matrix(NA, nrow = nrow(idx_excluded_form), ncol = 5))
  names(table_excluded_form) <- c("record_id", "redcap_event_name", "redcap_repeat_instance", "form_name", "var_name")

  variable_names_excluded_form <- sapply(names(matrix_missing)[idx_excluded_form$col], function(x) strsplit(x, "___")[[1]][1])
  variable_exists_excluded_form <- variable_names_excluded_form %in% dictionary$var_name

  table_excluded_form$record_id <- matrix_missing[idx_excluded_form$row, which(names(matrix_missing) == "record_id")]
  table_excluded_form$redcap_event_name <- matrix_missing[idx_excluded_form$row, which(names(matrix_missing) == "redcap_event_name")]
  table_excluded_form$redcap_repeat_instance <- matrix_missing[idx_excluded_form$row, which(names(matrix_missing) == "redcap_repeat_instance")]
  # If there is no redcap_repeat_instance in data, create an empty one
  if ("redcap_repeat_instance" %in% colnames(table_excluded_form) && (all(is.na(table_excluded_form$redcap_repeat_instance))|all(table_excluded_form$redcap_repeat_instance == "")) && nrow(table_excluded_form) > 0) {
    table_excluded_form$redcap_repeat_instance <- NA
  }
  table_excluded_form$var_name <- names(matrix_missing)[idx_excluded_form$col]
  table_excluded_form$form_name <- as.character(ifelse(variable_exists_excluded_form, dictionary$form_name[match(variable_names_excluded_form, dictionary$var_name)], NA))
  table_excluded_form <- table_excluded_form %>%
    select(record_id, redcap_event_name, form_name) %>%
    distinct()

  rm(idx_excluded_form, variable_names_excluded_form, variable_exists_excluded_form)

  # Create the unstarted questionnaire table
  table_unstarted_qx <- merge(event_form, as.data.frame(unique(data$record_id)), by = NULL) %>%
    rename(record_id = "unique(data$record_id)") %>%
    rename(redcap_event_name = event_name) %>%
    anti_join(table_complete, by = c("record_id", "redcap_event_name", "form_name")) %>%
    anti_join(table_missing, by = c("record_id", "redcap_event_name", "form_name")) %>%
    anti_join(table_excluded_form, by = c("record_id", "redcap_event_name", "form_name")) %>%

    mutate(var_name = "All")

  # Create the complete report (table_missing + table_unstarted_qx)
  missing_cols <- setdiff(names(table_missing), names(table_unstarted_qx))
  table_unstarted_qx[missing_cols] <- NA
  table_unstarted_qx <- table_unstarted_qx[names(table_missing)]
  rm(missing_cols)

  # Filter out non overdue variables/questionnaires
  table_missing <- table_missing %>%
    rbind(table_unstarted_qx) %>%
    left_join(data %>%
                select(!!sym(id_variable), !!sym(t0_variable)) %>%
                group_by(!!sym(id_variable)) %>%
                fill(!!sym(t0_variable), .direction = "down") %>%
                fill(!!sym(t0_variable), .direction = "up") %>%
                arrange(!!sym(id_variable), !!sym(t0_variable)) %>%
                distinct() %>%
                slice(1),
              by = "record_id") %>%
    group_by(record_id) %>%
    fill(!!sym(t0_variable), .direction = "down") %>%
    fill(!!sym(t0_variable), .direction = "up") %>%
    ungroup() %>%

    filter(!is.na(!!sym(t0_variable))) %>%

    left_join(event_description %>%
                select(unique_event_name, day_offset, offset_max, offset_min),
              by = c("redcap_event_name" = "unique_event_name")) %>%

    mutate(overdue_days = as.numeric(difftime(Sys.Date(), as.character(!!sym(t0_variable)), units = "days"))) %>%
    filter(overdue_days > day_offset + offset_max) %>%
    mutate(overdue_days = overdue_days - day_offset - offset_max) %>%

    select(-c(!!sym(t0_variable), day_offset, offset_max, offset_min))

  # Add the question header and label, required, and add the t0_variable
  table_missing <- table_missing %>%
    left_join(dictionary %>% select(var_name, form_name, section_header, field_label, required_field), by = c("var_name", "form_name")) %>%
    left_join(data %>%
                select(!!sym(id_variable), !!sym(t0_variable)) %>%
                group_by(!!sym(id_variable)) %>%
                fill(!!sym(t0_variable), .direction = "down") %>%
                fill(!!sym(t0_variable), .direction = "up") %>%
                arrange(!!sym(id_variable), !!sym(t0_variable)) %>%
                distinct() %>%
                slice(1) %>%
                rename(start_of_tx = !!sym(t0_variable)),
              by = id_variable) %>%
    relocate(start_of_tx, .after = !!sym(id_variable)) %>%
    relocate(section_header, .after = var_name) %>%
    relocate(field_label, .after = section_header) %>%
    relocate(required_field, .after = field_label)

  forms_with_required <- dictionary %>%
    filter(required_field == "y") %>%
    distinct(form_name) %>%
    pull(form_name)

  table_missing <- table_missing %>%
    mutate(required_field = if_else(
      var_name == "All" & form_name %in% forms_with_required,
      "y",
      required_field
    ))



  # Clean the output if needed
  if (is.null(start_of_tx_variable)) {
    table_missing <- table_missing %>%
      select(-overdue_days)
  }

  if (all(is.na(table_missing$redcap_repeat_instance))) {
    table_missing <- table_missing %>%
      select(-redcap_repeat_instance)
  }

  return(table_missing)
}



