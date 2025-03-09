REDCap_missing_value_report <- function(data, data_dictionary, InstrumentDesignations = NULL, InstrumentSkip = NULL, EventsDescription = NULL, id_variable = "record_id", start_of_tx_variable) {
  
  # Install/Load required packages
  install_and_load <- function(packages) {
    for (pkg in packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
    }
  }
  install_and_load(c("tidyverse", "rio", "progress", "xml2"))

  # Declare variables
  t0_variable <- start_of_tx_variable  # define which variable (date) refers as the start of the study data collection
  id_variable <- id_variable  # define which variable contains the participant's ID
  
  
  
  # Clean data and data_dictionnary
   # data
  data <- data %>% 
    mutate(across(where(is.character), ~na_if(., ""))) %>% 
    rename(record_id = !!sym(id_variable))
  colnames(data) <- gsub("____", "___", colnames(data))
  
   # data_dictionary
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
    ) %>% 
    
    filter(!var_name == id_variable)
  
  
  
  # Create empty dataset if the dataset is not available
   # InstrumentSkip
  if (is.null("InstrumentSkip")) {
    InstrumentSkip <- data.frame(
      form_name = character(),
      event_name = character(),
      control_condition = character(),
      stringsAsFactors = FALSE
    )
  }
  
   # EventDescription missing and InstrumentDesignations not missing
  if (is.null("EventsDescription") & exists("InstrumentDesignations")) {
    InstrumentDesignations <- InstrumentDesignations %>% 
      mutate(across(where(is.character), ~na_if(., ""))) %>% 
      rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>% 
      rename_with(~ ifelse(str_detect(., regex("event", ignore_case = TRUE)), "event_name", .))
    
    EventsDescription <- InstrumentDesignations %>% 
      select(unique_event_name) %>% 
      distinct() %>% 
      mutate(day_offset = 0,
             offset_min = 0,
             offset_max = 0)
    
  }
  
   # EventsDescription and InstrumentDesignations missing
  if (is.null("EventsDescription") & is.null("InstrumentDesignations")) {
    EventsDescription <- data.frame(
      event_name = "Baseline",
      day_offset = 0,
      offset_min = 0,
      offset_max = 0,
      unique_even_name = "baseline",
      stringsAsFactors = FALSE
    )
    
    InstrumentDesignations <- data_dictionary %>% 
      select(form_name) %>% 
      distinct() %>% 
      mutate(unique_event_name = "baseline")
    
    data$redcap_event_name <- "baseline"
  }
  
  
  
  # Clean InstrumentSkip, and InstrumentDesignations
   # InstrumentSkip
  InstrumentSkip <- InstrumentSkip %>% 
    mutate(across(where(is.character), ~na_if(., ""))) %>% 
    mutate(across(where(is.character), ~na_if(., ""))) %>% 
    rename_with(~ ifelse(str_detect(., regex("event", ignore_case = TRUE)), "event_name", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("control", ignore_case = TRUE)), "control_condition", .))
  
   # InstrumentDesignations
  InstrumentDesignations <- InstrumentDesignations %>% 
    mutate(across(where(is.character), ~na_if(., ""))) %>% 
    rename_with(~ ifelse(str_detect(., regex("form", ignore_case = TRUE)), "form_name", .)) %>% 
    rename_with(~ ifelse(str_detect(., regex("event", ignore_case = TRUE)), "event_name", .))
  
  
  
  # Process redcap conditions
  process_redcap_conditions <- function(data, column_name) {
    # Define a function to process a single branching logic string
    process_cell <- function(cell) {
      if (!is.na(cell)) {
        
        # Remove trailing spaces from the cell
        cell <- str_trim(cell)
        
        # Handle multiple-choice question format: pdh_q011(-7777) -> pdh_q011___7777
        # and pdh_q011(3) -> pdh_q011___3
        cell <- str_replace_all(cell, "\\(([-]?\\d+)\\)", function(x) {
          # Extract the number inside parentheses and reformat as ___number
          paste0("___", str_extract(x, "\\d+"))
        })
        
        # Handle `datediff` logic transformation
        if (str_detect(cell, "datediff")) {
          # Extract the full `datediff` function call and its parameters
          match <- str_match(
            cell,
            "datediff\\(\\s*\\[([^\\]]+)\\],\\s*\\[([^\\]]+)\\],\\s*'([^']+)',\\s*'([^']+)',\\s*(true|false)\\)"
          )
          
          if (!is.na(match[1])) {
            # Parameters extracted:
            first_var <- match[2]
            second_var <- match[3]
            unit <- match[4]
            date_format <- match[5]
            include_time <- match[6] == "true"
            
            # Create the R equivalent transformation
            r_equivalent <- paste0(
              "interval(", date_format, "(data$", first_var, "), ", 
              date_format, "(data$", second_var, ")) / ",
              ifelse(unit == "M", "months(1)", ifelse(unit == "Y", "years(1)", "days(1)"))
            )
            
            # Replace the `datediff` portion
            cell <- str_replace(cell, "datediff\\(.*?\\)", r_equivalent)
          }
        }
        
        # General transformations for branching logic
        if (str_detect(cell, "\\]\\[")) {
          # Extract all event-variable pairs
          events <- str_extract_all(cell, "(?<=\\[)[^\\]\\[]*(?=\\]\\[)")
          called_vars <- str_extract_all(cell, "(?<=\\]\\[).*?(?=\\])")
          
          # Additional transformations
          cell <- str_replace_all(cell, "\\]\\[", "\\^")
          
          for (i in seq_along(events[[1]])) {
            event <- events[[1]][i]
            cell <- str_replace_all(cell, paste0("\\[", event), event)  # Replace each event
          }
          
          for (i in seq_along(called_vars[[1]])) {
            called_var <- called_vars[[1]][i]
            cell <- str_replace_all(cell, paste0(called_var, "\\]"), called_var)  # Replace each called variable
          }
          
          cell <- str_replace_all(cell, "\\[", "data$")
          cell <- str_replace_all(cell, "\\]", "[i]")
          cell <- str_replace_all(cell, "\\bor\\b|\\bOR\\b", "|")
          cell <- str_replace_all(cell, "\\band\\b|\\bAND\\b", "&")
          cell <- str_replace_all(cell, "=", "==")
          cell <- str_replace_all(cell, ">==", ">=")
          cell <- str_replace_all(cell, "<==", "<=")
          cell <- str_replace_all(cell, "<>", "!=")
          
          for (i in seq_along(events[[1]])) {
            event <- events[[1]][i]
            called_var <- called_vars[[1]][i]
            
            # Construct the R equivalent for each event-calling variable pair
            cell <- str_replace_all(cell, paste0(event, "\\^", called_var),
                                    paste0(
                                      "data[which(data$redcap_event_name == \"", 
                                      event, 
                                      "\" & data$record_id == data$record_id[i]), \"", 
                                      called_var, "\"]"
                                    ))
          }
          
          cell <- str_replace_all(cell, "event-name", "redcap_event_name")
          cell <- str_replace_all(cell, '""', '"')
          cell <- gsub("data\\$data\\[", "data\\[", cell)
          
        } else {
          # Single event transformation
          cell <- str_replace_all(cell, "\\[", "data$")
          cell <- str_replace_all(cell, "\\]", "[i]")
          cell <- str_replace_all(cell, "\\bor\\b|\\bOR\\b", "|")
          cell <- str_replace_all(cell, "\\band\\b|\\bAND\\b", "&")
          cell <- str_replace_all(cell, "=", "==")
          cell <- str_replace_all(cell, ">==", ">=")
          cell <- str_replace_all(cell, "<==", "<=")
          cell <- str_replace_all(cell, "<>", "!=")
          cell <- str_replace_all(cell, "event-name", "redcap_event_name")
          cell <- str_replace_all(cell, '""', '"')
          cell <- gsub("data\\$data\\[", "data\\[", cell)
        }
        
        # Add na.omit to handle NA values in the conditions
        cell <- paste0("unique(na.omit(", cell, "))")
        cell <- str_trim(cell)
        cell <- gsub("[\r\n]", "", cell)
      }
      return(cell)
    }
    
    # Apply the function to the specified column
    data[[column_name]] <- sapply(data[[column_name]], process_cell)
    return(data)
  }
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
  branching_logic <- data_dictionary %>% 
    select(form_name, var_name, branching_logic)
  
  branching_logic <- process_redcap_conditions(branching_logic, "branching_logic")
  validation_results <- validate_redcap_condition(branching_logic, "branching_logic")
  
   # Control Condition (InstrumentSkip)
  if (nrow(InstrumentSkip) > 0) {
    InstrumentSkip <- process_redcap_conditions(InstrumentSkip, "control_condition")
    validation_results <- validate_redcap_condition(InstrumentSkip, "control_condition")
  }
  
  
  
  # Generate the matrix of missing variables
  matrix_missing <- data.frame(matrix(0,
                                      nrow = nrow(data),
                                      ncol = ncol(data)))
  
   # Identify metadata columns as those not present in the data dictionary and exclude '_complete' columns
  metadata_columns <- names(data)[!sapply(names(data), function(x) {
    base_var <- strsplit(x, "___")[[1]][1]
    base_var %in% data_dictionary$var_name | grepl("_complete$", x)
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
        form_name <- data_dictionary$form_name[data_dictionary$var_name == variable_name]
        event_name <- data$redcap_event_name[i]
        
        form_logic <- InstrumentSkip$control_condition[InstrumentSkip$form_name == form_name & InstrumentSkip$event_name == event_name]
        if (length(form_logic) == 0 || is.na(form_logic)) {
          form_logic <- InstrumentSkip$control_condition[InstrumentSkip$form_name == form_name]
        }
        
        
        if (length(form_logic) == 0) {
          form_logic <- NA
        }
        
        # Check if the event matches the form
        if (data$redcap_event_name[i] %in% InstrumentDesignations$event_name[InstrumentDesignations$form_name == form_name]) {
          
          if (data_dictionary$form_name[data_dictionary$var_name == variable_name] == data$redcap_repeat_instrument[i] | is.na(data$redcap_repeat_instrument[i])) {
            
            
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
  variable_exists_missing <- variable_names_missing %in% data_dictionary$var_name
  
  table_missing$record_id <- matrix_missing[idx_missing$row, which(names(matrix_missing) == "record_id")]
  table_missing$redcap_event_name <- matrix_missing[idx_missing$row, which(names(matrix_missing) == "redcap_event_name")]
  table_missing$redcap_repeat_instance <- matrix_missing[idx_missing$row, which(names(matrix_missing) == "redcap_repeat_instance")]
  table_missing$var_name <- names(matrix_missing)[idx_missing$col]
  table_missing$form_name <- ifelse(variable_exists_missing, data_dictionary$form_name[match(variable_names_missing, data_dictionary$var_name)], NA)
  
  rm(idx_missing, variable_names_missing, variable_exists_missing)
  
  
  # Create an index of rows and columns where matrix_missing values are 0 (complete) and create the table of complete questionnaires
  idx_complete <- which(matrix_missing[, start_col:ncol(matrix_missing)] == 0, arr.ind = TRUE)
  idx_complete[, "col"] <- idx_complete[, "col"] + (start_col-1)
  idx_complete <- as_tibble(idx_complete, .name_repair = "minimal")
  
  table_complete <- data.frame(matrix(NA, nrow = nrow(idx_complete), ncol = 5))
  names(table_complete) <- c("record_id", "redcap_event_name", "redcap_repeat_instance", "form_name", "var_name")
  
  variable_names_complete <- sapply(names(matrix_missing)[idx_complete$col], function(x) strsplit(x, "___")[[1]][1])
  variable_exists_complete <- variable_names_complete %in% data_dictionary$var_name
  
  table_complete$record_id <- matrix_missing[idx_complete$row, which(names(matrix_missing) == "record_id")]
  table_complete$redcap_event_name <- matrix_missing[idx_complete$row, which(names(matrix_missing) == "redcap_event_name")]
  table_complete$redcap_repeat_instance <- matrix_missing[idx_complete$row, which(names(matrix_missing) == "redcap_repeat_instance")]
  table_complete$var_name <- names(matrix_missing)[idx_complete$col]
  table_complete$form_name <- ifelse(variable_exists_complete, data_dictionary$form_name[match(variable_names_complete, data_dictionary$var_name)], NA)
  
  rm(idx_complete, variable_names_complete, variable_exists_complete)
  
  # Clean table_complete
  table_complete <- table_complete %>%
    mutate(var_name = str_remove(var_name, "___.*")) %>% 
    unique() %>% 
    group_by(record_id, form_name, redcap_event_name) %>%
    summarise(
      all_present = all(data_dictionary$var_name[data_dictionary$form_name == unique(form_name)] %in% var_name),
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
  variable_exists_excluded_form <- variable_names_excluded_form %in% data_dictionary$var_name
  
  table_excluded_form$record_id <- matrix_missing[idx_excluded_form$row, which(names(matrix_missing) == "record_id")]
  table_excluded_form$redcap_event_name <- matrix_missing[idx_excluded_form$row, which(names(matrix_missing) == "redcap_event_name")]
  table_excluded_form$redcap_repeat_instance <- matrix_missing[idx_excluded_form$row, which(names(matrix_missing) == "redcap_repeat_instance")]
  table_excluded_form$var_name <- names(matrix_missing)[idx_excluded_form$col]
  table_excluded_form$form_name <- as.character(ifelse(variable_exists_excluded_form, data_dictionary$form_name[match(variable_names_excluded_form, data_dictionary$var_name)], NA))
  table_excluded_form <- table_excluded_form %>% 
    select(record_id, redcap_event_name, form_name) %>% 
    distinct()
  
  rm(idx_excluded_form, variable_names_excluded_form, variable_exists_excluded_form)
  
   # Create the unstarted questionnaire table
  table_unstarted_qx <- merge(InstrumentDesignations, as.data.frame(unique(data$record_id)), by = NULL) %>% 
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
    
    left_join(EventsDescription %>% 
                select(unique_event_name, day_offset, offset_max, offset_min),
              by = c("redcap_event_name" = "unique_event_name")) %>% 
    
    mutate(overdue_days = as.numeric(difftime(Sys.Date(), as.character(!!sym(t0_variable)), units = "days"))) %>% 
    filter(overdue_days > day_offset + offset_max) %>% 
    mutate(overdue_days = overdue_days - day_offset - offset_max) %>% 
    
    select(-c(!!sym(t0_variable), day_offset, offset_max, offset_min))
  
   # Add the question header and label, required, and add the t0_variable
  table_missing <- table_missing %>% 
    left_join(data_dictionary %>% select(var_name, form_name, section_header, field_label, required_field), by = c("var_name", "form_name")) %>% 
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
    relocate(required_field, .after = field_label) %>% 
    mutate(required_field = ifelse(var_name == "All", "y", required_field))
  
  return(table_missing)
}


