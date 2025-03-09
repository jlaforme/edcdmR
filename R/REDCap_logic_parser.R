#' Translate REDCap logic to R
#'
#' @param data_dictionary REDCap data dictionary
#' @param column_name Name of the column containing containing the REDCap logic in the data dictionary
#' @param missing_data_codes Codes used to specify missing data; defaults to c("NA", "na")
#'
#' @returns A dataframe with "column_name" parsed
#' @export
#'
#' @examples REDCap_logic_parser(data_dictionary = REDCap_data_dictionary, column_name = "branching_logic")
#' @examples REDCap_logic_parser(data_dictionary = dd_export, column_name = "branching logic", missing_data_codes = c("-9999", "-999"))

REDCap_logic_parser <- function(data_dictionary, column_name, missing_data_codes = NULL) {

  # Default missing data codes
  default_missing_data_codes <- c("NA", "na")

  # Flag to check if isblankormissingcode() is detected
  isblankormissingcode_detected <- FALSE

  # Load dependant functions
  remove_last_parenthesis <- function(expr) {
    # Count the number of opening and closing parentheses
    open_paren_count <- str_count(expr, "\\(")
    close_paren_count <- str_count(expr, "\\)")

    # If the number of parentheses is odd, remove the last closing parenthesis
    if ((open_paren_count + close_paren_count) %% 2 == 1) {
      # Remove the last closing parenthesis
      expr <- substr(expr, 1, nchar(expr) - 1)
    }
    return(expr)
  }
  add_quotes_if_no_brackets <- function(strings) {
    sapply(strings, function(string) {
      if (!grepl("\\[.*\\]", string)) {
        return(paste0("'", string, "'"))  # Use double quotes instead of single quotes
      } else {
        return(string)  # Keep as is if it has brackets
      }
    })
  }

  # Checks
  if (!column_name %in% names(data_dictionary)) {
    stop(paste0("column_name ", "'", column_name, "'", " is not present in your data_dictionary"))
  }
  if (!is.data.frame(data_dictionary)) {
    stop("The provided data_dictionary is not a data frame")
  }

  # Clean the data_dictionary
  data_dictionary <- data_dictionary %>%
    mutate(across(all_of(column_name), ~ ifelse(. == "", NA, .)))


  # Define a function to process a single branching logic string
  process_cell <- function(cell) {
    if (!is.na(cell)) {

      # Remove trailing spaces from the cell
      cell <- str_trim(cell)

      # Handle multiple-choice question format: pdh_q011(-7777) -> pdh_q011___7777
      cell <- str_replace_all(cell, "\\(([-]?\\d+)\\)", function(x) {
        paste0("___", str_extract(x, "\\d+"))
      })


      # Handle `if()` function
      if (str_detect(cell, "if\\(")) {
        match <- str_match(cell, "if\\(([^,]+),\\s*([^,]+),\\s*(.*)\\)")

        if (!is.na(match[1])) {

          replace_if_condition <- function(cell, r_equivalent) {
            # Find the position of the first "if("
            start_pos <- str_locate(cell, "if\\(")[1]

            # If "if(" is not found, return the original cell
            if (is.na(start_pos)) {
              return(cell)
            }

            # Now we need to find the corresponding closing parenthesis
            open_paren_count <- 1
            close_pos <- start_pos + 3  # Start after "if("

            # Loop through the string, counting parentheses
            while (open_paren_count > 0 && close_pos <= nchar(cell)) {
              if (substr(cell, close_pos, close_pos) == "(") {
                open_paren_count <- open_paren_count + 1
              } else if (substr(cell, close_pos, close_pos) == ")") {
                open_paren_count <- open_paren_count - 1
              }
              close_pos <- close_pos + 1
            }

            # Extract the portion to replace
            if (open_paren_count == 0) {
              # Replace the matched "if(...)" with the r_equivalent
              cell <- paste0(substr(cell, 1, start_pos - 1), r_equivalent, substr(cell, close_pos, nchar(cell)))
            }

            return(cell)
          }

          condition <- match[2]
          true_value <- match[3]
          false_value <- match[4]

          condition <- remove_last_parenthesis(condition)
          true_value <- remove_last_parenthesis(true_value)
          false_value <- remove_last_parenthesis(false_value)

          r_equivalent <- paste0(
            "ifelse(", condition, ", ", true_value, ", ", false_value,  ")"
          )

          cell <- replace_if_condition(cell, r_equivalent)
        }
      }

      cell <- "datediff([dob], [today], 'Y', 'mdy', true) > 18"
      # Handle `datediff` logic transformation
      if (str_detect(cell, "datediff")) {
        match <- str_match(cell, "datediff\\(([^,]+),\\s*([^,]+),\\s*([^,]+)(?:,\\s*(.*))?\\)")


        if (!is.na(match[1])) {
          first_var <- match[2]
          second_var <- match[3]
          unit <- match[4]
          returnSignedValue <- match[5]

          first_var <- remove_last_parenthesis(first_var)
          second_var <- remove_last_parenthesis(second_var)
          unit <- remove_last_parenthesis(unit)
          unit <- gsub("'", "", unit)
          returnSignedValue <- if (!is.na(returnSignedValue)) {
            remove_last_parenthesis(returnSignedValue)
          } else {
            NA
          }

          r_equivalent <- ifelse(returnSignedValue %in% c("false", "FALSE", "F", "f", NA),
                                 paste0(
                                   "abs(interval(",
                                   ifelse(first_var == "'today'", "Sys.Date()", first_var),
                                   ", ",
                                   ifelse(second_var == "'today'", "Sys.Date()", second_var),
                                   ") / ",
                                   ifelse(unit == "M", "months(1)",
                                          ifelse(unit == "y", "years(1)",
                                                 ifelse(unit == "d", "days(1)",
                                                        ifelse(unit == "h", "hours(1)",
                                                               ifelse(unit == "m", "minutes(1)", "seconds(1)"))))), ")"),

                                 paste0(
                                   "interval(", "[",
                                   ifelse(first_var == "'today'", "Sys.Date()", first_var),
                                   ", ",
                                   ifelse(second_var == "'today'", "Sys.Date()", second_var),
                                   ") / ",
                                   ifelse(unit == "M", "months(1)",
                                          ifelse(unit == "y", "years(1)",
                                                 ifelse(unit == "d", "days(1)",
                                                        ifelse(unit == "h", "hours(1)",
                                                               ifelse(unit == "m", "minutes(1)", "seconds(1)")))))))

          cell <- str_replace(cell, "datediff\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `isblankormissingcode()`
      if (str_detect(cell, "isblankormissingcode")) {
        isblankormissingcode_detected <<- TRUE  # Set flag to TRUE

        match <- str_match(cell, "isblankormissingcode\\(\\s*\\[([^\\]]+)\\]\\s*\\)")

        if (!is.na(match[1])) {
          variable <- match[2]

          variable <- remove_last_parenthesis(variable)

          r_equivalent <- paste0(
            "is.na([", variable, "]) | [", variable, "] = '' | [", variable, "] %in% c(",
            paste(missing_data_codes %||% default_missing_data_codes, collapse = ", "), ")"
          )

          cell <- str_replace(cell, "isblankormissingcode\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `roundup`
      if (str_detect(cell, "roundup")) {
        message("A 'roundup' function was detected in your REDCap logic(s), there is no built in 'roundup' function in R. Before using the resulting logic(s), please load the following function in your environment:
        roundup <- function(x, digits = 0) {
          multiplier <- 10^digits
          ceiling(x * multiplier) / multiplier
        }")
        roundup <- function(x, digits = 0) {
          multiplier <- 10^digits
          ceiling(x * multiplier) / multiplier
        }
      }


      # Handle `rounddown`
      if (str_detect(cell, "roundup")) {
        message("A 'rounddown' function was detected in your REDCap logic(s), there is no built in 'rounddown' function in R. Before using the resulting logic(s), please load the following function in your environment:
        rounddown <- function(x, digits = 0) {
          multiplier <- 10^digits
          floor(x * multiplier) / multiplier
        }")
        rounddown <- function(x, digits = 0) {
          multiplier <- 10^digits
          floor(x * multiplier) / multiplier
        }

      }


      # Handle `mod()` (modulo)
      if (str_detect(cell, "mod(")) {
        match <- str_match(cell, "mod\\(([^,]+),\\s*([^\\)]+)\\)")


        if (!is.na(match[1])) {
          dividend <- match[2]
          divisor <- match[3]

          dividend <- remove_last_parenthesis(dividend)
          divisor <- remove_last_parenthesis(divisor)

          r_equivalent <- paste0("(", dividend, " %% ", divisor, ")")

          cell <- str_replace(cell, "mod\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `stdev()`
      if (str_detect(cell, "stdev(")) {
       cell <- gsub("stdv", "sd", cell)
      }


      # Handle `isnumber()`
      if (str_detect(cell, "isnumber(")) {
        cell <- gsub("isnumber", "is.numeric", cell)
      }


      # Handle `isinteger()`
      if (str_detect(cell, "isinteger(")) {
        cell <- gsub("isinteger", "is.integer", cell)
      }


      # Handle `length()`
      if (str_detect(cell, "length(")) {
        cell <- gsub("length ", "nchar", cell)
      }


      # Handle `contains()`
      if (str_detect(cell, "contains(")) {
        match <- str_match(cell, "contains\\(([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string1 <- match[2]
          string2 <- match[3]

          string1 <- remove_last_parenthesis(string1)
          string2 <- remove_last_parenthesis(string2)

          string1 <- add_quotes_if_no_brackets(string1)
          string2 <- add_quotes_if_no_brackets(string2)

          r_equivalent <- paste0("grepl(", string2, ", ", string1,  ", ignore.case = TRUE, fixed = TRUE)")

          cell <- str_replace(cell, "contains\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `not_contain()`
      if (str_detect(cell, "not_contain(")) {
        match <- str_match(cell, "not_contain\\(([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string1 <- match[2]
          string2 <- match[3]

          string1 <- remove_last_parenthesis(string1)
          string2 <- remove_last_parenthesis(string2)

          string1 <- add_quotes_if_no_brackets(string1)
          string2 <- add_quotes_if_no_brackets(string2)

          r_equivalent <- paste0("!grepl(", string2, ", ", string1,  ", ignore.case = TRUE, fixed = TRUE)")

          cell <- str_replace(cell, "not_contain\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `not_contain()`
      if (str_detect(cell, "not_contain(")) {
        match <- str_match(cell, "not_contain\\(([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string1 <- match[2]
          string2 <- match[3]

          string1 <- remove_last_parenthesis(string1)
          string2 <- remove_last_parenthesis(string2)

          string1 <- add_quotes_if_no_brackets(string1)
          string2 <- add_quotes_if_no_brackets(string2)


          r_equivalent <- paste0("!grepl(", string2, ", ", string1,  ", ignore.case = TRUE, fixed = TRUE)")

          cell <- str_replace(cell, "not_contain\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `starts_with()`
      if (str_detect(cell, "starts_with(")) {
        match <- str_match(cell, "starts_with\\(([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string1 <- match[2]
          string2 <- match[3]

          string1 <- remove_last_parenthesis(string1)
          string2 <- remove_last_parenthesis(string2)

          string1 <- add_quotes_if_no_brackets(string1)
          string2 <- add_quotes_if_no_brackets(string2)

          r_equivalent <- paste0("grepl('^' %>% paste0(", string2, "), ", string1,  ", ignore.case = TRUE)")

          cell <- str_replace(cell, "starts_with\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `ends_with()`
      if (str_detect(cell, "ends_with(")) {
        match <- str_match(cell, "ends_with\\(([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string1 <- match[2]
          string2 <- match[3]

          string1 <- remove_last_parenthesis(string1)
          string2 <- remove_last_parenthesis(string2)

          string1 <- add_quotes_if_no_brackets(string1)
          string2 <- add_quotes_if_no_brackets(string2)

          r_equivalent <- paste0("grepl(", string2, " %>% paste0('$'), ", string1,  ", ignore.case = TRUE)")

          cell <- str_replace(cell, "ends_with\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `left()`
      if (str_detect(cell, "left(")) {
        match <- str_match(cell, "left\\(([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string <- match[2]
          num_character <- match[3]

          string <- remove_last_parenthesis(string)
          num_character <- remove_last_parenthesis(num_character)

          string <- add_quotes_if_no_brackets(string)

          r_equivalent <- paste0("substr(", string, ", 1, ", num_character, ")")

          cell <- str_replace(cell, "left\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `right()`
      if (str_detect(cell, "right(")) {
        match <- str_match(cell, "right\\(([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string <- match[2]
          num_character <- match[3]

          string <- remove_last_parenthesis(string)
          num_character <- remove_last_parenthesis(num_character)

          string <- add_quotes_if_no_brackets(string)

          r_equivalent <- paste0("substr(", string, ", nchar(", string, ") - ", num_character, " + 1, ", "nchar(", string, "))")

          cell <- str_replace(cell, "right\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `find()`
      if (str_detect(cell, "find(")) {
        match <- str_match(cell, "find\\(([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string1 <- match[2]
          string2 <- match[3]

          string1 <- remove_last_parenthesis(string1)
          string2 <- remove_last_parenthesis(string2)

          string1 <- add_quotes_if_no_brackets(string1)
          string2 <- add_quotes_if_no_brackets(string2)

          r_equivalent <- paste0("ifelse(regexpr(", string1, ", ", string2, ", ignore.case = TRUE) == -1, 0, regexpr(", string1, ", ", string2, ", ignore.case = TRUE))")

          cell <- str_replace(cell, "find\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `replace_text()`
      if (str_detect(cell, "replace_text(")) {
        match <- str_match(cell, "replace_text\\(([^,]+),\\s*([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string1 <- match[2]
          string2 <- match[3]
          replace <- match[4]

          string1 <- remove_last_parenthesis(string1)
          string2 <- remove_last_parenthesis(string2)
          replace <- remove_last_parenthesis(replace)

          string1 <- add_quotes_if_no_brackets(string1)
          string2 <- add_quotes_if_no_brackets(string2)
          replace <- add_quotes_if_no_brackets(replace)

          r_equivalent <- paste0("gsub(", string2, ", ", replace, ", ", string1, ")")

          cell <- str_replace(cell, "replace_text\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `mid()`
      if (str_detect(cell, "mid(")) {
        match <- str_match(cell, "mid\\(([^,]+),\\s*([^,]+),\\s*([^\\)]+)\\)")

        if (!is.na(match[1])) {
          string <- match[2]
          start_position <- match[3]
          n_character <- match[4]

          string <- remove_last_parenthesis(string)
          start_position <- remove_last_parenthesis(start_position)
          n_character <- remove_last_parenthesis(n_character)

          string <- add_quotes_if_no_brackets(string)

          r_equivalent <- paste0("substr(", string, ", ", start_position, ", ", start_position, " + ", n_character, " - 1", ")")

          cell <- str_replace(cell, "mid\\(.*?\\)", r_equivalent)
        }
      }


      # Handle `concat()`
      if (str_detect(cell, "concat(")) {
        cell <- gsub("concat ", "paste0", cell)
      }


      # Handle `concat_ws()`
      if (str_detect(cell, "concat_ws(")) {
        separator <- str_match(cell, 'concat_ws\\(["\'](.*?)["\']')[,2]
        elements <- str_match_all(cell, '["\'](.*?)["\']')[[1]][,2]
        elements <- elements[-1]

        separator <- add_quotes_if_no_brackets(separator)
        elements <- add_quotes_if_no_brackets(elements)

        r_equivalent <- paste0("paste(", paste(elements, collapse = ", "), ", sep = ", separator, ")")

        cell <- str_replace(cell, "concat_ws\\(.*?\\)", r_equivalent)
      }


      # Handle `upper()`
      if (str_detect(cell, "upper(")) {
        cell <- gsub("upper ", "toupper", cell)
      }


      # Handle `lower()`
      if (str_detect(cell, "lower(")) {
        cell <- gsub("lower ", "tolower", cell)
      }


      # Handle `trim()`
      if (str_detect(cell, "trim(")) {
        cell <- gsub("trim ", "trimws", cell)
      }


      # General transformations for branching logic
      if (str_detect(cell, "\\]\\[")) {
        events <- str_extract_all(cell, "(?<=\\[)[^\\]\\[]*(?=\\]\\[)")
        called_vars <- str_extract_all(cell, "(?<=\\]\\[).*?(?=\\])")

        cell <- str_replace_all(cell, "\\]\\[", "\\^")

        for (i in seq_along(events[[1]])) {
          event <- events[[1]][i]
          cell <- str_replace_all(cell, paste0("\\[", event), event)
        }

        for (i in seq_along(called_vars[[1]])) {
          called_var <- called_vars[[1]][i]
          cell <- str_replace_all(cell, paste0("\\[", called_var, "\\]"), paste0("\\[", called_var, "`"))
          cell <- str_replace_all(cell, paste0(called_var, "\\]"), called_var)
        }

        cell <- str_replace_all(cell, "\\[", "data$`")
        cell <- str_replace_all(cell, "\\]", "`[i]")
        cell <- str_replace_all(cell, "\\bor\\b|\\bOR\\b", "|")
        cell <- str_replace_all(cell, "\\band\\b|\\bAND\\b", "&")
        cell <- str_replace_all(cell, "=", "==")
        cell <- str_replace_all(cell, ">==", ">=")
        cell <- str_replace_all(cell, "<==", "<=")
        cell <- str_replace_all(cell, "<>", "!=")

        for (i in seq_along(events[[1]])) {
          event <- events[[1]][i]
          called_var <- called_vars[[1]][i]

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
        cell <- str_replace_all(cell, "\\[", "data$`")
        cell <- str_replace_all(cell, "\\]", "`[i]")
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

      cell <- paste0("unique(na.omit(", cell, "))")
      cell <- str_trim(cell)
      cell <- gsub("[\r\n]", "", cell)
      cell <- ifelse(cell == "unique(na.omit())", NA, cell)
    }
    return(cell)
  }

  data_dictionary[[column_name]] <- sapply(data_dictionary[[column_name]], process_cell)

  if (isblankormissingcode_detected && is.null(missing_data_codes)) {
    message("No missing_data_codes specified. Using default: ", paste(default_missing_data_codes, collapse = ", "))
  }

  return(data_dictionary)
}
