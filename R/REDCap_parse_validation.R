#' Validate REDcap to R translation
#'
#' @description This function allows you to validate the output of REDCap_logic_parser in the context of you REDCap data
#'
#' @param data REDCap data on which you want to validate the parsing
#' @param logic Dataset or string containing the R logic
#' @param column_name Name of the column containing the logic if "logic" is a dataset, else, leave blank
#'
#' @returns
#' @export
#'
#' @examples

REDCap_parse_validation <- function(data, logic, column_name) {
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
          message("Invalid logic expression in row ", row_index)
          FALSE
        }

      }, error = function(e) {
        message("Error in logic (row ", row_index, "): ", cell, " - ", e$message)
        FALSE  # If an error occurs, return FALSE
      })

      return(result)
    }
    return(TRUE)  # Return TRUE if the cell is NA (not a problem)
  }

  # Iterate over the rows in the data and apply the validation
  validation_results <- sapply(1:nrow(logic), function(i) {
    validate_logic(logic[[column_name]][i], i)
  })

  return(validation_results)
}
