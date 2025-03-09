#' Title
#'
#' @param url REDCap API url
#' @param token REDCap API token
#' @param content_type The data type you want to import, "record" (data) or "metadata" (data dictionary); defaults to both
#' @param format The format in which the data and/or metadata are imported ("csv", "json" or "xml"); defaults to "csv"
#'
#' @returns The imported data and/or data dictionary
#' @export
#'
#' @examples REDCap_import(url = "https://your-redcap-api-url", token = "your_api_token")
#' @examples REDCap_import(url = "https://your-redcap-api-url", token = "your_api_token", content_type = "record", format = "json")

REDCap_import <- function(url, token, content_type = NULL, format = "csv") {
  # ---- Token Validation ----
  if (missing(token) || !is.character(token)) {
    stop("Error: API token is missing or not a valid character string.")
  }

  # Remove accidental spaces or invisible characters
  token <- trimws(token)

  # Check length first
  if (nchar(token) != 32) {
    stop("Error: Token must be exactly 32 characters long.")
  }

  # Ensure token contains only valid hexadecimal characters (0-9, a-f, A-F)
  if (!grepl("^[0-9A-Fa-f]{32}$", token)) {
    stop("Error: Token contains invalid characters. It should only contain hexadecimal characters (0-9, A-F).")
  }

  cat("Token validation passed.\n")

  # ---- Format Validation ----
  if (!format %in% c("csv", "json", "xml")) {
    stop("Invalid format. Choose 'csv', 'json', or 'xml'.")
  }

  # ---- Prepare the list to store the results ----
  results <- list()

  # ---- Import Data (Records) if content_type is NULL or 'record' ----
  if (is.null(content_type) || content_type == "record") {
    cat("Importing records (data)...\n")
    body <- list(
      token = token,
      content = "record",  # Request data (records)
      format = format,
      returnFormat = "json"  # Return in JSON format
    )

    response <- POST(url, body = body, encode = "form")

    # Check for API Errors
    if (http_status(response)$category != "Success") {
      stop("API request failed: ", http_status(response)$message)
    }

    # Process data response
    if (format == "csv") {
      data <- read.csv(text = content(response, "text"), stringsAsFactors = FALSE)
    } else if (format == "json") {
      data <- fromJSON(content(response, "text"))
    }

    results$data <- data  # Store the data in the list
  }

  # ---- Import Metadata if content_type is NULL or 'metadata' ----
  if (is.null(content_type) || content_type == "metadata") {
    cat("Importing metadata (data dictionary)...\n")
    body <- list(
      token = token,
      content = "metadata",  # Request metadata (data dictionary)
      format = format,
      returnFormat = "json"  # Return in JSON format
    )

    response <- POST(url, body = body, encode = "form")

    # Check for API Errors
    if (http_status(response)$category != "Success") {
      stop("API request failed: ", http_status(response)$message)
    }

    # Process metadata response
    if (format == "csv") {
      metadata <- read.csv(text = content(response, "text"), stringsAsFactors = FALSE)
    } else if (format == "json") {
      metadata <- fromJSON(content(response, "text"))
    }

    results$metadata <- metadata  # Store the metadata in the list
  }

  return(results)  # Return the list with both data and metadata
}
