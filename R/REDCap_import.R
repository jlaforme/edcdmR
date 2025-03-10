#' Import REDCap data and metadata
#'
#' @param url REDCap API url
#' @param token REDCap API token
#' @param content_type The data type you want to import, "record" (data), "metadata" (data dictionary), event (event description) or mapping (instrument-event mapping); defaults to all
#' @param format The format in which the data and/or metadata are imported ("csv", "json" or "xml"); defaults to "csv"
#'
#' @returns The imported data and/or selected metadata
#' @export
#'
#' @examples REDCap_import(url = "https://your-redcap-api-url", token = "your_api_token")
#' @examples REDCap_import(url = "https://your-redcap-api-url", token = "your_api_token", content_type = "record", format = "json")

REDCap_import <- function(url, token, content_type = NULL, format = "csv") {
  library(httr)
  library(jsonlite)

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

  # ---- Content Type Validation ----
  if (is.null(content_type)) {
    content_type <- c("record", "metadata", "event", "mapping")  # Default to all content types if empty
  }

  valid_content_types <- c("record", "metadata", "event", "mapping")

  # Ensure all content types are valid
  if (!all(content_type %in% valid_content_types)) {
    stop("Error: content_type must include only 'record', 'metadata', or 'event'.")
  }

  # ---- Import Data (Records) if content_type includes 'record' ----
  if ("record" %in% content_type) {
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

  # ---- Import Metadata if content_type includes 'metadata' ----
  if ("metadata" %in% content_type) {
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

  # ---- Import Event Data (event description) if content_type includes 'event' ----
  if ("event" %in% content_type) {
    cat("Importing event data (event description)...\n")
    body <- list(
      token = token,
      content = "event",  # Request event names and form associations
      format = format,
      returnFormat = "json"  # Return in JSON format
    )

    response <- POST(url, body = body, encode = "form")

    # Check for API Errors
    if (http_status(response)$category != "Success") {
      stop("API request failed: ", http_status(response)$message)
    }

    # Process event data response
    if (format == "csv") {
      event_data <- read.csv(text = content(response, "text"), stringsAsFactors = FALSE)
    } else if (format == "json") {
      event_data <- fromJSON(content(response, "text"))
    }

    results$event_data <- event_data  # Store the event data in the list
  }

  # ---- Import Mapping Data (event-form associations/mapping) if content_type includes 'mapping' ----
  if ("mapping" %in% content_type) {
    cat("Importing mapping data (event form mapping)...\n")
    body <- list(
      token = token,
      content = "formEventMapping",  # Request event names and form associations
      format = format,
      returnFormat = "json"  # Return in JSON format
    )

    response <- POST(url, body = body, encode = "form")

    # Check for API Errors
    if (http_status(response)$category != "Success") {
      stop("API request failed: ", http_status(response)$message)
    }

    # Process event data response
    if (format == "csv") {
      mapping_data <- read.csv(text = content(response, "text"), stringsAsFactors = FALSE)
    } else if (format == "json") {
      mapping_data <- fromJSON(content(response, "text"))
    }

    results$mapping_data <- mapping_data  # Store the event data in the list
  }


  # ---- Return a single data frame if only one content_type is specified ----
  if (length(results) == 1) {
    return(results[[1]])  # Return the single data frame
  }

  # ---- Otherwise return the list of results ----
  return(results)  # Return the list with data, metadata, and event data
}
