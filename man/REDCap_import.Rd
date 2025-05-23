% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/REDCap_import.R
\name{REDCap_import}
\alias{REDCap_import}
\title{Import REDCap data and metadata}
\usage{
REDCap_import(url, token, content_type = NULL, format = "csv")
}
\arguments{
\item{url}{REDCap API url}

\item{token}{REDCap API token}

\item{content_type}{The data type you want to import, "record" (data), "metadata" (data dictionary), event (event description) or mapping (instrument-event mapping); defaults to all}

\item{format}{The format in which the data and/or metadata are imported ("csv", "json" or "xml"); defaults to "csv"}
}
\value{
The imported data and/or selected metadata
}
\description{
Allow users to import REDCap data and metadata with an API.
}
\examples{
REDCap_import(url = "https://your-redcap-api-url", token = "your_api_token")
REDCap_import(url = "https://your-redcap-api-url", token = "your_api_token", content_type = "record", format = "json")
}
