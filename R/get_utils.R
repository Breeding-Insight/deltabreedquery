#' Build a generic BrAPI GET request
#'
#' Builds a GET requests to a specific BrAPI endpoint and adds custom error
#'   messaging for more helpful errors later on.
#'
#' @param url The DeltaBreed BrAPI URL (including /brapi/v2) to query.
#' @param token A valid Access Token for the instance.
#' @param endpoint The specific endpoint to query, e.g. germplasm or programinfo
#' @param page_size Number of records to request per page. Default is 1000.
#' @return A httr2 request object.
#' @details This function handles pagination automatically by making additional
#' requests if the total number of records exceeds the page size. It includes
#' custom error handling for common HTTP status codes (401, 404, 405) with
#' helpful error messages for troubleshooting authentication and endpoint issues.
#'
#' @noRd
build_get_request <- function(url, token, endpoint, page_size = 5000){
  req <- httr2::request(url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(pageSize = page_size) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_error(is_error = function(resp) {
      # Custom error messages to help people troubleshoot
      if (httr2::resp_status(resp) == 401) {
        stop("Status code: ", httr2::resp_status(resp),
             "\nAccess Token rejected by BrAPI endpoint. ",
             " Please double-check that your BrAPI URL is correct",
             " and try regenerating the Access Token.")
      } else if (httr2::resp_status(resp) %in% c(404, 405)) {
        stop("Status code: ", httr2::resp_status(resp),
             "\nSpecified BrAPI endpoint not found." ,
             " Please double-check that your BrAPI URL is correct.",
             " If this issue persists, please contact the package maintainers.")
      }
      httr2::resp_status(resp) != 200
    })
  req
}

#' Execute a generic BrAPI GET request
#'
#' Makes authenticated GET request to a BrAPI endpoint and handles result
#' pagination automatically. Returns parsed JSON data from all pages.
#'
#' @param url The DeltaBreed BrAPI URL (including /brapi/v2) to query.
#' @param token A valid Access Token for the instance.
#' @param endpoint The specific endpoint to query, e.g. germplasm or programinfo
#' @param page_size Number of records to request per page. Default is 1000.

#' @return A list of parsed JSON responses from all pages.
#' @details This function handles pagination automatically by making additional
#' requests if the total number of records exceeds the page size. It includes
#' custom error handling for common HTTP status codes (401, 404, 405) with
#' helpful error messages for troubleshooting authentication and endpoint issues.
#'
#' @noRd
execute_get_request <- function(url, token, endpoint,
                                page_size = 5000, verbose = TRUE){
  req <- build_get_request(url, token, endpoint, page_size)
  response <- httr2::req_perform(req)
  json <- response |>
    httr2::resp_body_json(simplifyVector = TRUE,
                   flatten = TRUE)

  # NOTE - not all DeltaBreed endpoints have pagination enabled
  # e.g. on /brapi/v2/observationunits, adjusting pageSize does nothing
  # To handle this, we use the pagination data from the response, not the request
  n_records <- json$metadata$pagination$totalCount
  page_size_response <- json$metadata$pagination$pageSize
  n_pages_response <- json$metadata$pagination$totalPages

  if (n_records == 0) {
    stop("API call was successful but 0 records were found.")
  }
  # iterate through pages if necessary - a little clunky here
  # there might be a better way to iterate through n+1 remaining pages
  if (verbose) cat("Number of records found: ", n_records, "\n")
  responses <- list(response)
  # NOTE - iterate_with_offset needs to start at 0
  # it doesn't know when to stop, though - supply this based on known page count
  if (n_pages_response > 1) {
    responses <- httr2::req_perform_iterative(req,
                                               httr2::iterate_with_offset("page",
                                                                          start = 0),
                                               max_reqs = n_pages_response)
  }
  json <- lapply(responses, function(x) httr2::resp_body_json(x,
                                                       simplifyVector = TRUE,
                                                       flatten = TRUE))
  json
}
