#' Build a generic BrAPI GET request object
#'
#' @description Builds a GET request for a specific BrAPI endpoint and adds more
#' specific error handling messages.
#'
#' @param url The DeltaBreed BrAPI URL (including /brapi/v2) to query.
#' @param token A valid Access Token for the instance.
#' @param endpoint The specific endpoint to query, e.g. germplasm or programinfo.
#' @param page_size Number of records to request per page. Default is 5000.
#' @return A httr2 request object.
#'
#' @details This function builds a GET request with the necessary headers
#' (which endpoint to query, page size, authentication token) needed to make a
#' valid request to a BrAPI endpoint. It adds DeltaBreed-specific error handling
#' for common HTTP status codes to help users troubleshoot in case.
#'
#' @noRd
build_get_request <- function(url, token, endpoint, page_size = 5000){
  req <- httr2::request(url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(pageSize = page_size) |>
    httr2::req_auth_bearer_token(token) |>
    # Custom error messages to help people troubleshoot
    httr2::req_error(is_error = function(resp) {
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
      } else if (httr2::resp_status(resp) == 500){
        stop("Status code: 500",
             "\nInternal Server Error. Please record the details of this request ",
             "to the DeltaBreed team to figure out next steps.")
      }
      httr2::resp_status(resp) != 200
    })
  req
}

#' Execute a GET request
#'
#' Executes a request constructed by build_get_request() and returns the a list
#' of responses in parsed JSON format.
#'
#' @param req A httr2_request object constructed by build_get_request().

#' @return A list containing the parsed JSON from the body of the requested
#' pages from the response.
#' @details This function executes a previously constructed GET request and
#' returns the parsed body of a given page from the response. Requests are
#' submitted individually for specific pages rather than using
#' httr2::req_perform_iterative(), since the raw JSON from the responses are
#' much larger than the parsed data frames of the same data and thus we only
#' want to keep a single page in memory at a time.
#'
#' @noRd
execute_get_request <- function(req, verbose = FALSE){
  response <- req |>
    httr2::req_url_query() |>
    httr2::req_perform()

  json <- response |>
    httr2::resp_body_json(simplifyVector = TRUE,
                          flatten = TRUE)

  # Use the pagination data from the response, not the request
  # Since not all endpoints have pageSize implemented
  n_records <- json$metadata$pagination$totalCount
  page_size_response <- json$metadata$pagination$pageSize
  n_pages_response <- json$metadata$pagination$totalPages

  if (n_records == 0) {
    stop("API call was successful but 0 records were found.")
  }
  if (verbose) cat("Number of records found: ", n_records, "\n")
  responses <- list(response)

  # iterate through pages if needed, starting at page 0
  # supply the stopping page ahead of time to avoid superfluous requests
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
