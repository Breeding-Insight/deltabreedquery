

build_get_request <- function(url, token, endpoint, page_size = 5000){
  req <- httr2::request(url) |>
    httr2::req_url_path_append(endpoint) |>
    # define page_size in the initial baseline request
    # don't want multiple requests in a series to be able to have different page sizes
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
      }
      httr2::resp_status(resp) != 200
    })
  req
}
