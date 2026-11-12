# reusable wrapper for more informative error code handling
# returns a httr2 request object
build_get_request <- function(url, token, endpoint, page_size = 10000){
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
             "\nSpecified BrAPI endpoint not found:" , endpoint, "\n",
             " Please double-check that your BrAPI URL is correct.",
             " If this issue persists, please contact the package maintainers.")
        # } else if (httr2::resp_status(resp) == 500){
        # as of Nov 2026, the 500 error on empty responses is still not fixed
        # leave in this workaround to allow 500 errors to look like 200s with an empty response
        #   stop("Status code: 500\nInternal Server Error.\n",
        #        "Please record the URL of the DeltaBreed instance ",
        #        " you are trying to reach and the function you are trying to execute ",
        #        " and send these details to the DeltaBreed team to figure out next steps.")
      }
      # httr2::resp_status(resp) != 200
      !(httr2::resp_status(resp) %in% c(200,500))
    })
  req
}

# perform a previously created request, including handling pagination
# returns list of json objects (which are also lists)
execute_get_request <- function(req, verbose = FALSE){
  resp <- req |>
    httr2::req_url_query() |>
    httr2::req_perform()

  # temp fix to get around the bug noted above
  # allow all 500 errors to look like 200 errors with empty responses
  if (response$status_code == 500){
    json_list <- list(result = list(data = data.frame()))
    return(json_list)
  }

  json_page0 <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE,
                          flatten = TRUE)
  # Use the pagination data from the response, not the request
  # Not all endpoints have pageSize implemented
  n_records <- json_page0$metadata$pagination$totalCount
  page_size_response <- json_page0$metadata$pagination$pageSize
  n_pages_response <- json_page0$metadata$pagination$totalPages

  # need to allow for a 500 error on empty response, as noted above

  if (json_page0$metadata$pagination$currentPage != 0) {
    stop("Error in pagination data. Page number of an initial server response ",
         "should be 0, but current page number is: ",
         json_page0$metadata$pagination$currentPage)
  }

  if (n_records == 0) {
    if (verbose) message("API call was successful but no records were found in the target endpoint.\n")
    json_list <- list(result = list(data = data.frame()))
    return(json_list)
  }

  if (verbose) message("Number of records found: ", n_records, "\n")
  responses <- list(response)

  # iterate through pages if needed, starting at page 0
  # supply the stopping page ahead of time to avoid superfluous requests
  if (n_pages_response > 1) {
    responses <- httr2::req_perform_iterative(req,
                                              httr2::iterate_with_offset("page",
                                                                         start = 0),
                                              max_reqs = n_pages_response)
  }
  json_list <- lapply(responses, function(x) httr2::resp_body_json(x,
                                                                   simplifyVector = TRUE,
                                                                   flatten = TRUE))
  json_list
}

# operates on a single page of JSON response
json_to_data <- function(json){
  data <- json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  data
}

# vectorized, produces a data frame of the response data
# bind_rows is outer join, will include all columns from all data dfs
json_list_to_df <- function(json_list){
  df_list <- lapply(json_list, json_to_data)
  df <- dplyr::bind_rows(df_list)
  df
}

# convert column names in a data frame from BrAPI convention to DeltaBreed names
# mapping vector comes from define_mapping_xyz() function in each get_xyz.R file
# also handles the column ordering
brapi_to_db_names <- function(data, mapping_vector){
  # mapping_vector has DeltaBreed terms as names, BrAPI terms as values
  # NA values for terms that must be in the final df but do not have a 1:1 mapping to a BrAPI field
  # those need to be handled elsewhere
  renamed <- data |>
    dplyr::rename(dplyr::any_of(stats::na.omit(mapping_vector)))
  missing_cols <- setdiff(names(mapping_vector),
                          colnames(renamed))
  for (col in missing_cols) {
    renamed[[col]] <- NA
  }
  renamed <- renamed |>
    dplyr::select(names(mapping_vector))
  renamed
}

# Load a bundled example JSON file from inst/ and return in json_list format
load_example_json <- function(filename) {
  path <- system.file(filename, package = "deltabreedquery")
  if (nchar(path) == 0) stop("Example data file not found: ", filename)
  json <- jsonlite::fromJSON(path, simplifyVector = TRUE, flatten = TRUE)
  list(json)
}
