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
             "\nSpecified BrAPI endpoint not found." ,
             " Please double-check that your BrAPI URL is correct.",
             " If this issue persists, please contact the package maintainers.")
        # temporary fix to get around DeltaBreed v1.3 bug
        # all requests with an empty response are returning 500 errors
        # should be returning empty responses with a 200 status
        # } else if (httr2::resp_status(resp) == 500){
        #   stop("Status code: 500",
        #        "\nInternal Server Error. Please record the details of this request ",
        #        "to the DeltaBreed team to figure out next steps.")
      }
      !(httr2::resp_status(resp) %in% c(200,500))
      # httr2::resp_status(resp) != 200
    })
  req
}

# perform a previously created request, including handling pagination
# returns list of json objects (which are also lists)
execute_get_request <- function(req, verbose = FALSE){
  response <- req |>
    httr2::req_url_query() |>
    httr2::req_perform()

  # temp fix to get around DeltaBreed 1.3 bug noted above
  if (response$status_code == 500){
    json <- list(result = list(data = data.frame()))
    return(json)
  }
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
    dplyr::rename(any_of(na.omit(mapping_vector)))
  missing_cols <- setdiff(names(mapping_vector),
                          colnames(renamed))
  for (col in missing_cols) {
    renamed[[col]] <- NA
  }
  renamed <- renamed |>
    dplyr::select(names(mapping_vector))
  renamed
}
