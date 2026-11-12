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
  if (resp$status_code == 500){
    return(data.frame())
  }

  # in future versions, we can merge this with the req_perform() step
  # only need to have it separate for now to accommodate the 500 error
  json_page0 <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE,
                          flatten = TRUE)

  # Use the pagination data from the response, not the request
  # Not all endpoints have pageSize implemented
  n_records <- json_page0$metadata$pagination$totalCount
  page_size_response <- json_page0$metadata$pagination$pageSize
  n_pages_response <- json_page0$metadata$pagination$totalPages

  if (json_page0$metadata$pagination$currentPage != 0) {
    stop("Error in pagination data. Page number of an initial server response ",
         "should be 0, but current page number is: ",
         json_page0$metadata$pagination$currentPage)
  }

  if (n_records == 0) {
    if (verbose) message("API call was successful but no records were found in the target endpoint.\n")
    return(data.frame())
  }
  if (verbose) message("Number of records found: ", n_records, "\n")

  # iterate through pages if needed, starting at page 0
  # supply the stopping page ahead of time to avoid superfluous requests
  if (n_pages_response > 1) {
    all_dfs <- list()
    all_dfs[[1]] <- json_to_data(json_page0)
    page_range <- 1:(n_pages_response-1)
    for (p in page_range){
      json <- req |>
        httr2::req_url_query(page = p) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE,
                              flatten = TRUE)
      all_dfs[[(p+1)]] <- json_to_data(json)
    }
    df_out <- dplyr::bind_rows(all_dfs)
    if (nrow(df_out) != n_records){
      warning("Number of records retrieved does not match BrAPI metadata.\n",
              "metadata$totalCount: ", n_records,
              "\nRecords retrieved: ", nrow(df_out))
    }
  } else {
    df_out <- json_to_data(json_page0)
  }
  df_out
}

# operates on a single page of JSON response
json_to_data <- function(json){
  data <- json$result$data
  # empty results in a valid 200 response are a list() of length 0
  if (length(data) == 0){
    return(data.frame())
  }
  data
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

# Load a bundled example JSON file from inst/ and return data frame
load_example_json <- function(filename) {
  path <- system.file(filename, package = "deltabreedquery")
  if (nchar(path) == 0) stop("Example data file not found: ", filename)
  json <- jsonlite::fromJSON(path, simplifyVector = TRUE, flatten = TRUE)
  df <- json_to_data(json)
  df
}
