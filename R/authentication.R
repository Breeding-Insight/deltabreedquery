# Private package-level environment for storing session credentials
.dbc_env <- new.env(parent = emptyenv())

#' Log in to a DeltaBreed instance
#'
#' This function stores your authentication credentials for a target DeltaBreed
#' instance. To log in, you will require the BrAPI Base URL and a valid
#' authentication token, both of which can be found on the 'BrAPI' tab of
#' DeltaBreed. The URL and token can be given as arguments or supplied to
#' function prompts. The function performs some basic checks, including
#' verifying that the user has internet access and making some test calls to the
#' BrAPI server.
#'
#' Access tokens are valid for 24 hours after generation. To check your
#' authorization credentials at any time, use the check_auth() function.
#'
#' @return No return value, called for side effects (storing credentials)
#' @param base_url The BrAPI Base URL, found on the BrAPI tab of DeltaBreed in
#'   the BrAPI Information pane.
#' @param access_token A valid Access Token, retrieved from the DeltaBreed user
#'   interface.
#' @param verbose Whether to print out success messages.
#' @export
#' @examples \dontrun{
#' # function can be run with no arguments to bring up a prompt to enter the URL/token
#' login_deltabreed()
#'
#' # since your program's URL will remain static, you can supply it as an argument, e.g.:
#' login_deltabreed("https://app.breedinginsight.net/v1/programs/f152169d-049f-4a7c-b5d8-c725b14e66f0")
#' }
login_deltabreed <- function(base_url = NULL, access_token = NULL, verbose = TRUE) {
  if (verbose) message("=== DeltaBreed Login and Authentication ===")
  # Prompt for Base URL if not supplied
  if (is.null(base_url)) {
    cat("Please enter the BrAPI Base URL.\n",
        "This can be found on the BrAPI tab of DeltaBreed,",
        "under the 'BrAPI Information' pane at left.\n", sep = "")
    base_url <- readline(prompt = "BrAPI Base URL: ")
  }
  # Validate URL
  if (nchar(trimws(base_url)) == 0) {
    stop("BrAPI Base URL cannot be empty")
  }
  base_url <- trimws(base_url)

  # Example mode: bypass internet check and API calls entirely
  if (tolower(base_url) == "example") {
    .dbc_env$base_url        <- "example"
    .dbc_env$full_url        <- "example"
    .dbc_env$access_token    <- "example_token"
    .dbc_env$login_timestamp <- Sys.time()
    if (verbose) message("Example mode enabled. Using bundled sample data.")
    return(invisible(TRUE))
  }

  # Verify that the user has internet access
  if (!httr2::is_online()){
    stop("No internet connection detected. Please check your connection before \
         proceeding.")
  }

  # Should be no trailing slash, but remove one just to be safe
  base_url <- sub("/$", "", base_url)
  # Build a full URL for simpler request building
  full_url <- paste0(base_url, '/brapi/v2')

  # Prompt for Access Token if needed
  if (is.null(access_token)){
    cat("\nPlease generate an Access Token from the BrAPI tab of DeltaBreed.\n")
    access_token <- readline(prompt = "Access Token: ")
  }
  # Validate token input
  if (nchar(trimws(access_token)) == 0) {
    stop("Access Token cannot be empty")
  }

  # Test authentication by making a test API call
  if (verbose) message("Testing authentication...")
  test_resp <- httr2::request(full_url) |>
    httr2::req_url_path_append('programs') |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_perform()

  if (httr2::resp_status(test_resp) == 200) {
    test_json <- test_resp |>
      httr2::resp_body_json(simplifyVector = TRUE,
                            flatten = TRUE)
    if (verbose) {
      message("URL and Access Token validated!")
      message("Program name: ",
              test_json$result$data$programName)
    }

    # if authentication is successful, store credentials in package environment
    .dbc_env$base_url        <- base_url
    .dbc_env$full_url        <- full_url
    .dbc_env$access_token    <- access_token
    .dbc_env$login_timestamp <- Sys.time()
  } else if (httr2::resp_status(test_resp) == 401) {
    stop("401: Access Token not accepted. ",
         "Please double-check the BrAPI Base URL and ",
         "try generating a new Access Token.")
  } else if (httr2::resp_status(test_resp) == 404) {
    stop("404: BrAPI endpoint not found. ",
         "Please double-check the BrAPI Base URL.")
  } else {
    stop("Unexpected error during authentication test. Status code: ",
         httr2::resp_status(test_resp))
  }
  invisible(TRUE)
}

#' Clear DeltaBreed authentication credentials
#'
#' @description Removes stored credentials (URL and access token) from the
#' package environment. The access token will remain valid for as long as the
#' DeltaBreed instance specifies, but in order to retrieve data the URL/token
#' will need to be-entered with login_deltabreed().
#'
#' @return No return value, called for side effects (clearing credentials)
#' @export
logout_deltabreed <- function() {
  if (exists("base_url", envir = .dbc_env))     rm("base_url",        envir = .dbc_env)
  if (exists("full_url", envir = .dbc_env))     rm("full_url",        envir = .dbc_env)
  if (exists("access_token", envir = .dbc_env)) rm("access_token",    envir = .dbc_env)
  if (exists("login_timestamp", envir = .dbc_env)) rm("login_timestamp", envir = .dbc_env)

  message("\u2713 Credentials cleared successfully.")
  invisible(TRUE)
}

#' @title Check whether authentication credentials exist
#'
#' @description Checks if a BrAPI Base URL and access token exist in the global
#'   environment. Note that it does not specify whether the credentials are
#'   valid, merely that they exist.
#'
#' @return Logical value indicating if base_url and access_token exist in the
#'   global env.
auth_exists <- function() {
  exists("full_url", envir = .dbc_env) &&
    exists("access_token", envir = .dbc_env)
}

# Helper function for checking example mode, cleaner than referring constantly
# to .dbc_env$base_url
is_example_mode <- function() {
  isTRUE(exists("base_url", envir = .dbc_env) &&
           .dbc_env$base_url == "example")
}

#' Validate BrAPI authentication credentials
#'
#' @description Checks if the user has credentials currently stored and
#' validates them by performing a test call to the BrAPI endpoint.
#' Also prints the remaining time until the access token expires, if
#' applicable.
#'
#' @return No return value, called for side effects (printing status)
#' @export
#' @examples
#' check_auth()
#' login_deltabreed("example", verbose = FALSE)
#' check_auth()
check_auth <- function() {
  if (!auth_exists()){
    message("\u2718 You do not currently have any DeltaBreed authentication credentials stored.\n",
            "Please run login_deltabreed() to authenticate.")
  } else if (is_example_mode()) {
    message("\u2713 Running in example mode with bundled sample data.")
    message("No authentication credentials required.")
  } else {
    message("\u2713 You have DeltaBreed authentication credentials stored.")
    # Check for login timestamp and print remaining time (assuming 24h expiry)
    if (exists("login_timestamp", envir = .dbc_env)) {
      login_time <- .dbc_env$login_timestamp
      expiry_time <- login_time + 24 * 60 * 60
      now <- Sys.time()
      remaining <- as.numeric(difftime(expiry_time, now, units = "secs"))
      if (remaining > 0) {
        hours <- floor(remaining / 3600)
        minutes <- floor((remaining %% 3600) / 60)
        message(sprintf("Access token expires in %d hours %d minutes.", hours, minutes))
      } else {
        message("\u2718 Access Token has expired.\n",
                "Please run login_deltabreed() to re-authenticate.")
      }
    }

    # Test authentication by making a call to endpoint
    test_resp <- httr2::request(.dbc_env$full_url) |>
      httr2::req_url_path_append('programs') |>
      httr2::req_auth_bearer_token(.dbc_env$access_token) |>
      httr2::req_perform()

    if (httr2::resp_status(test_resp) == 200) {
      message("\u2713 URL and Access Token successfully validated!")
      test_json <- test_resp |>
        httr2::resp_body_json(simplifyVector = TRUE,
                              flatten = TRUE)
      message("Program name: ",
              test_json$result$data$programName)

    } else {
      message("\u2718 The test call to the BrAPI server has failed.\n",
              "Please run login_deltabreed() to re-authenticate.")
    }
  }
  invisible(TRUE)
}
