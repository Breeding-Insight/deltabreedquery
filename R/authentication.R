#' Log in to a DeltaBreed instance
#'
#' This function "logs in" to a DeltaBreed instance using the BrAPI Base
#'   URL and an Access Token, which are then stored. Both the URL and the token
#'   can be given as arguments or supplied to function prompts. The function
#'   performs some basic checks, including verifying that the user has internet
#'   access and making some test calls to the BrAPI server.
#'
#'   Access tokens are valid for 24 hours after generation. To check your
#'   authorization credentials at any time, use the
#'   check_auth() function.
#'
#' @return No return value, called for side effects (storing credentials)
#' @param base_url The BrAPI Base URL, found on the BrAPI tab of DeltaBreed
#'   in the BrAPI Information pane.
#' @param access_token A valid Access Token, retrieved from the DeltaBreed
#' user interface.
#' @export
#' @examples \dontrun{
#' login_deltabreed()
#' }
login_deltabreed <- function(base_url = NULL, access_token = NULL) {
  cat("=== DeltaBreed Login and Authentication ===\n")
  # Verify that the user has internet access
  if (!httr2::is_online()){
    stop("No internet connection detected. Please check your connection before \
         proceeding.")
  }
  # Prompt for Base URL if not supplied
  if (is.null(base_url)) {
    cat("Please enter the BrAPI Base URL.\n",
        "This can be found on the BrAPI tab of DeltaBreed,",
        "under the 'BrAPI Information' pane at left.\n")
    base_url <- readline(prompt = "BrAPI Base URL: ")
  }
  # Validate URL
  if (nchar(trimws(base_url)) == 0) {
    stop("BrAPI Base URL cannot be empty")
  }
  # Should be no trailing slash, but remove one just to be safe
  base_url <- sub("/$", "", trimws(base_url))
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
  cat("\nTesting authentication...\n")
  test_resp <- httr2::request(full_url) |>
    httr2::req_url_path_append('programs') |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_perform()

  if (httr2::resp_status(test_resp) == 200) {
    cat("URL and Access Token validated!\n")
    test_json <- test_resp |>
      httr2::resp_body_json(simplifyVector = TRUE,
                            flatten = TRUE)
    cat("Program name: ",
        test_json$result$data$programName, "\n")

    # if authentication is successful, store credentials in global environment
    # Initialize global environment if it doesn't exist
    if (!exists("deltabreedr_global", envir = .GlobalEnv)) {
      assign("deltabreedr_global", new.env(), envir = .GlobalEnv)
    }
    deltabreedr_global$base_url <- base_url
    deltabreedr_global$full_url <- full_url
    deltabreedr_global$access_token <- access_token
    deltabreedr_global$login_timestamp <- Sys.time()
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
#' will need to be-entered with `login_deltabreed()`.
#'
#' @return No return value, called for side effects (clearing credentials)
#' @export
logout_deltabreed <- function() {
  if (exists("deltabreedr_global", envir = .GlobalEnv)) {
    env <- get("deltabreedr_global", envir = .GlobalEnv)
    if (exists("base_url", envir = env)) rm("base_url", envir = env)
    if (exists("access_token", envir = env)) rm("access_token", envir = env)
  }

  cat("✓ Credentials cleared successfully.\n")
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
  if (!exists("deltabreedr_global", envir = .GlobalEnv)) {
    return(FALSE)
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)
  if (!exists("full_url", envir = env) ||
      !exists("access_token", envir = env)) {
    return(FALSE)
  }
  return(TRUE)
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
check_auth <- function() {
  if (!auth_exists()){
    cat("✗ You do not currently have any DeltaBreed authentication credentials",
        "stored.\n",
        "Please run login_deltabreed() to authenticate.\n")
  } else {
    cat("✓ You have DeltaBreed authentication credentials stored.\n")
    env <- get("deltabreedr_global", envir = .GlobalEnv)
    # Check for login timestamp and print remaining time (assuming 24h expiry)
    if (exists("login_timestamp", envir = env)) {
      login_time <- env$login_timestamp
      expiry_time <- login_time + 24 * 60 * 60
      now <- Sys.time()
      remaining <- as.numeric(difftime(expiry_time, now, units = "secs"))
      if (remaining > 0) {
        hours <- floor(remaining / 3600)
        minutes <- floor((remaining %% 3600) / 60)
        cat(sprintf("  Access token expires in %d hours %d minutes.\n", hours, minutes))
      } else {
        cat("✗ Access Token has expired.\n",
            "  Please run login_deltabreed() to re-authenticate.\n" )
      }
    }

    # Test authentication by making a call to endpoint
    test_resp <- httr2::request(env$full_url) |>
      httr2::req_url_path_append('programs') |>
      httr2::req_auth_bearer_token(env$access_token) |>
      httr2::req_perform()

    if (httr2::resp_status(test_resp) == 200) {
      cat("\n✓ URL and Access Token successfully validated!\n")
      test_json <- test_resp |>
        httr2::resp_body_json(simplifyVector = TRUE,
                              flatten = TRUE)
      cat("  Program name: ",
          test_json$result$data$programName, "\n")

    } else {
      cat("\n✗ The test call to the BrAPI server has failed.\n",
          "  Please run login_deltabreed() to re-authenticate.\n" )
    }
  }
  invisible(TRUE)
}
