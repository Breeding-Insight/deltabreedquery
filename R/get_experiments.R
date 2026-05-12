#' Retrieve experiment summary
#'
#' Retrieves a summary of all experiments and environments in a given DeltaBreed
#' program. This may include experiments for which no observations have been
#' recorded yet.
#'
#' @param verbose Whether to print
#' @param summarize Whether to include summaries. Setting
#'
#' @return Data frame of experiment/environment metadata.
#' @export
#' @examples
#' \dontrun{
#' login_deltabreed()
#' get_experiments()
#' }
get_experiments <- function(verbose = TRUE, summarize = TRUE) {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  # Need to pull from trials, studies, and seasons endpoints
  json_trials <- build_get_request(env$full_url,
                                   env$access_token,
                                   'trials') |>
    execute_get_request(verbose = FALSE)

  json_studies <- build_get_request(env$full_url,
                                    env$access_token,
                                    'studies') |>
    execute_get_request(verbose = FALSE)

  json_seasons <- build_get_request(env$full_url,
                                    env$access_token,
                                    'seasons') |>
    execute_get_request(verbose = FALSE)

  df_trials <- dplyr::bind_rows(lapply(json_trials, clean_json_trials))
  df_studies <- dplyr::bind_rows(lapply(json_studies, clean_json_studies))
  df_seasons <- dplyr::bind_rows(lapply(json_seasons, clean_json_seasons))

  cat("Number of Experiments found:  ", nrow(df_trials), "\n")
  cat("Number of Environments found: ", nrow(df_studies), "\n")

  # Join the seasons df into studies
  df_studies <- dplyr::left_join(df_studies,
                                 df_seasons,
                                 by = dplyr::join_by(seasons == seasonDbId)) |>
    dplyr::select(!seasons)

  df_expts <- dplyr::full_join(df_trials,
                               df_studies,
                               by = "trialDbId") |>
    dplyr::select(ExptName,
                  ExptType,
                  EnvName,
                  Location,
                  Year,
                  ObservationLevel,
                  CreatedBy,
                  CreatedDate) |>
    dplyr::arrange(Year,
                   ExptName,
                   EnvName)

  # TODO - when they implement multiple seasons (for long lived perennials)
  # revisit this to add support for multi-year cycles
  df_expts
}

# Trials are the LARGER entity - "Experiment" in DeltaBreed nomenclature
clean_json_trials <- function(json) {
  data = json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  data <- rename_brapi_columns(data, 'trials') |>
    dplyr::select(ExptName,
                  ExptType,
                  ObservationLevel,
                  CreatedBy,
                  CreatedDate,
                  trialDbId)
  data
}

# Studies are the SMALLER entity - "Environment" in DeltaBreed nomenclature
# Until multi-season environments are implemented, delist() should work
clean_json_studies <- function(json) {
  data = json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  data <- rename_brapi_columns(data, 'studies') |>
    dplyr::mutate(seasons = unlist(seasons)) |>
    dplyr::select(EnvName,
                  Location,
                  Active,
                  studyDbId,
                  trialDbId,
                  seasons)
  data
}

# Seasons endpoint is used to get year(s) of the trials
# currently this supports an environment being a single year
# not sure if we will need support for multiple years in the future
clean_json_seasons <- function(json) {
  data = json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  data <- data |>
    dplyr::select(seasonDbId,
                  year) |>
    dplyr::rename(Year = year)
  data
}
