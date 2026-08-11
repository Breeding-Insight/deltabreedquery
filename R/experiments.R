#' Retrieve experiment summary
#'
#' Retrieves a summary of all experiments and environments in a given DeltaBreed
#' program. This may include experiments for which no observations have been
#' recorded yet.
#'
#' @param verbose Whether to print out the number of experiments/environments found.
#' @param include_dbids Whether to include the lengthy unique ID for each experiment/environment.
#' Typically used for debugging or merging data from other sources.
#'
#' @return Data frame of experiment/environment metadata.
#' @export
#' @examples
#' login_deltabreed("example", verbose = FALSE)
#'
#' expt <- get_experiments()
#' expt
#' @importFrom rlang .data
get_experiments <- function(verbose = TRUE,
                            include_dbids = FALSE) {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  # Need to pull from trials, studies, and seasons endpoints
  if (is_example_mode()) {
    df_trials <- load_example_json("trials.json") |> json_list_to_df()
    df_studies <- load_example_json("studies.json") |> json_list_to_df()
    df_seasons <- load_example_json("seasons.json") |> json_list_to_df()
  } else {
    df_trials <- build_get_request(.dbc_env$full_url,
                                   .dbc_env$access_token,
                                   'trials') |>
      execute_get_request() |>
      json_list_to_df()

    df_studies <- build_get_request(.dbc_env$full_url,
                                    .dbc_env$access_token,
                                    'studies') |>
      execute_get_request() |>
      json_list_to_df()

    df_seasons <- build_get_request(.dbc_env$full_url,
                                    .dbc_env$access_token,
                                    'seasons') |>
      execute_get_request() |>
      json_list_to_df()
  }

  if (nrow(df_trials) == 0){
    return(df_trials)
  }

  df_trials <- df_trials |>
    dplyr::select("trialName",
                  "additionalInfo.experimentType",
                  "additionalInfo.createdBy.userName",
                  "additionalInfo.createdDate",
                  "trialDbId")

  df_studies <- df_studies |>
    dplyr::mutate("seasons" = unlist(.data$seasons)) |>
    dplyr::select("studyName",
                  "locationName",
                  "studyDbId",
                  "trialDbId",
                  "seasons")

  df_seasons <- df_seasons |>
    dplyr::select("seasonDbId",
                  "year")

  if (verbose == TRUE){
    message("Number of Experiments found:  ", nrow(df_trials))
    message("Number of Environments found: ", nrow(df_studies))
  }

  # merge in just the seasonDbId and year(s)
  # will need to revisit this if we ever get multi-year environments
  df_studies <- dplyr::left_join(df_studies,
                                 df_seasons,
                                 by = dplyr::join_by("seasons" == "seasonDbId")) |>
    dplyr::select(!c("seasons"))

  df_expts <- dplyr::full_join(df_trials,
                               df_studies,
                               by = "trialDbId")

  mapping_expt <- define_mapping_expts()
  if (include_dbids == TRUE){
    mapping_expt <- c(mapping_expt,
                      c("studyDbId" = "studyDbId",
                        "trialDbId" = "trialDbId"))
  }
  df_expts <- brapi_to_db_names(df_expts, mapping_expt) |>
    dplyr::arrange(.data$Year,
                   .data$ExpName,
                   .data$EnvName)
  df_expts
}

define_mapping_expts <- function() {
  mapping <- c(
    "ExpName" = "trialName",
    "ExpType" = "additionalInfo.experimentType",
    "EnvName" = "studyName",
    "Location" = "locationName",
    "Year" = "year",
    "CreatedBy" = "additionalInfo.createdBy.userName",
    "CreatedDate" = "additionalInfo.createdDate"
  )
  mapping
}


