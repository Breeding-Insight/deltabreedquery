#' Get all observation data
#'
#' @description Retrieves all observation data from a DeltaBreed program via BrAPI.
#' @return A data frame of observation units and observations.
#' @export
#' @examples
#' \dontrun{
#' get_observations()
#' }
get_observations <- function(page_size = 5000,
                             drop_empty_columns = FALSE) {
  if (!auth_exists()) {
    stop("No authentication credentials found.",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  cat("Requesting observation units...\n")
  obsunit_request <- build_get_request(env$full_url,
                                     env$access_token,
                                     "observationunits",
                                     page_size = page_size)
  json_obsunits <- execute_get_request(obsunit_request)


  cat("Requesting phenotype values...\n")
  obs_request <- build_get_request(env$full_url, env$access_token,
                                "observations")
  json_obs <- execute_get_request(obs_request)

  # Select the columns and arrange rows of obs units for readability
  df_obsunits <- dplyr::bind_rows(lapply(json_obsunits, clean_json_obsunits)) |>
    dplyr::select(ExptName,
                  EnvName,
                  Location,
                  ExpUnitID,
                  Rep,
                  Block,
                  Row,
                  Column,
                  GermplasmName,
                  GID,
                  TestOrCheck,
                  observationUnitDbId) |>
    dplyr::arrange(ExptName, EnvName, ExpUnitID)

  # observations are long format (np x 1), need to be wide (n x p)
  df_obs <- dplyr::bind_rows(lapply(json_obs, clean_json_obs)) |>
    tidyr::pivot_wider(names_from = "observationVariableName",
                       values_from = "value")

  # dbIds needed for merging, but drop after
  # only want human-readable info in the final data frame
  df_final <- dplyr::left_join(df_obsunits, df_obs,
                               by = "observationUnitDbId") |>
    dplyr::select(!observationUnitDbId)

  if (drop_empty_columns == TRUE){
    empty_cols <- apply(df_final, 2, function(x) all(is.na(x)))
    df_final <- df_final[,!empty_cols]
  }
  df_final
}

clean_json_obsunits <- function(json) {
  data = json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  # block and rep are within a column which does not get fully flattened
  # verify that this column exists, then scrape the values
  if ("observationUnitPosition.observationLevelRelationships" %in% colnames(data)){
    data$Rep = sapply(data$observationUnitPosition.observationLevelRelationships,
                      function(x) x |>
                        dplyr::filter(levelName == 'rep') |>
                        dplyr::pull(levelCode))
    data$Block = sapply(data$observationUnitPosition.observationLevelRelationships,
                        function(x) x |>
                          dplyr::filter(levelName == 'block') |>
                          dplyr::pull(levelCode))
  }
  rename_brapi_columns(data, 'observationunits')
}

clean_json_obs <- function(json) {
  data = json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  # there is extra information in the Observation response
  # but it is all redundant with data from ObsUnits
  # validating this is fairly costly from a time perspective
  # just pull the values and dbids as needed
  data |> dplyr::select(observationUnitDbId,
                        observationVariableName,
                        value)
  # side note - Observations response often contains year data
  # It's unwise to use this, since some Envs have no observations
  # better to pull this info from Seasons
}
