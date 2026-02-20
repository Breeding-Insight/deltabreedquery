#' Get Observation data
#'
#' @description Retrieves observation data from a DeltaBreed program via BrAPI.
#' @return Observation data from the BrAPI API
#' @export
#' @examples
#' \dontrun{
#' get_observations()
#' }
get_observations <- function() {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  # pull the observation units (field/plot layout) and the values (observations)
  cat('Requesting observation units...\n')
  json_obsunits <- execute_get_request(env$full_url, env$access_token,
                                       "observationunits", verbose = FALSE)
  cat('Requesting phenotype values...\n')
  json_obs <- execute_get_request(env$full_url, env$access_token,
                                  "observations", verbose = FALSE)

  # select and arrange the obs unit df to simplify col selection after merging
  df_obsunits <- dplyr::bind_rows(lapply(json_obsunits, clean_json_obsunits)) |>
    dplyr::select(ExptName, EnvName, Location,
                  ExpUnitID, Rep, Block, Row, Column, GermplasmName, GID, TestOrCheck,
                  observationUnitDbId) |>
    dplyr::arrange(ExptName, EnvName, ExpUnitID)

  df_obs <- dplyr::bind_rows(lapply(json_obs, clean_json_obs)) |>
    tidyr::pivot_wider(names_from = "observationVariableName",
                       values_from = "value")

  # merge together and summarize
  dplyr::left_join(df_obsunits, df_obs,
                   by = "observationUnitDbId") |>
  dplyr::select(!observationUnitDbId)

  #cat("Number of observations found:  ", nrow(df_obsunits), "\n")
  #cat("Number of environments found: ", nrow(df_obs), "\n")

}

clean_json_obsunits <- function(json) {
  data = json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  # block and rep are within a column which does not get fully flattened
  # verify that this column exists, then scrape the values
  if ('observationUnitPosition.observationLevelRelationships' %in% colnames(data)){
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
  # side note - Observations response contains year data
  # It's unwise to use this, since some Envs have no observations
  # better to pull it from Seasons
}
