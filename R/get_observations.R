# lookup function to hard-code the name conversions (if required)
obs_lookup <- function(){
  0
}

#' Retrieve all observation data
#'
#' Retrieves all observation (and sub-observation) data from a DeltaBreed
#' program via BrAPI calls, converting it into a data frame that resembles
#' how the data appears on DeltaBreed itself.
#'
#' @param page_size Page size to use when making the request. Larger page size can make requests faster but may cause errors.
#' @param verbose Whether to print short messages showing the number of records found
#' @param drop_empty_columns Whether to drop columns that contain no data across the entire data frame.
#' @param include_dbids Whether to include the DbIds of the observation units, mostly useful for debugging.
#'
#' @return A data frame of observations (phenotypes), formatted in DeltaBreed style.
#' @export
#' @examples
#' \dontrun{
#' login_deltabreed()
#' obs <- get_observations()
#' }
get_observations <- function(page_size = 5000,
                             drop_empty_columns = FALSE,
                             include_dbids = FALSE,
                             verbose = TRUE) {
  if (!auth_exists()) {
    stop("No authentication credentials found.",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  if (verbose) cat("Requesting observation units...\n")
  obsunit_request <- build_get_request(env$full_url,
                                       env$access_token,
                                       "observationunits",
                                       page_size = page_size)
  json_obsunits <- execute_get_request(obsunit_request)

  if (verbose) cat("Requesting phenotype values...\n")
  obs_request <- build_get_request(env$full_url,
                                   env$access_token,
                                   "observations",
                                   page_size = page_size)
  json_obs <- execute_get_request(obs_request)

  # Select using the final desired column ordering
  df_obsunits <- dplyr::bind_rows(lapply(json_obsunits, clean_json_obsunits)) |>
    dplyr::select(ExpName,
                  EnvName,
                  Location,
                  ExpUnit,
                  ExpUnitID,
                  SubObsUnit,
                  SubUnitID,
                  Rep,
                  Block,
                  Row,
                  Column,
                  GermplasmName,
                  GID,
                  TestOrCheck,
                  observationUnitDbId,
                  ParentObsUnitDbId) |>
    dplyr::arrange(ExpName, !is.na(SubUnitID), EnvName)

  # slightly complex sorting here so we can handle integer ExpUnitIDs and/or SubUnitIDs
  # for IDs like 1,2,[...],10,11,12, we want to sort that as an integer
  # columns may contain strings as well - decide env-by-env
  df_obsunits <- df_obsunits |>
    dplyr::group_by(ExpName, !is.na(SubUnitID), EnvName) |>
    dplyr::mutate(expids_all_integers = all(!grepl("\\D", ExpUnitID)),
                  subids_all_integers = all(!grepl("\\D", SubUnitID))) |>
    dplyr::mutate(new_order_obs = ifelse(expids_all_integers,
                                         rank(as.integer(ExpUnitID)),
                                         rank(ExpUnitID)),
    new_order_sub = ifelse(subids_all_integers,
                           rank(as.integer(SubUnitID)),
                           rank(SubUnitID))) |>
    dplyr::arrange(ExpName, !is.na(SubUnitID), EnvName, new_order_obs, new_order_sub) |>
    dplyr::ungroup() |>
    dplyr::select(!c(expids_all_integers,
                     subids_all_integers,
                     new_order_obs,
                     new_order_sub))

  # observations are long format (np x 1), need to be wide (n x p)
  df_obs <- dplyr::bind_rows(lapply(json_obs, clean_json_obs)) |>
    tidyr::pivot_wider(names_from = "observationVariableName",
                       values_from = "value")

  # dbIds dropped by default, as they make the df much harder to read
  df_final <- dplyr::left_join(df_obsunits, df_obs,
                               by = "observationUnitDbId")
  if (include_dbids == FALSE){
    df_final <- df_final |>
      dplyr::select(!observationUnitDbId)
  }
  if (drop_empty_columns == TRUE){
    empty_cols <- apply(df_final, 2, function(x) all(is.na(x)))
    df_final <- df_final[,!empty_cols]
  }

  # TODO - make a request to a /lists/ endpoint to get the ordering of variables
  # this is currently bugged as of mid-May 2026, need to wait for a fix
  df_final
}

clean_json_obsunits <- function(json) {
  data <- json$result$data
  if (length(data) == 0){
    return(data.frame())
  }

  # pull the levelName and the unit ID into the appropriate columns
  # levelOrder = 0 for observations, 1 for sub-observations
  data$ExpUnit <- ifelse(data$observationUnitPosition.observationLevel.levelOrder == 0,
                         data$observationUnitPosition.observationLevel.levelName,
                         NA)
  data$SubObsUnit <- ifelse(data$observationUnitPosition.observationLevel.levelOrder == 1,
                            data$observationUnitPosition.observationLevel.levelName,
                            NA)

  data$ExpUnitID <- ifelse(data$observationUnitPosition.observationLevel.levelOrder == 0,
                           data$observationUnitName,
                           NA)
  data$SubUnitID <- ifelse(data$observationUnitPosition.observationLevel.levelOrder == 1,
                           data$observationUnitName,
                           NA)

  # observationLevelRelationships is an array, so it won't be un-flattened
  # it contains Rep, Block, and the containing ObsUnit if one exists
  # vectorizing this could be nice, but it's much easier to read with an index loop
  data$Rep <- NA
  data$Block <- NA
  data$ParentObsUnitDbId <- NA
  for (i in seq(nrow(data))){
    vals <- unpack_level_df(data[[i,"observationUnitPosition.observationLevelRelationships"]])
    data[i,"Rep"] <- vals[1]
    data[i,"Block"] <- vals[2]
    if (length(vals) > 2){
      data[i,"ExpUnit"] <- names(vals)[3]
      # the DbID of the parent obs has a suffix like " [CCBMAD-3]"
      # clean this out before saving
      data[i,"ParentObsUnitDbId"] <- strsplit(vals[3], " [", fixed = TRUE)[[1]][1]
    }
  }

  # if we have any sub-observations, pull the appropriate unit ID using its parent's DbId
  if (any(!is.na(data$ParentObsUnitDbId))){
    lookup <- data$ExpUnitID
    names(lookup) <- data$observationUnitDbId
    lookup <- na.omit(lookup)
    data$ExpUnitID <- dplyr::if_else(is.na(data$ExpUnitID),
                                     lookup[data$ParentObsUnitDbId],
                                     data$ExpUnitID)
  }

  data <- rename_brapi_columns(data, 'observationunits')
  data
}

clean_json_obs <- function(json) {
  data <- json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  # there is some metadata in the Observation response
  # but it is all redundant with data from ObsUnits
  # validating this is fairly costly from a time perspective
  # just pull the values and DbIds for merging purposes
  data |> dplyr::select(observationUnitDbId,
                        observationVariableName,
                        value)
  # side note - Observations response often contains year data
  # It's unwise to use this, since some Envs have no observations yet
  # better to pull this info from Seasons
}

# observationUnitPosition.observationLevelRelationships is an array
# it won't be delisted, but instead becomes a 2- or 3-row data frame
# convert to a named vector for simplicity
unpack_level_df <- function(df){
  codes <- df$levelCode
  names(codes) <- df$levelName
  parent_name <- setdiff(df$levelName, c("rep","block"))
  codes[c("rep","block",parent_name)]
}








