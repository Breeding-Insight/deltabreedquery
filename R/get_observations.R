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
  df_obsunits <- build_get_request(env$full_url,
                                   env$access_token,
                                   "observationunits",
                                   page_size = page_size) |>
    execute_get_request() |>
    json_list_to_df()

  ### handling obs units and sub-obs units
  # pull the levelName and the unit ID into the appropriate columns
  # levelOrder = 0 for observations, 1 for sub-observations
  df_obsunits$ExpUnit <- ifelse(df_obsunits$observationUnitPosition.observationLevel.levelOrder == 0,
                                df_obsunits$observationUnitPosition.observationLevel.levelName,
                                NA)
  df_obsunits$SubUnit <- ifelse(df_obsunits$observationUnitPosition.observationLevel.levelOrder == 1,
                                df_obsunits$observationUnitPosition.observationLevel.levelName,
                                NA)
  df_obsunits$ExpUnitID <- ifelse(df_obsunits$observationUnitPosition.observationLevel.levelOrder == 0,
                                  df_obsunits$observationUnitName,
                                  NA)
  df_obsunits$SubUnitID <- ifelse(df_obsunits$observationUnitPosition.observationLevel.levelOrder == 1,
                                  df_obsunits$observationUnitName,
                                  NA)
  # scrape the observationLevelRelationships array, which will remain un-flattened
  # it contains Rep, Block, and the containing ObsUnit (if one exists)
  # vectorizing this could be nice, but it's much easier to read with an index loop
  df_obsunits$Rep <- NA
  df_obsunits$Block <- NA
  df_obsunits$ParentObsUnitDbId <- NA
  for (i in seq(nrow(df_obsunits))){
    vals <- unpack_level_df(df_obsunits[[i,"observationUnitPosition.observationLevelRelationships"]])
    df_obsunits[i,"Rep"] <- vals[1]
    df_obsunits[i,"Block"] <- vals[2]
    if (length(vals) > 2){
      df_obsunits[i,"ExpUnit"] <- names(vals)[3]
      # the DbID of the parent obs has a suffix like " [CCBMAD-3]"
      # clean this out before saving
      df_obsunits[i,"ParentObsUnitDbId"] <- strsplit(vals[3], " [", fixed = TRUE)[[1]][1]
    }
  }

  # if we have sub-observations, pull the appropriate UnitIDs using the parent's DbId
  if (any(!is.na(df_obsunits$ParentObsUnitDbId))){
    lookup <- df_obsunits$ExpUnitID
    names(lookup) <- df_obsunits$observationUnitDbId
    lookup <- na.omit(lookup)
    df_obsunits$ExpUnitID <- dplyr::if_else(is.na(df_obsunits$ExpUnitID),
                                            lookup[df_obsunits$ParentObsUnitDbId],
                                            df_obsunits$ExpUnitID)
  }

  mapping_obsunits <- define_mapping_obsunits()
  df_obsunits <- brapi_to_db_names(df_obsunits, mapping_obsunits)

  if (verbose) cat("Requesting phenotype values...\n")
  df_obs <- build_get_request(env$full_url,
                              env$access_token,
                              "observations",
                              page_size = page_size) |>
    execute_get_request() |>
    json_list_to_df() |>
    # there is some plot metadata in the Observation response fields
    # but it is all redundant with data from ObsUnits
    # validating this is fairly costly from a time perspective, so we skip it here
    dplyr::select(observationUnitDbId,
                  observationVariableName,
                  value) |>
    # observations are long format (np x 1), need to be wide (n x p)
    tidyr::pivot_wider(names_from = "observationVariableName",
                       values_from = "value")
  # save num of phenotype cols for later, so we can count backwards from the right side
  # gives us more flexibility in which columns we drop from the final df
  n_pheno_cols <- ncol(df_obs) - 1

  df_final <- dplyr::left_join(df_obsunits, df_obs,
                               by = dplyr::join_by(ObsUnitDbId == observationUnitDbId))
  if (include_dbids == FALSE){
    df_final <- df_final |>
      dplyr::select(!c(ObsUnitDbId, ParentObsUnitDbId))
  }
  if (drop_empty_columns == TRUE){
    empty_cols <- apply(df_final, 2, function(x) all(is.na(x)))
    df_final <- df_final[,!empty_cols]
  }

  # final sorting, kind of complex so we can handle integer ExpUnitIDs and/or SubUnitIDs
  # for IDs like 1,2,[...],10,11,12, we want to sort that as an integer, not as a string
  df_final <- df_final |>
    dplyr::mutate(has_subobs = !is.na(SubUnitID)) |>
    dplyr::arrange(ExpName, has_subobs, EnvName) |>
    dplyr::group_by(ExpName, has_subobs, EnvName) |>
    dplyr::mutate(expids_all_integers = all(!grepl("\\D", ExpUnitID)),
                  subids_all_integers = all(!grepl("\\D", SubUnitID))) |>
    dplyr::mutate(new_order_obs = ifelse(expids_all_integers,
                                         rank(as.integer(ExpUnitID)),
                                         rank(ExpUnitID)),
                  new_order_sub = ifelse(subids_all_integers,
                                         rank(as.integer(SubUnitID)),
                                         rank(SubUnitID))) |>
    dplyr::arrange(ExpName,
                   !is.na(SubUnitID),
                   EnvName,
                   new_order_obs,
                   new_order_sub) |>
    dplyr::ungroup() |>
    dplyr::select(!c(expids_all_integers,
                     subids_all_integers,
                     new_order_obs,
                     new_order_sub,
                     has_subobs))

  # TODO - add the type checking
  # need to check variables endpoint to see what the data types should be
  df_vars <- build_get_request(env$full_url,
                               env$access_token,
                               "variables") |>
    execute_get_request() |>
    json_list_to_df()

  # sort the phenotypic columns, since ordering in the response is random
  # TODO - make a request to a /lists/ endpoint to get the ordering of variables
  # these endpoints are currently bugged as of May 2026, need to wait for a fix
  trait_index <- (ncol(df_final) - n_pheno_cols + 1):ncol(df_final)
  pheno_cols <- colnames(df_final)[trait_index]
  df_final[, trait_index] <- df_final[, sort(pheno_cols)]
  colnames(df_final)[trait_index] <- sort(pheno_cols)

  # TODO - make a request to a /lists/ endpoint to get the ordering of variables
  # this is currently bugged as of mid-May 2026, need to wait for a fix
  df_final
}

# observationUnitPosition.observationLevelRelationships is an array
# it won't get delisted, instead becomes a 2- or 3-row data frame
# convert to a named vector, much faster than pivoting each df
unpack_level_df <- function(df){
  codes <- df$levelCode
  names(codes) <- df$levelName
  parent_name <- setdiff(df$levelName, c("rep","block"))
  codes[c("rep","block",parent_name)]
}


define_mapping_obsunits <- function(){
  mapping <- c(
    "ExpName" = "trialName",
    "EnvName" = "studyName",
    "Location" = "locationName",
    "ExpUnit" = NA,
    "ExpUnitID" = NA,
    "SubUnit" = NA,
    "SubUnitID" = NA,
    "GermplasmName" = "germplasmName",
    "GID" = "additionalInfo.gid",
    "TestOrCheck" = "observationUnitPosition.entryType",
    "Rep" = NA,
    "Block" = NA,
    "Row" = "observationUnitPosition.positionCoordinateX",
    "Column" = "observationUnitPosition.positionCoordinateY",
    "ObsUnitDbId" = "observationUnitDbId",
    "ParentObsUnitDbId" = NA
  )
  mapping
}




