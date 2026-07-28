#' Retrieve all observation data
#'
#' Retrieves all observation and sub-observation data from a DeltaBreed
#' instance via BrAPI call, converting it into a data frame that resembles
#' how the data appears on DeltaBreed itself.
#'
#' @param page_size Page size to use when making the request. Larger page size can make requests faster but may cause errors.
#' @param verbose Whether to print short messages showing the number of records found.
#' @param drop_empty_columns Whether to drop all empty columns (including metadata columns) from the returned data frame
#' @param include_dbids Whether to include the DbIds of the observation units, mostly useful for debugging.
#'
#' @return A data frame of observations (phenotypes), formatted in DeltaBreed style.
#' @export
#' @examples
#' \dontrun{
#' login_deltabreed()
#' obs <- get_observations()
#' }
get_observations <- function(page_size = 10000,
                             drop_empty_columns = FALSE,
                             include_dbids = FALSE,
                             verbose = TRUE) {
  if (!auth_exists()) {
    stop("No authentication credentials found.",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  if (verbose) cat("Requesting observation units...\n")
  df_obsunits <- build_get_request(.dbc_env$full_url,
                                   .dbc_env$access_token,
                                   "observationunits",
                                   page_size = page_size) |>
    execute_get_request() |>
    json_list_to_df()

  # I don't think it should be possible for a program to have have valid expts but no obs units defined
  # but in case it does happen, warn and return the empty df
  if (nrow(df_obsunits) == 0){
    if (verbose == TRUE) cat("Experiments exist in the target program, but no observation units were found.")
    return(df_obsunits)
  }

  # need to merge in Year from the seasons/ endpoint
  expts <- get_experiments(verbose = FALSE,
                           include_dbids = TRUE)
  df_obsunits <- dplyr::left_join(df_obsunits,
                                  expts[,c("studyDbId","Year")],
                                  by = "studyDbId")

  mapping_obsunits <- define_mapping_obsunits()
  df_obsunits <- handle_subunits_obsdf(df_obsunits)
  df_obsunits <- brapi_to_db_names(df_obsunits,
                                   mapping_obsunits)

  if (verbose) cat("Requesting phenotype values...\n")
  df_obs <- build_get_request(.dbc_env$full_url,
                              .dbc_env$access_token,
                              "observations",
                              page_size = page_size) |>
    execute_get_request() |>
    json_list_to_df()

  if (nrow(df_obs) > 0){
    df_obs <- df_obs |>
      dplyr::select("observationUnitDbId",
                    "observationVariableName",
                    "value") |>
      tidyr::pivot_wider(names_from = "observationVariableName",
                         values_from = "value")

    # save num of phenotype cols for later, useful for ordering columns
    # df_obs should just have two columns
    n_pheno_cols <- ncol(df_obs) - 1

    df_final <- dplyr::left_join(df_obsunits,
                                 df_obs,
                                 by = dplyr::join_by("ObsUnitDbId" == "observationUnitDbId"))

  } else {
    n_pheno_cols <- 0
    df_final <- df_obsunits
  }

  # drop columns as needed
  if (include_dbids == FALSE){
    df_final <- df_final |>
      dplyr::select(!c("ObsUnitDbId", "ParentObsUnitDbId"))
  }

  df_final <- sort_obsdf_rows(df_final)
  df_final <- sort_obsdf_columns(df_final, n_pheno_cols)
  df_final <- type_obsdf_columns(df_final,
                                 get_variables(verbose = FALSE),
                                 n_pheno_cols = n_pheno_cols)

  if (drop_empty_columns == TRUE){
    empty_cols <- apply(df_final, 2, function(x) all(is.na(x)))
    df_final <- df_final[,!empty_cols]
  }
  df_final
}

#' Retrieve a filtered list of observation data
#'
#' This function retrieves a filtered list of observation data using one or more
#' filters provided by the user, packaging the response into a data frame of the
#' same format as get_observations().
#'
#' This function is best used by calling get_experiments() first, in order to
#' see what experiments are actually present in the target DeltaBreed instance.
#'
#' @param year A year or vector of years.
#' @param location A location name or vector of locations.
#' @param exp_name An experiment name or vector of names.
#' @param env_name An environment name or vector of names.
#' @param exp_type An experiment type or vector of types.
#' @param page_size Page size to use for the request. Larger page sizes can decrease total time needed, but may also throw errors.
#' @param drop_empty_columns Whether to drop all empty columns (including metadata columns) from the returned data frame
#' @param include_dbids Whether to include the DbIds of the observation units, mostly useful for debugging.
#' @param verbose Whether to print short messages about the number of records found.
#'
#' @returns A data frame of observations using the supplied filters.
#' @export
#'
#' @examples \dontrun{
#' filter_observations(year = 2025)
#' filter_observations(year = c(2024, 2025), location = "Ithaca")
#' }
filter_observations <- function(year = NA,
                                location = NA,
                                exp_name = NA,
                                env_name = NA,
                                exp_type = NA,
                                page_size = 10000,
                                drop_empty_columns = FALSE,
                                include_dbids = FALSE,
                                verbose = TRUE){
  if (all(is.na(c(year, location, exp_name, env_name, exp_type)))){
    stop("Please specify a year, location, experiment name, environment name, and/or experiment type. ",
         "To retrieve all observation data, use get_observations().")
  }
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  expts <- get_experiments(verbose = FALSE, include_dbids = TRUE)
  filt_expts <- expts |>
    dplyr::filter(.data$Year %in% year | all(is.na(year)),
                  .data$Location %in% location | all(is.na(location)),
                  .data$ExpName %in% exp_name | all(is.na(exp_name)),
                  .data$EnvName %in% env_name | all(is.na(env_name)),
                  .data$ExpType %in% exp_type | all(is.na(exp_type)))

  if (nrow(filt_expts) == 0){
    if (verbose == TRUE) cat("No experiments found with the requested filters.\n")
    return()
  }
  if (verbose) cat(nrow(filt_expts), "matching environment(s) found.\n")


  # base requests we will append and reuse for each envt (study) in the filter
  basereq_obs   <- build_get_request(.dbc_env$full_url,
                                     .dbc_env$access_token,
                                     'observations',
                                     page_size = page_size)
  basereq_obsunits <- build_get_request(.dbc_env$full_url,
                                        .dbc_env$access_token,
                                        'observationunits',
                                        page_size = page_size)

  # iterate across the list of studies
  obsunit_dfs <- list()
  obs_dfs <- list()
  if (verbose) cat("Requesting observation units and observations...\n")
  for (i in 1:nrow(filt_expts)){
    if (verbose) {
      cat("Expt:", filt_expts[i,"ExpName"], "Envt:", filt_expts[i,"EnvName"], "\n")
    }
    dbid = filt_expts[i,"studyDbId"]
    req_obsunits <- basereq_obsunits |>
      httr2::req_url_query(studyDbId = dbid)
    obsunit_dfs[[i]] <- execute_get_request(req_obsunits) |>
      json_list_to_df()

    req_obs <- basereq_obs |>
      httr2::req_url_query(studyDbId = dbid)
    obs_dfs[[i]] <- execute_get_request(req_obs) |>
      json_list_to_df()
  }

  df_obsunits <- dplyr::bind_rows(obsunit_dfs)
  df_obs <- dplyr::bind_rows(obs_dfs)

  # I don't think it should be possible for a program to have have valid expts but no obs units defined
  # but in case it does happen, warn and return the empty df
  if (nrow(df_obsunits) == 0){
    if (verbose == TRUE) cat("Experiments exist in the target program, but no observation units were found\n")
    return(df_obsunits)
  }

  # Year is stored separately in the /seasons endpoint, merge that in
  df_obsunits <- dplyr::left_join(df_obsunits,
                                  expts[,c("studyDbId","Year")],
                                  by = "studyDbId")

  mapping_obsunits <- define_mapping_obsunits()
  df_obsunits <- handle_subunits_obsdf(df_obsunits)
  df_obsunits <- brapi_to_db_names(df_obsunits,
                                   mapping_obsunits)

  if (nrow(df_obs) > 0){
    df_obs <- df_obs |>
      dplyr::select("observationUnitDbId",
                    "observationVariableName",
                    "value") |>
      tidyr::pivot_wider(names_from = "observationVariableName",
                         values_from = "value")

    # save num of phenotype cols for later, useful for ordering columns
    # df_obs should just have two columns
    n_pheno_cols <- ncol(df_obs) - 1

    df_final <- dplyr::left_join(df_obsunits,
                                 df_obs,
                                 by = dplyr::join_by("ObsUnitDbId" == "observationUnitDbId"))

  } else {
    n_pheno_cols <- 0
    df_final <- df_obsunits
  }

  # drop columns as needed
  if (include_dbids == FALSE){
    df_final <- df_final |>
      dplyr::select(!c("ObsUnitDbId", "ParentObsUnitDbId"))
  }

  df_final <- sort_obsdf_rows(df_final)
  df_final <- sort_obsdf_columns(df_final, n_pheno_cols)
  df_final <- type_obsdf_columns(df_final,
                                 get_variables(verbose = FALSE),
                                 n_pheno_cols = n_pheno_cols)

  if (drop_empty_columns == TRUE){
    empty_cols <- apply(df_final, 2, function(x) all(is.na(x)))
    df_final <- df_final[,!empty_cols]
  }

  return(df_final)
}


define_mapping_obsunits <- function(){
  mapping <- c(
    "ExpName" = "trialName",
    "EnvName" = "studyName",
    "Location" = "locationName",
    "Year" = "Year",
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

# observationUnitPosition.observationLevelRelationships is an array
# it won't get delisted, instead becomes a 2- or 3-row data frame
# convert to a named vector, much faster than pivoting each df
unpack_level_df <- function(relship_df){
  # level code is static, no matter what the obs unit names are
  # 3 is rep, 4 is block, 0 is either the parent (if one exists) or the unit itself
  rep_block_parent <- relship_df |>
    dplyr::filter(.data$levelOrder %in% c(0,3,4))
  codes <- rep_block_parent$levelCode
  names(codes) <- rep_block_parent$levelName
  parent_name <- setdiff(rep_block_parent$levelName, c("rep","block"))
  return(codes[c("rep","block", parent_name)])
}


# observation units have a fairly complex handling
# desired output is to have any observation units that have a parent labeled as sub-observation units instead
handle_subunits_obsdf <- function(df){
  missing_colnames <- setdiff(
    c("observationUnitPosition.observationLevel.levelOrder",
      "observationUnitPosition.observationLevel.levelName",
      "observationUnitName",
      "observationUnitPosition.observationLevelRelationships"),
    colnames(df)
  )
  if (length(missing_colnames) > 0){
    stop("One or more necessary column(s) are missing from data frame to be sorted.\n",
         "Missing column(s):",
         missing_colnames,
         "Column names of df being sorted:",
         colnames(df))
  }

  # pull levelName and unitName into the appropriate columns
  # levelOrder = 0 for observations, 1 for sub-observations
  df$ExpUnit <- ifelse(df$observationUnitPosition.observationLevel.levelOrder == 0,
                       df$observationUnitPosition.observationLevel.levelName,
                       NA)
  df$SubUnit <- ifelse(df$observationUnitPosition.observationLevel.levelOrder == 1,
                       df$observationUnitPosition.observationLevel.levelName,
                       NA)
  df$ExpUnitID <- ifelse(df$observationUnitPosition.observationLevel.levelOrder == 0,
                         df$observationUnitName,
                         NA)
  df$SubUnitID <- ifelse(df$observationUnitPosition.observationLevel.levelOrder == 1,
                         df$observationUnitName,
                         NA)

  # scrape the df found in each cell of the observationLevelRelationships column
  # this will remain un-flattened when we run httr2::resp_body_json()
  # it contains Rep, Block, and the containing ObsUnit (if one exists)
  # vectorizing this could be nice, but it's much easier to read with an index loop
  df$Rep <- NA
  df$Block <- NA
  df$ParentObsUnitDbId <- NA
  # unpack_level_df() turns the data frame into a 2- or 3-length vector
  for (i in seq(nrow(df))){
    vals <- unpack_level_df(df[[i,"observationUnitPosition.observationLevelRelationships"]])
    df[i,"Rep"] <- vals[1]
    df[i,"Block"] <- vals[2]
    if (length(vals) > 2){
      df[i,"ExpUnit"] <- names(vals)[3]
      # the DbID of the parent obs has a suffix like " [CCBMAD-3]"
      # clean this out before saving
      df[i,"ParentObsUnitDbId"] <- strsplit(vals[3], " [", fixed = TRUE)[[1]][1]
    }
  }
  # if we have sub-observations, pull the appropriate UnitIDs using the parent's DbId
  if (any(!is.na(df$ParentObsUnitDbId))){
    lookup <- df$ExpUnitID
    names(lookup) <- df$observationUnitDbId
    lookup <- stats::na.omit(lookup)
    df$ExpUnitID <- dplyr::if_else(is.na(df$ExpUnitID),
                                   lookup[df$ParentObsUnitDbId],
                                   df$ExpUnitID)
  }
  return(df)
}

# sorting gets kind of complex so we can handle integer ExpUnitIDs and/or SubUnitIDs
# for IDs like 1,2,[...],10,11,12, we want to sort that as an integer, not as a string
# make this check separately within each expt/envt (study level in BrAPI terms)
sort_obsdf_rows <- function(df){
  missing_colnames <- setdiff(
    c("SubUnitID", "ExpName", "EnvName","ExpUnitID","SubUnitID"),
    colnames(df)
  )
  if (length(missing_colnames) > 0){
    stop("One or more necessary column(s) are missing from data frame to be sorted.\n",
         "Missing column(s):\n",
         cat(missing_colnames, sep = "\n"),
         "Column names of df being sorted:",
         cat(colnames(df), sep = "\n"))
  }

  df <- df |>
    dplyr::mutate("has_subobs" = ! is.na(.data$SubUnitID)) |>
    dplyr::arrange(.data$ExpName,
                   .data$has_subobs,
                   .data$EnvName) |>
    dplyr::group_by(.data$ExpName, .data$has_subobs, .data$EnvName) |>
    # if ALL obs unit IDs or sub-obs unit IDs are integers for a given expt/envt
    # then treat them like integers
    dplyr::mutate("expids_all_integers" = all(!grepl("\\D", .data$ExpUnitID)),
                  "subids_all_integers" = all(!grepl("\\D", .data$SubUnitID))) |>
    dplyr::mutate("new_order_obs" = ifelse(.data$expids_all_integers,
                                         rank(as.integer(.data$ExpUnitID)),
                                         rank(.data$ExpUnitID)),
                  "new_order_sub" = ifelse(.data$subids_all_integers,
                                         rank(as.integer(.data$SubUnitID)),
                                         rank(.data$SubUnitID))) |>
    dplyr::arrange(.data$ExpName,
                   .data$EnvName,
                   .data$has_subobs,
                   .data$new_order_obs,
                   .data$new_order_sub) |>
    dplyr::ungroup() |>
    dplyr::select(!c("expids_all_integers",
                     "subids_all_integers",
                     "new_order_obs",
                     "new_order_sub",
                     "has_subobs"))
  df
}

# ordering of columns in Obs response is random, so we alphabetize for repeatability
# we could in theory reconstruct column ordering from lists/ endpoint
# this is bugged on the DeltaBreed end though, icebox for now (June 2026)
sort_obsdf_columns <- function(df, n_pheno_cols){
  if (n_pheno_cols == 0){
    return(df)
  } else if (n_pheno_cols < 1){
    stop("Expected a positive number of phenotype columns. Number supplied:", n_pheno_cols)
  } else {
    trait_index <- (ncol(df) - n_pheno_cols + 1):ncol(df)
    pheno_cols <- colnames(df)[trait_index]
    df[, trait_index] <- df[, sort(pheno_cols)]
    colnames(df)[trait_index] <- sort(pheno_cols)
    return(df)
  }
}

# apply appropriate data types using the obs variable definitions in /variables endpoint
# dtypes are present in the initial JSON but lost upon import to R
type_obsdf_columns <- function(df, var_df, n_pheno_cols){
  if (n_pheno_cols == 0){
    return(df)
  }
  # Date class doesn't work on tibbles, so ensure it's a vanilla df first
  # accessing first element here bc class(tibble) = c(tbl_df, tbl, data.frame)
  if (class(df)[1] != "data.frame"){ df <- as.data.frame(df) }

  # Row and Column are allowed to be non-integers, so don't convert them
  for (col in c("GID","Rep","Block","Year")){
    df[,col] = as.integer(df[,col])
  }
  first_var_col <- ncol(df) - n_pheno_cols + 1
  for (j in first_var_col:ncol(df)){
    var_name <- colnames(df)[j]
    if (! var_name %in% var_df$Name){
      stop("Column to be typed was not found in program observation variable names: ",
           var_name)
    }
    dtype <- var_df |>
      dplyr::filter(.data$Name == var_name) |>
      dplyr::pull("ScaleClass")
    level_str <- var_df |>
      dplyr::filter(.data$Name == var_name) |>
      dplyr::pull("Categories")
    if (dtype == "Numerical"){
      df[,j] <- as.numeric(df[,j])
    } else if (dtype == "Text"){
      df[,j] <- as.character(df[,j])
    } else if (dtype == "Nominal"){
      df[,j] <- factor(df[,j],
                       levels = strsplit(level_str, "; *")[[1]])
    } else if (dtype == "Ordinal"){
      # we expect 1=Low; 2=Medium, etc, but that's not actually enforced
      # people could use 3=High;2=Medium;1=Low, A=Low,B=Medium;C=High, etc
      # can't fix everything, just use the ordering of levels as it occurs in the db itself
      split_once <- strsplit(level_str, "; *")[[1]]
      split_twice <- strsplit(split_once, "= *")
      vals <- sapply(split_twice, function(x) x[1])
      df[,j] <- factor(df[,j],
                       levels = sapply(split_twice, function(x) x[1]))
    } else if (dtype == "Date"){
      df[,j] <- as.Date(df[,j],
                        format = "%Y-%m-%d")
    } else {
      warning("Could not resolve the data type for the following column:\n",
              var_name,
              "\nData type: ", dtype, "\n")
    }
  }
  return(df)
}



