
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
#'
#' @returns A data frame of observations using the supplied filters.
#' @export
#'
#' @examples
filter_observations <- function(year = NA,
                                location = NA,
                                exp_name = NA,
                                env_name = NA,
                                exp_type = NA,
                                page_size = 5000,
                                verbose = TRUE){
  if (all(is.na(c(year, location, exp_name, env_name, exp_type)))){
    stop("Please specify a year, location, experiment name, environment name, or experiment type. ",
         "To retrieve all observation data, use get_observations().")
  }
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  expts <- get_experiments(verbose = FALSE, include_dbids = TRUE)
  filt_expts <- expts |>
    dplyr::filter(Year %in% year | is.na(year),
                  Location %in% location | is.na(location),
                  ExpName %in% exp_name | is.na(exp_name),
                  EnvName %in% env_name | is.na(env_name),
                  ExpType %in% exp_type | is.na(exp_type))

  if (nrow(filt_expts) == 0){
    stop("No experiments found with the requested filters.")
  }
  if (verbose == TRUE){
    cat(nrow(filt_expts), "matching environment(s) found")
  }

  # base requests we will reuse for each env (study) in the filter
  basereq_obs   <- build_get_request(env$full_url,
                                   env$access_token,
                                   'observations',
                                 page_size = page_size)
  basereq_obsunits <- build_get_request(env$full_url,
                                    env$access_token,
                                    'observationunits',
                                    page_size = page_size)

  df_list = list()
  for (i in seq(nrow(filt_expts))){
    dbid = filt_expts[i,"studyDbId"]
    print(dbid)
    req_obs <- basereq_obs |>
      httr2::req_url_query(studyDbId = dbid)
    json_obs <- execute_get_request(req_obs)

    req_obsunits <- basereq_obsunits |>
      httr2::req_url_query(studyDbId = dbid)
    json_obsunits <- execute_get_request(req_obsunits)

    # Select using the final desired column ordering
    df_obsunits <- dplyr::bind_rows(lapply(json_obsunits, clean_json_obsunits)) |>
      dplyr::select(ExpName,
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
      dplyr::arrange(ExpName, EnvName)

    # slightly complex sorting here, in order to handle integer ExpUnitIDs
    # we want
    df_obsunits <- df_obsunits |>
      dplyr::group_by(ExpName, EnvName) |>
      dplyr::mutate(all_integers = all(!grepl("\\D", ExpUnitID))) |>
      # order(order()) gives us an index for sorting the data frame
      dplyr::mutate(new_order = ifelse(all_integers,
                                       order(order(as.integer(ExpUnitID))),
                                       order(order(ExpUnitID)))) |>
      dplyr::ungroup() |>
      dplyr::arrange(ExpName, EnvName, new_order) |>
      dplyr::select(!c(all_integers, new_order))

    # observations are long format (np x 1), need to be wide (n x p)
    df_obs <- dplyr::bind_rows(lapply(json_obs, clean_json_obs)) |>
      tidyr::pivot_wider(names_from = "observationVariableName",
                         values_from = "value")

    # dbIds needed for merging, but drop after
    # we only want human-readable info in the final data frame
    df_single <- dplyr::left_join(df_obsunits, df_obs,
                                 by = "observationUnitDbId") |>
      dplyr::select(!observationUnitDbId)
    df_list[[i]] = df_single
  }
  df_final = dplyr::bind_rows(df_list)
  df_final
}
