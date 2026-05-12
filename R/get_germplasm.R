#' Get germplasm data
#'
#' @description Retrieves all germplasm data from the current DeltaBreed instance.
#' @return Germplasm data from the BrAPI API
#' @export
#' @examples
#' \dontrun{
#' login_deltabreed()
#' germplasm <- get_germplasm()
#' }
get_germplasm <- function() {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  # send GET request and clean JSON responses
  json <- build_get_request(env$full_url, env$access_token, "germplasm") |>
    execute_get_request()
  dfs <- lapply(json, clean_json_germplasm)
  df <- dplyr::bind_rows(dfs)
  df
}


clean_json_germplasm <- function(json) {
  data <- json$result$data
  # account for empty responses, if any occur
  if (length(data) == 0){
    return(data.frame())
  }
  rename_brapi_columns(data, 'germplasm') |>
    dplyr::select(GID, GermplasmName, BreedingMethod,
           Source, Pedigree,
           CreatedDate, CreatedBy) |>
    dplyr::arrange(as.integer(GID))
}

