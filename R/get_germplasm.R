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
get_germplasm <- function(page_size = 5000) {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  df <- build_get_request(env$full_url,
                            env$access_token,
                            "germplasm",
                            page_size = page_size) |>
    execute_get_request() |>
    json_list_to_df()

  mapping_germplasm <- define_mapping_germplasm()
  renamed <- brapi_to_db_names(df, mapping_germplasm) |>
    dplyr::arrange(as.integer(GID))
  renamed
}


# define the mappings here, instead of in a .CSV accompanying the package
# ended up being easier to track and manage
# we can also use the ordering of this vector to stipulate the final ordering
define_mapping_germplasm <- function(){
  mapping <- c(
    "GID" = "accessionNumber",
    "GermplasmName" = "germplasmName",
    "BreedingMethod" = "additionalInfo.breedingMethod",
    "Source" = "seedSource",
    "Pedigree" = "additionalInfo.pedigreeByName",
    "FemaleParentGID" = "additionalInfo.femaleParentGid",
    "MaleParentGID" = "additionalInfo.maleParentGid",
    "CreatedDate" = "additionalInfo.createdDate",
    "CreatedBy" = "additionalInfo.createdBy.userName"
  )
  mapping
}

