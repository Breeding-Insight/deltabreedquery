#' Retrieve germplasm data
#'
#' @param page_size Page size to use for the response. Larger page sizes may decrease total retrieval time.
#'
#' @description Retrieves all germplasm data from the current DeltaBreed instance.
#' @return Data frame of germplasm/accession/entry information drawn from BrAPI `/germplasm` endpoint.
#' Data is formatted
#' @export
#' @examples
#' login_deltabreed("example", verbose = FALSE)
#'
#' germplasm <- get_germplasm()
#' head(germplasm)
get_germplasm <- function(page_size = 10000) {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  if (is_example_mode()) {
    df <- load_example_json("germplasm.json") |> json_list_to_df()
  } else {
    df <- build_get_request(.dbc_env$full_url,
                              .dbc_env$access_token,
                              "germplasm",
                              page_size = page_size) |>
      execute_get_request() |>
      json_list_to_df()
  }

  if (nrow(df) == 0){
    return(df)
  }

  mapping_germplasm <- define_mapping_germplasm()
  renamed <- brapi_to_db_names(df, mapping_germplasm) |>
    dplyr::mutate("GID" = as.integer(.data$GID)) |>
    dplyr::arrange(.data$GID)
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
