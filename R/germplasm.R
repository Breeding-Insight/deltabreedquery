#' Retrieve germplasm data
#'
#' @description Retrieves all germplasm data from the current DeltaBreed
#'   instance, reformatting it to match DeltaBreed layout.
#' @return Data frame of germplasm/accession/entry information.
#' @export
#' @examples
#' login_deltabreed("example", verbose = FALSE)
#'
#' germplasm <- get_germplasm()
#' head(germplasm)
get_germplasm <- function(page_size = 10000) {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run login_deltabreed() to authenticate first.")
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
    dplyr::mutate("GID" = as.integer(.data$GID),
                  "FemaleParentGID" = as.integer(.data$FemaleParentGID),
                  "MaleParentGID" = as.integer(.data$MaleParentGID),
                  "Pedigree" = dplyr::if_else(.data$Pedigree == "", NA, .data$Pedigree),
                  "CreatedDate" = as.Date.character(.data$CreatedDate, format = "%d/%m/%Y")) |>
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
