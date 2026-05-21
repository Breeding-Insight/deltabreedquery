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

  json <- build_get_request(env$full_url,
                            env$access_token,
                            "germplasm") |>
    execute_get_request()
  dfs <- lapply(json, clean_json_germplasm)
  df <- dplyr::bind_rows(dfs)
  df
}

# cleaning function applied to each page of response JSON
clean_json_germplasm <- function(json) {
  data <- json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  mapping_germplasm <- define_mapping_germplasm()
  renamed <- rename_new(data, mapping_germplasm) |>
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
    "CreatedDate" = "additionalInfo.createdDate",
    "CreatedBy" = "additionalInfo.createdBy.userName"
  )
  mapping
}

