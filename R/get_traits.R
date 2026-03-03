library(httr2)
library(dplyr)
#' Get trait data
#'
#' @description Retrieves trait data from a DeltaBreed program via BrAPI.
#' @return Data frame of trait information drawn from BrAPI endpoints
#' @export
#' @examples
#' \dontrun{
#' get_traits()
#' }

get_traits <- function(include_archived = FALSE) {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  # Note - BrAPI nomenclature around trait endpoints is a bit confusing
  # There are endpoints for Ontology, ObservationVariable, and Trait
  # not to mention the Method and Scale endpoints
  json_traits <- httr2::execute_get_request(env$full_url, env$access_token,
                                            "variables", verbose = FALSE)
  dfs_traits <- lapply(json_traits, clean_json_traits)
  df <- dplyr::bind_rows(dfs_traits) |>
    dplyr::arrange(Name) |>
    dplyr::mutate(Units = dplyr::if_else(ScaleClass == "Numerical", Units, ""))
  cat("Number of traits found: \t", nrow(df), "\n")
  if (!include_archived) {
    df <- df |> dplyr::filter(Status != "archived")
    cat("Number of active traits: \t", nrow(df), "\n")
  }
  df
}

clean_json_traits <- function(json) {
  data = json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  data |>
    # the final value of synonyms (?) is the full name
    dplyr::mutate(FullName = sapply(trait.synonyms,
                                    function(x) tail(x,1)),
                  # if any more names exist past those two, then concat them together
                  # note that this is different from the table view within DB
                  # but it more closely matches the upload template
                  Synonyms = sapply(trait.synonyms,
                                    function(x) ifelse(length(x) > 2,
                                                       paste0(x[2:(length(x)-1)], collapse = ";"),
                                                       "")),
                  Categories = sapply(scale.validValues.categories, collapse_trait_categories)) |>
    rename_brapi_columns('variables') |>
    dplyr::select(Name, FullName, Synonyms, Entity, Attribute, Method,
                  ScaleClass, Units, Min, Max, Categories, Status)
}

collapse_trait_categories <- function(df) {
  if (is.null(df)){
    out_str = ""
  } else if (ncol(df) == 1) {    # Nominal vars have only 1 column
    out_str = paste(df$value, collapse = "; ")
  } else if (ncol(df) == 2) {    # Ordinal vars have 2
    paired_labels = paste(df$value, df$label, sep = "=")
    out_str = paste(paired_labels, collapse = "; ")
  }
  out_str
}
