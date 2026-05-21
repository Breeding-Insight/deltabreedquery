#' Get observation variables (trait definitions) from a DeltaBreed instance.
#'
#' @param verbose Whether to print a short message about the number of traits found.
#' @param include_archived Should the output include archived (non-active) traits?
#' @param page_size Page size used in the request.
#'
#' @description Retrieves trait data from a DeltaBreed program via BrAPI.
#' @return Data frame of trait information drawn from BrAPI `/variables`
#' endpoint.
#' @export
#' @examples
#' \dontrun{
#' get_variables()
#' }
get_variables <- function(verbose = TRUE,
                          include_archived = FALSE,
                          page_size = 1000) {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  # BrAPI nomenclature around trait endpoints is a bit confusing
  # lots of endpoints, but the one we need is mostly in /variables
  obsvars_request <- build_get_request(env$full_url,
                                       env$access_token,
                                       "variables",
                                       page_size = page_size)
  json_obsvars <- execute_get_request(obsvars_request)

  dfs_obsvars <- lapply(json_obsvars, clean_json_obsvars)

  df <- dplyr::bind_rows(dfs_obsvars) |>
    dplyr::arrange(Name) |>
    # non-numerical traits are "", should be NA
    dplyr::mutate(Units = dplyr::if_else(ScaleClass == "Numerical", Units, NA))

  cat("Number of traits found: \t", nrow(df), "\n")
  if (!include_archived) {
    df <- df |> dplyr::filter(Status != "archived")
    cat("Number of active traits: \t", nrow(df), "\n")
  }
  df
}

clean_json_obsvars <- function(json) {
  data = json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  mapping_variables <- define_mapping_variables()

  # formatting and display of observation variables is a thicket of thorns
  # the table as seen on DB is quite different from the downloaded version
  # some fields (e.g. full description) are not available via BrAPI calls
  data <- data |>
    dplyr::filter(status == "active") |>
    # the first value of synonyms is the Name field ()
    # the last value of synonyms is the plain-text FullName
    dplyr::mutate(FullName = sapply(trait.synonyms,
                                    function(x) tail(x,1)),
                  # only put values into Synonyms field if alternate names exist
                  # Name and FullName are technically synonyms, but this redundancy is not useful
                  Synonyms = sapply(trait.synonyms,
                                    function(x) ifelse(length(x) > 2,
                                                       paste0(x[2:(length(x)-1)], collapse = "; "),
                                                       NA)),
                  Trait = paste(trait.entity, trait.attribute),
                  Categories = sapply(scale.validValues.categories,
                                      collapse_trait_categories))
  data <- rename_new(data, mapping_variables)
  data
}

# Format the Categories field for categorical data
# Using
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

define_mapping_variables <- function(){
  mapping <- c(
    "Name" = "observationVariableName",
    "FullName" = NA,
    "Trait" = NA,
    "Method" = "method.methodClass",
    "ScaleClass" = "scale.dataType",
    "Units" = "scale.scaleName",
    "Min" = "scale.validValues.min",
    "Max" = "scale.validValues.max",
    "Categories" = NA,
    "Synonyms" = NA,
    "Status" = "status"
  )
  mapping
}


