#' Get observation variables (trait definitions) from a DeltaBreed instance.
#'
#' @param verbose Whether to print a short message about the number of traits found.
#' @param include_archived Should the output include archived (non-active) traits?
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
                          include_archived = FALSE) {
  if (!auth_exists()) {
    stop("No authentication credentials found. ",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)

  # BrAPI nomenclature around trait endpoints is a bit confusing
  # lots of endpoints, but the one we need is mostly in /variables
  df <- build_get_request(env$full_url,
                          env$access_token,
                          "variables",
                          page_size = 1000) |>
    execute_get_request() |>
    json_list_to_df()

  # filter / report
  if (verbose == TRUE) cat("Number of traits found: \t", nrow(df), "\n")
  if (!include_archived) {
    df <- df |> dplyr::filter(.data$status != "archived")
  }
  if (verbose == TRUE) cat("Number of active traits found: \t", nrow(df), "\n")

  if (nrow(df) == 0) return(df)

  # scale.validValue.categories will only appear if there are any ordinal/nominal vars
  # separate the handling of this column out on its own, simpler this way
  if ("scale.validValues.categories" %in% colnames(df)){
    df <- df |>
      dplyr::mutate("Categories" = sapply(.data$scale.validValues.categories,
                                        collapse_trait_categories))
  } else {
    df["scale.validValues.categories"] = NA
  }

  df <- df |>
    dplyr::mutate("FullName" = sapply(.data$trait.synonyms,
                                    function(x) utils::tail(x,1)),
                  # only put values into Synonyms field if alternate names exist
                  # Name and FullName are technically synonyms, but this redundancy is not useful
                  Synonyms = sapply(.data$trait.synonyms,
                                    function(x) ifelse(length(x) > 2,
                                                       paste0(x[2:(length(x)-1)], collapse = "; "),
                                                       NA)
                                    ),
                  Trait = paste(.data$trait.entity,
                                .data$trait.attribute)
                  )

  mapping_vars <- define_mapping_variables()
  df <- brapi_to_db_names(df, mapping_vars) |>
    dplyr::arrange("Name") |>
    # non-numerical trait Units are "" but really should be NA
    dplyr::mutate("Units" = dplyr::if_else(.data$ScaleClass == "Numerical",
                                         .data$Units,
                                         NA))

  df
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


