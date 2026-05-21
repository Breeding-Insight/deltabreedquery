# expands a data frame to include all strings given in target_cols as headers
add_columns_to_match <- function(df, target_cols){
  missing_cols <- setdiff(target_cols, colnames(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  df <- df[, target_cols]
  df
}

# rename columns of a df from BrAPI names to DeltaBreed names
# mapping comes from data-raw/brapi_name_key
rename_brapi_columns <- function(resp_df, endpoint){
  brapi_name_key |>
    dplyr::filter(endpoint_name == endpoint) -> filtered_key
  # add missing columns (if any)
  missing_cols <- setdiff(filtered_key$name_brapi, colnames(resp_df))
  for (col in missing_cols) {
    resp_df[[col]] <- NA
  }
  # create a named vector lookup for use with rename()
  endpoint_lookup <- filtered_key$name_brapi
  names(endpoint_lookup) <- filtered_key$name_output
  df <- resp_df |>
    dplyr::rename(all_of(endpoint_lookup))
  df
}

rename_new <- function(data, mapping_vector){
  # mapping_vector has DeltaBreed terms as names, BrAPI terms as values
  # add missing columns (if any)
  missing_cols <- setdiff(na.omit(mapping_vector),
                          colnames(data))
  for (col in missing_cols) {
    data[[col]] <- NA
  }
  df <- data |>
    dplyr::rename(all_of(na.omit(mapping_vector))) |>
    dplyr::select(names(mapping_vector))

  df
}


