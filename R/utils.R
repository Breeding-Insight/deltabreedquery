# operates on a single page of JSON response
json_to_data <- function(json){
  data <- json$result$data
  if (length(data) == 0){
    return(data.frame())
  }
  data
}

# response pages should always be packaged into a list
# even if only one page
json_list_to_df <- function(json_list){
  df_list <- lapply(json_list, json_to_data)
  df <- dplyr::bind_rows(df_list)
  df
}

# convert column names in a data frame from BrAPI convention to DeltaBreed names
# also handles the ordering
# mapping vector comes from define_mapping_xyz() function in each get_xyz.R file
brapi_to_db_names <- function(data, mapping_vector){
  # mapping_vector has DeltaBreed terms as names, BrAPI terms as values
  renamed <- data |>
    dplyr::rename(any_of(na.omit(mapping_vector)))
  # Add any columns that happened to be missed
  missing_cols <- setdiff(names(mapping_vector),
                          colnames(renamed))
  for (col in missing_cols) {
    renamed[[col]] <- NA
  }
  renamed <- renamed |>
    dplyr::select(names(mapping_vector))
  renamed
}
