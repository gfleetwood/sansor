detect_outliers_mad <- function(group, interval = 2){
  
  med = median(group)
  mad_ = mad(group)
  lower_bnd = med - interval*mad_
  upper_bnd = med + interval*mad_
  results <- between(group, lower_bnd, upper_bnd)
  
  # The inversion is necessary to have the outliers labeled at 1 instead of 0
  results_inverted <- as.integer(1 - results)
  
  return(results_inverted)
  
}

check_key <- function(dict, key){

    result <- ifelse(key %in% names(fromJSON(dict)), fromJSON(dict)[key], NA)

    return(result)

}

create_db_doc_template <- function(con){

    tbls <- odbc::dbListTables(con)
    # DBI::dbReadTable(con_pg, tbls[1])

    docs_template <- data.frame(tables = tbls, stringsAsFactors = FALSE) %>%
        mutate(schema = "public") %>%
        select(schema, tables) %>%
        purrrlyr::by_row(
            function(x) dbListFields(con, pull(x, tables)),
            .collate = "rows",
            .to = "column"
        ) %>%
        select(-.row) %>%
        mutate(description = "")

    return(docs_template)

}

extract_json <- function(df){

    result <- df %>%
        mutate(
            col_new = unlist(future_map(col, ~ check_key(.x, "")))
        )

    return(result)

}
