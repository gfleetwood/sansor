#' @title TBL To DB
#' @description U
#' @param con DB connection
#' @param schema Schema name
#' @param tbl_path The path to the table
#' @return The df variable without the anomalous columns
#' @export
create_tbl <- function(data, schema_name, tbl_name){

    tbl = read_csv(tbl_path)

    dbWriteTable(
        con,
        DBI::Id(schema = schema_name, table = tbl_name),
        data
    )

    return(TRUE)

}

#' @title Read Data Thru Tunnel
#' @description U
#' @param con DB connection
#' @param schema Schema name
#' @param tbl_path The path to the table
#' @return The df variable without the anomalous columns
#' @export
read_db_data_thru_tunnel <- function(qry){
   
   cmd = Sys.getenv("TUNNEL")
   rp = r_bg(function(x) system(x), args = list(cmd))
   
   con = dbConnect(
     odbc(), 
     Driver = Sys.getenv("DRIVER"),
     Database = "data_warehouse",
     Port = "5439", 
     Server = Sys.getenv("SERVER"), 
     UID = Sys.getenv("UN"), 
     PWD = Sys.getenv("PW")
   )
   
   df = dbGetQuery(con, qry)
   rp$kill()
   df
   
}
  
#' @title Read Data Thru Tunnel
#' @description U
#' @param con DB connection
#' @param df schema Schema name
#' @param tbl_name The path to the table
#' @return SQL query to create the dataframe as a table in a database
#' @export
create_sql_table_query <- \(con, df,  tbl_name){
  
  #con <- dbConnect(RSQLite::SQLite(), ":memory:")
  query <- sqlCreateTable(con, tbl_name, df, row.names = F)
  toString(query) %>% str_replace_all("`","")
  
}

#' @title CSV To SQL Insert Statement
#' @description g(iris)
#' @param con DB connection
#' @param schema Schema name
#' @param tbl_path The path to the table
#' @return The df variable without the anomalous columns
#' @export
create_sql_insert_query <- \(df){
    
    create_sql_value_representation <- \(row){

  row_sql_temp <- unname(row) %>%
    map(as.character) %>%
    paste(collapse = ", ")

  glue::glue("({row_sql_temp})")

}

  template = "
  INSERT INTO table_name
  VALUES
  "

  map_chr(1:nrow(df), ~ create_sql_value_representation(df[.x, ])) %>%
    paste(collapse = ",\n ") %>%
    paste(template, ., ";") %>%
    cat()

}
