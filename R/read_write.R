#' @title TBL To DB
#' @description U
#' @param con DB connection
#' @param schema Schema name
#' @param tbl_path The path to the table
#' @return The df variable without the anomalous columns
#' @export

write_tbl_to_db <- function(data, schema, tbl_name){

    tbl <- read_csv(tbl_path)

    dbWriteTable(
        con,
        DBI::Id(schema = schema, table = tbl_name),
        data
    )

    return(TRUE)

}

#' @title Create Schema
#' @description U
#' @param con DB connection
#' @param schema Schema name
#' @return The df variable without the anomalous columns
#' @export

create_schema <- function(con, schema){

    dbGetQuery(
        con,
        glue::glue("
    IF NOT EXISTS (
    SELECT schema_name FROM information_schema.schemata
    WHERE schema_name = '{schema}'
    )
    BEGIN
	    EXEC('CREATE SCHEMA {schema}')
    END
    ")
    )

    return(TRUE)

}
