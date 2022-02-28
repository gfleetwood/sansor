#' @title TBL To DB
#' @description U
#' @param con DB connection
#' @param schema Schema name
#' @param tbl_path The path to the table
#' @return The df variable without the anomalous columns
#' @export

create_tbl <- function(data, schema, tbl_name){

    tbl <- read_csv(tbl_path)

    dbWriteTable(
        con,
        DBI::Id(schema = schema, table = tbl_name),
        data
    )

    return(TRUE)

}
