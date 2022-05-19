#' @title TBL To DB
#' @description U
#' @param con DB connection
#' @param schema Schema name
#' @param tbl_path The path to the table
#' @return The df variable without the anomalous columns
#' @export

create_tbl = function(data, schema_name, tbl_name){

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


read_db_data_thru_tunnel = function(qry){
   
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
