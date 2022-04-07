#' Write data frame to postgres
#'
#' This code only works with relevant credentials.
#'
#' @param df dataframe output of \link[ddvR]{ddv_transform}
#' @param db name of database on the local server with the davcni_racuni table
#' @param usr user for db access
#' @param psw password for db access

#' @return data frame with number of rows in database table.
#' using mock db for testing.
#'
#' @export
write_to_db <- function(df, db = "ddvtest", usr = "ddvr", psw = Sys.getenv("PG_DDVR_PSW")) {
  rlog::log_info(paste0("Writing to the ", db, " database."))
  con <- RPostgres::dbConnect(RPostgres::Postgres(),
                              dbname = db,
                              host = "localhost",
                              port = 5432,
                              user = usr,
                              password = psw)
  on.exit(dbDisconnect(con))
  try(RPostgres::dbWriteTable(con, "davcni_racuni", df, row.names=FALSE, append=TRUE))

  query <- "SELECT count(*) from davcni_racuni"

  n <- return(dbGetQuery(con, query))[1,1]
  rlog::log_info(paste0(nrow(df), "new rows added to the table for a total of ", n, "rows in total." ))

}

