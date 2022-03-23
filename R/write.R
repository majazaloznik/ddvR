#' Write data frame to postgres
#'
#' @param df dataframe output of \link[ddvR]{ddv_transform}
#' @param db name of database on the local server with the davcni_racuni table
#'
#' @return data frame with number of rows in database table.
#' using mock db for testing.
#'
#' @export
write_to_db <- function(df, db = "ddvtest") {
  con <- RPostgres::dbConnect(RPostgres::Postgres(),
                              dbname = db,
                              host = "localhost",
                              port = 5432,
                              user = "ddvr",
                              password = Sys.getenv("PG_DDVR_PSW"))
  on.exit(dbDisconnect(con))
  try(RPostgres::dbWriteTable(con, "davcni_racuni", df, row.names=FALSE, append=TRUE))

  query <- "SELECT count(*) from davcni_racuni"

  return(dbGetQuery(con, query))
}

