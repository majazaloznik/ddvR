#' Write data frame to postgres
#'
#' This code only works with relevant credentials.
#'
#' @param df dataframe output of \link[ddvR]{ddv_transform}
#' @param db name of database on the local server with the davcni_racuni table
#'
#' @return data frame with number of rows in database table.
#' using mock db for testing.
#'
#' @export
write_to_db <- function(df, db) {
  rlog::log_info(paste0("Writing to the ", db, " database."))

  try(RPostgres::dbWriteTable(con, "davcni_racuni", df, row.names=FALSE, append=TRUE))

  query <- "SELECT count(*) from davcni_racuni"

  out <- dbGetQuery(con, query)
  n <- out[1,1]
  rlog::log_info(paste0(nrow(df), " new rows added to the table for a total of ", n, " rows in total." ))
  rlog::log_info(paste0("This week's total revenues are ", ballpark_last_week(df), " % of the previous week's, just to give you a ballpark idea. \n                        ##################################################################"))
  invisible(out)
  }



#' Compare this week's total with last week's
#'
#' This is to check the ballpark of the imported values is approximately correct
#' should not be used for anything real. If first week of the year, the comparison
#' is with wek 52 of the previous year, regardless of whether week 53 exists. Because
#' it doesn't matter and i can't be bothered.
#'
#' @param df dataframe output of \link[ddvR]{ddv_transform}
#'
#' @return single value expressed as percentage of previous week's total
#' @export
#'
ballpark_last_week <- function(df) {

  df %>% dplyr::ungroup() %>%
    dplyr::summarise(wk = min(teden)) %>%  dplyr::pull() -> a
  df %>% dplyr::ungroup() %>%
    dplyr::summarise( yr = max(leto)) %>%  dplyr::pull() -> b
  dplyr::tbl(con, "davcni_racuni") %>%
    dplyr::filter(if (a != 1) teden == a - 1 &  leto == b else
      teden == 52 &  leto == b - 1) %>%
    dplyr::summarise(znesek  = sum(znesek, na.rm = TRUE)) %>%  dplyr::pull() -> old

  df %>%  dplyr::ungroup() %>%
    dplyr::summarise(znesek = sum(znesek )) %>%
    dplyr::pull() %>%  `/`(old) %>%
    `*` (100) %>%
    round(2)
}

#' Send email with log
#'
#' Convenience wrapper for sending the log
#' @param log path to log file
#' @param recipient I think single email is all that's allowed. Haven't tried more
#'
#' @return nothing, side effect is the email being sent.
#' @export
#'
email_log <- function(log, recipient = "maja.zaloznik@gov.si") {
  text_msg <- gmailr::gm_mime() %>%
    gmailr::gm_to(recipient) %>%
    gmailr::gm_subject("FURS DDV (VAT) data import") %>%
    gmailr::gm_from("maja.zaloznik@gmail.com") %>%
    gmailr::gm_text_body(paste("This is an automated email. \n New data",
                               "has been processed by the ddvR automated script, and the log with more details",
                               "is in the attachment.")) %>%
    gmailr::gm_attach_file(log)

  gmailr::gm_send_message(text_msg)
}
