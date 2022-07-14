#' Write data frame to postgres
#'
#' This code only works with relevant credentials.
#'
#' @param df dataframe output of \link[ddvR]{ddv_transform}
#' @param db name of database on the local server with the appropriate table
#' @param tbl name of the table in the database. defaults to davcni_racuni, but
#' might also be test123 for example
#' @return data frame with number of rows in database table.
#' using mock db for testing.
#'
#' @export
write_to_db <- function(df, db = "test", tbl =  "davcni_racuni") {
  rlog::log_info(paste0("Attempting to write to the table ", tbl, "."))

  tryCatch({
    RPostgres::dbWriteTable(con, tbl, df, row.names=FALSE, append=TRUE)
    query <- paste0("SELECT count(*) from ", tbl)
    out <- dbGetQuery(con, query)
    n <- out[1,1]
    rlog::log_info(paste0(nrow(df), " new rows added to the table for new a total of ", n, " rows." ))
    rlog::log_info(paste0("This week's total revenues are ", ballpark_last_week(df), " % of the previous week's, just to give you a ballpark idea. \n                        ##################################################################"))
    invisible(out)
  },
  error = function(cnd){
    rlog::log_info(paste0("Writing to ", tbl, " was unsuccessful."))
    rlog::log_info(paste0("This probably means the data was already in the table. But for the record, this is the original error: \n", cnd))
  }
  )
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
#' Convenience wrapper for sending the log to a bunch of recipients.
#'
#' @param log path to log file
#' @param recipient email (not checked) address to be sent to as BCC.
#' I think single email is all that's allowed. Haven't tried more.
#'
#' @return nothing, side effect is the email being sent.
#' @export
#'
email_log <- function(log, recipient) {
  text_msg <- gmailr::gm_mime() %>%
    gmailr::gm_bcc(recipient) %>%
    gmailr::gm_subject("FURS DDV (VAT) data import") %>%
    gmailr::gm_html_body(paste0("To je avtomatsko sporo\u010dilo. <br><br>",
                                "Na stre\u017eniku <code>umar-bi</code> so v bazo <code>davcni_racuni</code> dodani novi zapisi za pretekli teden.<br> ",
                                "V priponki je log z dodatnimi informacijami. <br><br>",
                                "Tvoj Umar Data Bot &#129302;")) %>%
    gmailr::gm_attach_file(log)

  gmailr::gm_send_message(text_msg)
}


#' Wrapper function for complete ELT pipeline
#'
#' This wrapper function runs the whole pipeline in the \link[ddvR]{ddv_import} and then
#' \link[ddvR]{ddv_transform} functions and finally \link[ddvR]{write_to_db}, while
#' logging everything to the sink and emailing the logs to the listed recipients
#'
#' @param new_file base filename of the appropriate .csv file that is located in
#'  O:/Avtomatizacija/furs-surs-soap/data/
#' @param tbl which table in the database to write to. Default is test123, but the real
#' one is at davcne_blagajne
#' @param email One or more emails to send logs to - as character vector. If NA,
#' no emails are sent.
#'
#' @return Nothing, just side effects :). Writes to the database and emails logs.
#' @export
#'
update_ddv <- function(new_file, tbl = "test123", email = "maja.zaloznik@gov.si") {
  log <- paste0("log/log_", format(Sys.time(), "%d-%b-%Y %H.%M.%S"), ".log")
  sink(log)
  input <- paste0("O:/Avtomatizacija/furs-surs-soap/data/", new_file)
  df <- ddv_import(input)
  df <- ddv_transform(df)
  write_to_db(df, tbl = tbl)
  sink()
  #if(all(!is.na(email))) sapply(email, function(who) email_log(log, recipient = who))
  email_log(log, recipient = email)
}



