#' Import csv file
#'
#' Reads csv input file using read.csv2
#'
#' @param input absolute or relative path to .csv file
#'
#' @return data frame.
#'@export
read_file <- function(input) {
  df <- utils::read.csv2(input)
  df
}


#' Check all column names are as expected
#'
#' @param df data frame output of \link[ddvR]{read_file}
#'
#' @return logical
#' @export
check_columns <- function(df) {
  all.equal(colnames(df), colz)
}


#' Fix data types
#'
#' The output of \link[ddvR]{read_file} will usually have only character types
#' because of a faulty header row. This function fixes the column types, which
#' also introduces NAs by coersion. These are later removed with
#' the \link[ddvR]{remove_na_rows} function. Also rename ST_DEJAVNOSTI while i'm
#' at it
#'
#' @param df data frame output of \link[ddvR]{read_file}
#'
#' @return data frame with same dimensions as input
#' @export
fix_types <- function(df) {
  df   %>%
    dplyr::mutate(DATUM = as.Date(DATUM, format = "%d.%m.%Y"),
                  OSNOVA_DAVKA  = suppressWarnings(
                    as.numeric(gsub(",", ".", OSNOVA_DAVKA))),
                  STEVILO = suppressWarnings(as.numeric(STEVILO)),
                  ZNESEK = suppressWarnings(as.numeric(gsub(",", ".", ZNESEK))),
                  ZNESEK_DAVKA = suppressWarnings(as.numeric(gsub(",", ".",ZNESEK_DAVKA))),
                  SKD_5 = ST_DEJAVNOSTI) %>%
    dplyr::select(-ST_DEJAVNOSTI)

}


#' Remove illegal values for STOPNJA
#'
#' @param df data frame output of \link[ddvR]{fix_types} or data frame output of \link[ddvR]{remove_na_rows}
#'
#' @return data frame with same number of columns as input and possibly fewer rows
#' @export
remove_xrates <- function(df) {
  df %>%
    dplyr::filter(STOPNJA %in% ratez)
}


#' Recodes known old and wrong SKD codes
#'
#' Sometimes old SKD codes are used instead of new ones. When these old ones
#' don't exist in the new classification, we can flag them and replace them
#' with the correct ones. This says nothing about the cases where old codes
#' are used and there is no way for us to know.¯\_(ツ)_/¯
#' Additionally, some inputs might have a 4-digit skd code with the leading
#' 0 missing. This function tacks the 0 back on.
#'
#' @param df data frame output of \link[ddvR]{fix_types} or data frame output of \link[ddvR]{remove_na_rows}
#'
#' @return data frame with same dimensions as input
#' @export
recode_skd <- function(df) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(SKD_5 =
                    sub("^[1-9]{1}\\.",
                        paste0("0", regmatches(SKD_5,
                                               regexpr("^[1-9]{1}\\.",SKD_5))),
                        SKD_5)) %>%
    dplyr::mutate(SKD_5 =
                    dplyr::recode(SKD_5, !!!skd_recode_lookup))
}


#' Remove illegal values for ST_DEJAVNOSTI
#'
#' First remove all rows where the format is not xx.xxx or empty. of the remaining ones
#' leave the ones in the code table, the rest make empty, since they are unknown
#' in the new classification.
#'
#' @param df data frame output of \link[ddvR]{fix_types} or data frame output of \link[ddvR]{remove_na_rows}
#'
#' @return data frame with same number of columns as input and possibly fewer rows
#' @export
remove_xskd <- function(df) {
  df %>%
    dplyr::filter(grepl("^[0-9]{2}\\.[0-9]{3}$", SKD_5)| SKD_5 == "") %>%
    dplyr::mutate(SKD_5 =
                    ifelse(!SKD_5 %in% c(skdz, ""), "", SKD_5))
}


#' Remove rows with NAs
#'
#' @param df data frame output of \link[ddvR]{fix_types}
#'
#' @return data frame with same number of columns as input and possibly fewer rows
#' @export
remove_na_rows <- function(df) {
  df  %>%
    dplyr::select(-SKD_5) %>%
    stats::complete.cases() %>%
    # `!` %>% # if you want to see incomplete cases
    df[., ]
}
