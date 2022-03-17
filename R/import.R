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
#' the \link[ddvR]{remove_na_rows} function.
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
                  ZNESEK_DAVKA = suppressWarnings(as.numeric(gsub(",", ".",ZNESEK_DAVKA))))

}



#' Remove rows with NAs
#'
#' @param df data frame output of \link[ddvR]{fix_types}
#'
#' @return data frame with same number of columns as input and possibly fewer rows
#' @export
remove_na_rows <- function(df) {
  df  %>%
    dplyr::select(-ST_DEJAVNOSTI) %>%
    stats::complete.cases() %>%
    # `!` %>% # if you want to see incomplete cases
    df[., ]
}




