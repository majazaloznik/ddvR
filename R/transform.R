#' Add columns that split date into constituent parts
#'
#' Splits the date into day, month and year columns as well as the ISO
#' week and adds all four columns
#'
#' @param df data frame output of import functions (add link)
#'
#' @return data frame with 4 more columns than before
#' @export
date_split <- function(df) {
  df %>%
    dplyr::mutate(DAN = as.numeric(format(DATUM, "%d")),
           MESEC = as.numeric(format(DATUM, "%m")),
           LETO = as.numeric(format(DATUM, "%Y")),
           TEDEN =  lubridate::isoweek(DATUM),
           TEDEN_US = format(DATUM, "%U")) %>%
    dplyr::relocate(DAN, MESEC, LETO, TEDEN, TEDEN_US,  .after= DATUM)
}


#' Add 2 digit SKD code
#'
#' Extracts 2 digit SKD code from 5 digit one
#'
#' @param df data frame output of import functions (add link)
#'
#' @return data frame with 1 more column than before
#' @export
skd_2 <- function(df) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(SKD_2 = strsplit(SKD_5, "\\.")[[1]][1])
}



#' Add alpha SKD code
#'
#' Adds a single character SKD code from the lookup table.
#'
#' @param df data frame output of import functions \link[ddvR]{skd_2}
#'
#' @return data frame with 1 more column than before
#' @export
skd_alpha <- function(df) {
  df %>%
   dplyr::left_join(skd_aplha_lookup, by = "SKD_2")
}

#' Add filter for some SKD codes to exclude
#'
#' SOmetimes we like to exclude a set of SKD codes, at the moment they are set
#' to c(35, 36, 52, 61, 64), This filter is set to 0 for those codes
#'
#' @param df data frame output of import functions \link[ddvR]{skd_2}
#'
#' @return data frame with 1 more column than before
#' @export
skd_filter <- function(df) {
  df %>%
    dplyr::mutate(FILTER = ifelse(SKD_2 %in% skd_filter_codes, 0, 1))
}


#' Adds additional SKD categorisation with retail subgroups
#'
#' This is the standard 2/digit classification with three additional subgroups
#' for the retail sale sector
#'
#' @param df data frame output of import functions \link[ddvR]{skd_2}
#'
#' @return data frame with 1 more column than before
#' @export

skd_retail <- function(df) {
  df %>%
    dplyr::left_join(retail_codes, by = "SKD_5") %>%
    dplyr::mutate(SKD_2PLUS = ifelse(is.na(SKD_2PLUS), SKD_2, SKD_2PLUS))
}


#' Run whole transform sequence
#' Run thourgh all the transformation steps and at the end also change the
#' colum names to lowercase, because that's how postgres likes them.
#'
#' @param df data frame outpuit from \link[ddvR]{ddv_import}
#'
#' @return df with 15 columns
#' @export
#'
ddv_transform <- function(df){
  df %>%
    date_split() %>%
    skd_2() %>%
    skd_alpha() %>%
    skd_retail() %>%
    skd_filter() -> df
    names(df) <- tolower(names(df))
    df
}
