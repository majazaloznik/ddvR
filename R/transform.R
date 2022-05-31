#' Add columns that split date into constituent parts
#'
#' Splits the date into day, month and year columns as well as the ISO
#' week and adds all four columns. Also add  ISO year and week in YYYY-WxX format,
#' because teden and leto are not enough tu unambiguously determine the correct iso week.
#' Then also adds the "Equivalent last year" or ELY week, which is the one exactly
#' 52 weeks ago. WHich is the simplest way to do y-o-y stuff I believe.
#' For convenienve adds a column with the year-month in YYYY-MM format and the
#' "Equivalent last year" for easy y-o-y calculations
#'
#' @param df data frame output of import functions (add link)
#'
#' @return data frame with 8 more columns than before
#' @export
date_split <- function(df) {
  rlog::log_info("Adding date and week columns.")
  df %>%
    dplyr::mutate(DAN = as.numeric(format(DATUM, "%d")),
           MESEC = as.numeric(format(DATUM, "%m")),
           LETO = as.numeric(format(DATUM, "%Y")),
           TEDEN =  lubridate::isoweek(DATUM),
           TEDEN_US = format(DATUM, "%U")) %>%
    dplyr::mutate(ISO_TEDEN = paste0(lubridate::isoyear(DATUM), "-W",
                                     formatC(lubridate::isoweek(DATUM), format = "f",
                                             digits = 0, width = 2, flag = "0")),
                  ISO_TEDEN_ELY = paste0(lubridate::isoyear(DATUM - 364), "-W",
                                         formatC(lubridate::isoweek(DATUM - 364), format = "f",
                                                 digits = 0, width = 2, flag = "0"))) %>%
    dplyr::mutate(LETO_MESEC = paste0(LETO, "-",
                                      formatC(MESEC, format = "f",
                                              digits = 0, width = 2, flag = "0")),
                  LETO_MESEC_ELY = paste0(LETO - 1, "-",
                                          formatC(MESEC, format = "f",
                                                  digits = 0, width = 2, flag = "0"))) %>%
    dplyr::relocate(DAN, MESEC, LETO, TEDEN, TEDEN_US,ISO_TEDEN, ISO_TEDEN_ELY,
                    LETO_MESEC, LETO_MESEC_ELY, .after= DATUM)
}


#' Add 2 digit SKD code
#'
#' Extracts 2 digit SKD code from 5 digit one
#'
#' @param df data frame output of import functions (add link)
#'
#' @return data frame with 1 more column than before
#' @export
#'
skd_2 <- function(df) {
  rlog::log_info("Adding SKD code columns and filter.")
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


#' Change all empty or NA SKDs into zeros
#'
#' @param df data frame output of import functions \link[ddvR]{ddv_import}
#'
#' @return data frame with sam dimensions as input
#' @export

skd_zero <- function(df) {
  df %>%
    dplyr::mutate(SKD_5 = dplyr::na_if(SKD_5, "")) %>%
    tidyr::replace_na(list(SKD_2 = "0", SKD_2PLUS = "0", SKD_5 = "0", SKD_ALPHA = "0"))
}

#' Run whole transform sequence
#' Run thourgh all the transformation steps and at the end also change the
#' colum names to lowercase, because that's how postgres likes them.
#'
#' @param df data frame outpu from \link[ddvR]{ddv_import}
#'
#' @return df with 20 columns
#' @export
#'
ddv_transform <- function(df){
  df %>%
    date_split() -> x
  x %>%
    skd_2() -> x
  x %>%
    skd_alpha() -> x
  x %>%
    skd_retail()-> x
  x %>%
    skd_filter() -> x
  x %>%
    skd_zero() -> df
    names(df) <- tolower(names(df))
    rlog::log_info(paste0("Completed data transformations.  \n"))
    df
}


#' Manually ovverride some wrong hotel data
#'
#' 11.-15.12.2019 there was an odd anomaly in the data for the code 55.100
#' This function manually replaces the data in the original table with some
#' stuff we kinda made up.
#'
#' @param df data frame output of import functions \link[ddvR]{ddv_transform}
#'
#' @return data frame with same dimensions as input
#' @export

override_55 <- function(df){
  df %>%
    dplyr::left_join(replacement, by = c("datum", "stopnja", "skd_5")) %>%
    dplyr::mutate(znesek = ifelse(is.na(znesek.y), znesek.x, znesek.y),
                  znesek_davka = ifelse(is.na(znesek_davka.y), znesek_davka.x, znesek_davka.y),
                  osnova_davka = ifelse(is.na(osnova_davka.y), osnova_davka.x, osnova_davka.y)) %>%
    dplyr::select(-znesek.y, -znesek.x, -znesek_davka.y,
                  -znesek_davka.x, -osnova_davka.x, -osnova_davka.y)
}

