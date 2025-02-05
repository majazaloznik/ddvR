library(readxl)
library(dplyr)
# column names that need to be present
colz <- c("DATUM", "STOPNJA", "ST_DEJAVNOSTI", "OSNOVA_DAVKA",
          "STEVILO", "ZNESEK", "ZNESEK_DAVKA")

# the only legal tax rates
ratez <- c("22%", "9,5%",  "9,50%",  "5%", "ostalo", "ostalo ")

# old skd codes that we know hot to recode 2002 to 2008
skd_recode_lookup <- list("01.131" = "01.210",
                          "55.232" = "55.202",
                          "45.420" = "43.320")

# old skd codes that we know hot to recode 2008 to 2025
url <- "https://www.stat.si/Klasje/Klasje/createXlsx?q=1872s5531s17977&s=4"
temp <- tempfile()
download.file(url,temp, mode = "wb")
nace <- read_excel(temp)
unlink(temp)

clean <- nace |>
  dplyr::mutate(
    `Izvorna kategorija` = stringr::str_remove(`Izvorna kategorija`, "^[A-Za-z]"),
    `Ciljna kategorija` = stringr::str_remove(`Ciljna kategorija`, "^[A-Za-z]")
  ) |>
  dplyr::filter(
    stringr::str_detect(`Izvorna kategorija`, "^\\d{2}\\.\\d{3}$"),
    stringr::str_detect(`Ciljna kategorija`, "^\\d{2}\\.\\d{3}$")
  )

safe_mappings <- clean |>
  dplyr::group_by(`Ciljna kategorija`) |>
  dplyr::filter(dplyr::n_distinct(`Izvorna kategorija`) >= 1) |>
  dplyr::ungroup() |>
  filter(`Izvorna kategorija` != `Ciljna kategorija`)

# add to the previous ones
skd_recode_lookup <- c(setNames(safe_mappings$`Ciljna kategorija`,
                                safe_mappings$`Izvorna kategorija`),
                       skd_recode_lookup)

# get skd table - this is now SKD 2025
url <- "https://www.stat.si/Klasje/Klasje/createXlsx?q=17977&s=1"
temp <- tempfile()
download.file(url,temp, mode = "wb")
nace <- read_excel(temp)
unlink(temp)

# legal 5 digit skd codes
nace  |>
  dplyr::filter(nchar(`Šifra kategorije`) == 7)  |>
  dplyr::mutate(skd.5 = gsub("[^0-9.]+", "", `Šifra kategorije`)) |>
  dplyr::select(skd.5) |>
  dplyr::pull()-> skdz

# skd 2 digit to alpha lookup table (in 2025 missing 45)
nace  |>
  dplyr::filter(nchar(`Šifra kategorije`) == 3) |>
  dplyr::mutate(SKD_ALPHA = gsub("[0-9$]{2}", "", `Šifra kategorije`),
                SKD_2 = gsub("^[A-z]+", "", `Šifra kategorije`)) |>
  dplyr::select(SKD_2, SKD_ALPHA) -> skd_aplha_lookup

# skd codes that we like to remove sometimes
skd_filter_codes <- c(35, 36, 52, 61, 64)

# skd code grouping for retail sales
retail <- read.csv2("data-raw/47.csv")

retail |>
  dplyr::mutate(SKD_5 = gsub("[A-z]{1}", "", SKD_5)) -> retail_codes




# # replacement data from wrong hotel data was calculated with this code here:
# # the znesek total values came from Janez, this  code just spreads them out
# # as necessary.

# new_points <- data.frame(datum = seq(as.Date("2019-12-11"), as.Date("2019-12-15"), by="1 day"),
#                          znesek = c(1496154, 1829162, 2442374, 1452925, 2041868))
#
# con %>%
# tbl("davcni_racuni") %>%
#   filter(skd_2 == "55", skd_5 != "55.100" | stopnja %in% c("5%", "ostalo")) %>%
#   filter(datum >= "2019-12-11",
#          datum <= "2019-12-15") %>%
#   group_by(datum) %>%
#   summarise(znesek = sum(znesek)) %>%
#   collect() %>%
#   left_join(new_points, by = c("datum" = "datum")) %>%
#   mutate(preostanek = znesek.y-znesek.x) %>%
#   mutate(znesek_22 = preostanek/122* 22,
#          znesek_95 = preostanek/122* 100) %>%
#   tidyr::pivot_longer(znesek_22:znesek_95, names_to = "stopnja") %>%
#   select(datum, stopnja, value) %>%
#   mutate(stopnja = ifelse(stopnja == "znesek_22", "22%", "9,50%"),
#          skd_5 = "55.100",
#          osnova_davka = ifelse(stopnja == "22%", value*100/122, value*100/109.5),
#          znesek_davka = value -osnova_davka) %>%
#   rename(znesek = value) -> replacement
# dput(replacement)

replacement <- structure(list(datum = structure(c(18241, 18241, 18242, 18242,
                                                  18243, 18243, 18244, 18244, 18245, 18245), class = "Date"),
                              stopnja = c("22%",
                                          "9,50%", "22%", "9,50%", "22%", "9,50%", "22%", "9,50%", "22%",
                                          "9,50%"),
                              znesek = c(260641.669344262, 1184734.86065574, 319640.980819672,
                                         1452913.54918033, 422949.296721311, 1922496.80327869, 246822.280983607,
                                         1121919.45901639, 353976.701803279, 1608985.00819672),
                              skd_5 = c("55.100",
                                        "55.100", "55.100", "55.100", "55.100", "55.100", "55.100", "55.100",
                                        "55.100", "55.100"),
                              osnova_davka = c(213640.712577264, 1081949.64443446,
                                                                              262000.803950551, 1326861.68874916, 346679.751410911, 1755704.84317688,
                                                                              202313.34506853, 1024583.98083689, 290144.837543671, 1469392.70154952
                                        ),
                              znesek_davka = c(47000.9567669981, 102785.216221274, 57640.1768691212,
                                               126051.86043117, 76269.5453104004, 166791.960101804, 44508.9359150766,
                                               97335.4781795044, 63831.8642596076, 139592.306647204)),
                         row.names = c(NA,
                                       -10L), class = c("tbl_df", "tbl", "data.frame"))

################################################################################
# rerun this after you make any changes here!!!
################################################################################
usethis::use_data(colz,
                  ratez,
                  skd_recode_lookup,
                  skdz,
                  skd_aplha_lookup,
                  skd_filter_codes,
                  retail_codes,
                  replacement,
                  internal = TRUE, overwrite = TRUE)
#
