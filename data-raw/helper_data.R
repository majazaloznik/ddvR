# column names that need to be present
colz <- c("DATUM", "STOPNJA", "ST_DEJAVNOSTI", "OSNOVA_DAVKA",
          "STEVILO", "ZNESEK", "ZNESEK_DAVKA")

# the only legal tax rates
ratez <- c("22%", "9,5%",  "9,50%",  "5%", "ostalo", "ostalo ")

# old skd codes that we know hot to recode
skd_recode_lookup <- list("01.131" = "01.210",
                          "60.240" = "49.410",
                          "55.232" = "55.202",
                          "28.520" = "25.520",
                          "25.240" = "22.290",
                          "45.420" = "43.320")

# get skd lookup table
library(readxl)
url <- "https://www.stat.si/Klasje/Klasje/createXlsx?q=5531&s=1"
temp <- tempfile()
download.file(url,temp, mode = "wb")
nace <- read_excel(temp)
unlink(temp)

# legal 5 digit skd codes
nace %>%
  dplyr::filter(nchar(`Šifra kategorije`) == 7) %>%
  dplyr::mutate(skd.5 = gsub("[^0-9.]+", "", `Šifra kategorije`)) %>%
  dplyr::select(skd.5) %>%
  dplyr::pull()-> skdz

# skd 2 digit to alpha lookup table
nace %>%
  dplyr::filter(nchar(`Šifra kategorije`) == 3) %>%
  dplyr::mutate(SKD_ALPHA = gsub("[0-9$]{2}", "", `Šifra kategorije`),
                SKD_2 = gsub("^[A-z]+", "", `Šifra kategorije`)) %>%
  dplyr::select(SKD_2, SKD_ALPHA) -> skd_aplha_lookup

# skd codes that we like to remove sometimes
skd_filter_codes <- c(35, 36, 52, 61, 64)

# skd code grouping for retail sales
retail <- read.csv2("data-raw/47.csv")

retail %>%
  dplyr::mutate(SKD_5 = gsub("[A-z]{1}", "", SKD_5)) -> retail_codes




# # replacement data from wrong hotel data was calculated with this code here:

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
