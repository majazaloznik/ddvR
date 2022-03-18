# column names that need to be present
colz <- c("DATUM", "STOPNJA", "ST_DEJAVNOSTI", "OSNOVA_DAVKA",
          "STEVILO", "ZNESEK", "ZNESEK_DAVKA")

# the only legal tax rates
ratez <- c("22%", "9,5%",    "5%", "ostalo")

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
  dplyr::mutate(ST_DEJAVNOSTI = gsub("[A-z]{1}", "", ST_DEJAVNOSTI)) -> retail_codes

usethis::use_data(colz,
                  ratez,
                  skd_recode_lookup,
                  skdz,
                  skd_aplha_lookup,
                  skd_filter_codes,
                  retail_codes,
                  internal = TRUE, overwrite = TRUE)

