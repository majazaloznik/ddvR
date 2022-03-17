colz <- c("DATUM", "STOPNJA", "ST_DEJAVNOSTI", "OSNOVA_DAVKA",
          "STEVILO", "ZNESEK", "ZNESEK_DAVKA")

ratez <- c("22%", "9,5%",    "5%", "ostalo")

skd_recode_lookup <- list("01.131" = "01.210",
                          "60.240" = "49.410")

# get skd
library(readxl)
url <- "https://www.stat.si/Klasje/Klasje/createXlsx?q=5531&s=1"
temp <- tempfile()
download.file(url,temp, mode = "wb")
nace <- read_excel(temp)
unlink(temp)

nace %>%
  dplyr::filter(nchar(`Šifra kategorije`) == 7) %>%
  dplyr::mutate(skd.5 = gsub("[^0-9.]+", "", `Šifra kategorije`)) %>%
  dplyr::select(skd.5) %>%
  dplyr::pull()-> skdz


usethis::use_data(colz,
                  ratez,
                  skd_recode_lookup,
                  skdz,
                  internal = TRUE, overwrite = TRUE)

