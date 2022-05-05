test_that("check date splitting works OK", {
  clean_df <- remove_na_rows(remove_xskd(recode_skd(remove_xrates(fix_types(read_file(
    test_path("testdata", "test_import_03.csv")))))))
  expect_equal( ncol(date_split(clean_df)), 16)
  expect_lte(max(date_split(clean_df)$DAN), 31)
  expect_gte(min(date_split(clean_df)$DAN), 1)
  expect_lte(max(date_split(clean_df)$MESEC), 12)
  expect_gte(min(date_split(clean_df)$MESEC), 1)
  expect_lte(max(date_split(clean_df)$LETO), as.numeric(format(Sys.Date(), "%Y")))
  expect_gte(min(date_split(clean_df)$LETO), 2019)
  expect_lte(max(date_split(clean_df)$TEDEN), 53)
  expect_gte(min(date_split(clean_df)$TEDEN), 1)
})


test_that("check skd recoding works OK", {
  clean_df <- remove_na_rows(remove_xskd(recode_skd(remove_xrates(fix_types(read_file(
    test_path("testdata", "test_import_03.csv")))))))
  expect_equal(ncol(skd_2(clean_df)), 8)
  expect_equal(max(nchar(skd_2(clean_df)$SKD_2), na.rm = TRUE), 2)
  expect_equal(max(nchar(skd_2(clean_df)$SKD_2), na.rm = TRUE), 2)
  expect_equal(ncol(skd_alpha(skd_2(clean_df))), 9)
  expect_equal(max(nchar(skd_alpha(skd_2(clean_df))$SKD_ALPHA), na.rm = TRUE), 1)
  df <- skd_2(remove_na_rows(remove_xskd(recode_skd(remove_xrates(fix_types(read_file(
    test_path("testdata", "test_import_05.csv"))))))))
  expect_equal(ncol(skd_filter(df)), 9)
  expect_true(sum(skd_filter(df)$FILTER) == 1)
  df <- skd_retail(skd_2(remove_na_rows(remove_xskd(recode_skd(remove_xrates(fix_types(read_file(
    test_path("testdata", "test_import_06.csv")))))))))
  expect_equal(ncol(df), 9)
  expect_equal(length(unique(df$SKD_2PLUS)), 5)
})

test_that("check ely weeks work ok", {
  df <- date_split(ddv_import(test_path("testdata", "test_import_13.csv")))
  expect_equal(df$ISO_TEDEN[1], "2019-W01")
  expect_equal(df$ISO_TEDEN[2], "2020-W01")
  expect_equal(df$ISO_TEDEN[3], "2020-W01")
  expect_equal(df$ISO_TEDEN[4], "2020-W53")
  expect_equal(df$ISO_TEDEN_ELY[1], "2018-W01")
  expect_equal(df$ISO_TEDEN_ELY[2], "2019-W01")
  expect_equal(df$ISO_TEDEN_ELY[3], "2019-W01")
  expect_equal(df$ISO_TEDEN_ELY[4], "2020-W01")
})

test_that("check ely months work ok", {
  df <- date_split(ddv_import(test_path("testdata", "test_import_13.csv")))
  expect_equal(df$LETO_MESEC[1], "2019-01")
  expect_equal(df$LETO_MESEC[2], "2019-12")
  expect_equal(df$LETO_MESEC_ELY[1], "2018-01")
  expect_equal(df$LETO_MESEC_ELY[2], "2018-12")
})

test_that("check skd recoding works OK", {
  expect_equal(ddv_transform(ddv_import(test_path("testdata", "test_import_10.csv")))$skd_2, "64")
  expect_equal(ddv_transform(ddv_import(test_path("testdata", "test_import_01.csv")))$skd_2[7], "64")})

test_that("check skd NAs are now zero", {
  df <- skd_zero(skd_filter(skd_retail(skd_alpha(skd_2(date_split(ddv_import(
    test_path("testdata", "test_import_07.csv"))))))))
  expect_equal(df$SKD_5[2], "0")
  expect_equal(df$SKD_2[2], "0")
  expect_equal(df$SKD_ALPHA[2], "0")
  expect_equal(df$SKD_2PLUS[2], "0")
  })



test_that("transform workflow works", {
  expect_equal(ncol(ddv_transform(
    ddv_import(test_path("testdata", "test_import_01.csv")))), 20)
} )



