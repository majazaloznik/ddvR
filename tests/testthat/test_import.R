
test_that("imported df conforms to requirements", {
  expect_equal(is.data.frame(read_file(
    test_path("testdata", "test_import_01.csv"))), TRUE)
  expect_equal(check_columns(read_file(
    test_path("testdata", "test_import_01.csv"))), TRUE)
  expect_false(check_columns(read_file(
    test_path("testdata", "test_import_02.csv"))))
})

test_that("column types have been fixed correctly", {
  df_types <- fix_types(read_file(
    test_path("testdata", "test_import_03.csv")))
  is.date <- function(x) inherits(x, 'Date')
  expect_equal(is.date(df_types$DATUM), TRUE)
  expect_type(df_types$STOPNJA, "character")
  expect_type(df_types$SKD_5 , "character")
  expect_type(df_types$OSNOVA_DAVKA, "double")
  expect_type(df_types$STEVILO, "integer")
  expect_type(df_types$ZNESEK, "double")
  expect_type(df_types$ZNESEK_DAVKA, "double")
})

test_that("column types have been fixed correctly", {
  df_no_dups <- remove_duplicates(fix_types(read_file(
    test_path("testdata", "test_import_07.csv"))))
  expect_equal(nrow(df_no_dups), 5)
})

test_that("only legal values are in coded columns", {
 clean_rates <- remove_xrates(fix_types(read_file(
   test_path("testdata", "test_import_03.csv"))))
 expect_equal(nrow(clean_rates), 16)
 clean_rates <- remove_xrates(fix_types(read_file(
   test_path("testdata", "test_import_08.csv"))))
 expect_equal(length(unique(clean_rates$STOPNJA)), 1)
 recoded <- recode_skd(fix_types(read_file(
   test_path("testdata", "test_import_04.csv"))))
 expect_equal(unique(recoded$SKD_5), c("01.210", "49.410", "01.610",
                                               "08.610", "25.520", "22.290", "43.320"))
 clean_skd <- remove_xskd(recode_skd(fix_types(read_file(
   test_path("testdata", "test_import_03.csv")))))
 expect_equal(nrow(clean_skd), 15)
 clean_codes <- remove_xrates(fix_types(read_file(
   test_path("testdata", "test_import_03.csv"))))
 expect_equal(nrow(clean_codes), 16)
 clean_df <- remove_na_rows(remove_xskd(recode_skd(remove_xrates(fix_types(read_file(
   test_path("testdata", "test_import_03.csv")))))))
 expect_equal(nrow(clean_df), 10)
 clean_df <- remove_na_rows(remove_xskd(recode_skd(remove_xrates(fix_types(read_file(
   test_path("testdata", "test_import_11.csv")))))))
 expect_equal(nrow(clean_df), 4)
 })

test_that("post recode aggregation is done OK", {
  clean_df <- remove_na_rows(remove_xskd(recode_skd(remove_xrates(remove_duplicates(fix_types(read_file(
    test_path("testdata", "test_import_09.csv"))))))))
  expect_equal(nrow(sum_duplicates(clean_df)), 2)
})

test_that("import workflow works", {
  expect_equal(nrow(ddv_import(test_path("testdata", "test_import_01.csv"))), 19)
  expect_equal(nrow(ddv_import(test_path("testdata", "test_import_03.csv"))), 10)
  expect_error(nrow(ddv_import(test_path("testdata", "test_import_02.csv"))))
} )


test_that("numeric columns don't loose decimal points", {
  df_types <- fix_types(read_file(
    test_path("testdata", "test_import_12.csv")))
  expect_equal(df_types$OSNOVA_DAVKA[1] , 36.84)
})
