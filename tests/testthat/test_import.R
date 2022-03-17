
test_that("imported df conforms to requirements", {
  expect_equal(is.data.frame(read_file(
    test_path("testdata", "test_import_01.csv"))), TRUE)
  expect_equal(check_columns(read_file(
    test_path("testdata", "test_import_01.csv"))), TRUE)
  expect_type(check_columns(read_file(
    test_path("testdata", "test_import_02.csv"))), "character")
})

test_that("column types have been fixed correctly", {
  df_types <- fix_types(read_file(
    test_path("testdata", "test_import_03.csv")))
  is.date <- function(x) inherits(x, 'Date')
  expect_equal(is.date(df_types$DATUM), TRUE)
  expect_type(df_types$STOPNJA, "character")
  expect_type(df_types$ST_DEJAVNOSTI , "character")
  expect_type(df_types$OSNOVA_DAVKA, "double")
  expect_type(df_types$STEVILO, "double")
  expect_type(df_types$ZNESEK, "double")
  expect_type(df_types$ZNESEK_DAVKA, "double")
})

test_that("only legal values are in coded columns", {
 clean_rates <- remove_xrates(fix_types(read_file(
   test_path("testdata", "test_import_03.csv"))))
 expect_equal(nrow(clean_rates), 15)
 clean_skd <- remove_xskd(fix_types(read_file(
   test_path("testdata", "test_import_03.csv"))))
 expect_equal(nrow(clean_skd), 13)
 clean_codes <- remove_xskd(remove_xrates(fix_types(read_file(
   test_path("testdata", "test_import_03.csv")))))
 expect_equal(nrow(clean_codes), 10)
 clean_df <- remove_na_rows(remove_xskd(remove_xrates(fix_types(read_file(
   test_path("testdata", "test_import_03.csv"))))))
 expect_equal(nrow(clean_df), 5)
 })

