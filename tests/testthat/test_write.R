# with_mock_db(
#   test_that("db connection works as does writing", {
#     df <- write_to_db(ddv_transform(ddv_import(test_path("testdata", "test_import_01.csv"))))
#     expect_equal(dim(df), c(1,1))
#   })
# )
#
#
# with_mock_db(
#   test_that("db connection works as does writing", {
#     df <- ddv_transform(ddv_import(test_path("testdata", "test_import_01.csv")))
#     diff <- ballpark_last_week(df)
#     expect_equal(diff, 0.02)
#   })
# )
