library(RPostgres)
library(dittodb)
df <- ddv_transform(ddv_import(test_path("testdata", "test_import_01.csv")))

start_db_capturing()
  write_to_db(df)
stop_db_capturing()
