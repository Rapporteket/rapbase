# How to create the testdata.rda file

testdata <- data.frame(
  id = 1:10,
  someText = paste0("text", 1:10),
  someInt = 11:20,
  someBigInt = 123456789 * 1:10,
  someFloat = 123 / 1:10,
  someTime = as.POSIXct(
    paste0("2022-03-24 ", sprintf("%02d", 1:10), ":00:00"),
    tz = "UTC"
  ),
  stringsAsFactors = FALSE
)
usethis::use_data(testdata, overwrite = TRUE)

