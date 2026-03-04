## code to prepare `appLog` dataset goes here
time <- c(
  "2021-07-22 13:30:22", "2021-07-22 10:44:15", "2021-07-19 14:00:53",
  "2021-07-22 15:15:21", "2021-07-22 10:07:46",
  "2021-07-27 10:14:05", "2021-07-23 14:31:31", "2021-07-23 11:22:39",
  "2021-07-23 11:06:39", "2021-07-19 13:43:50",
  "2021-07-23 14:24:21", "2021-07-22 13:32:29", "2021-07-29 14:49:55",
  "2021-07-19 14:05:48", "2021-07-22 15:05:45",
  "2021-07-23 13:40:31", "2021-07-19 10:23:09", "2021-07-27 12:35:59",
  "2021-07-19 11:39:56", "2021-07-26 13:20:52"
)

user <- "ttester"
name <- "Tore Tester"
group <- "rapbase"
role <- "tester"
resh_id <- 999999
message <- "Staring test application"

appLog <- data.frame(
  time = time, user = user, name = name, group = group,
  role = role, resh_id = resh_id, message = message,
  stringsAsFactors = FALSE
) |> dplyr::mutate(id = as.character(dplyr::row_number()))

usethis::use_data(appLog, overwrite = TRUE)
