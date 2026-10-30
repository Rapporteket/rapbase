source("dev/renv.R")

source("../../felles/renv.R")

devtools::test()

rapbase::exportApp(teamName = "nord", dbName = "db_log", logAsJson = FALSE)

rapbase::statsApp()
