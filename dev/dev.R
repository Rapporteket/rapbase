source("dev/renv.R")

source("../../felles/renv.R")
Sys.setenv(RUN_LATEX = "false")


devtools::test()

rapbase::exportApp(teamName = "nord", dbName = "db_log", logAsJson = FALSE)

rapbase::statsApp()
