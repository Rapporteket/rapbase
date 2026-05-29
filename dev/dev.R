
source("dev/renv.R")
devtools::test()

Sys.setenv(MYSQL_HOST = "db")
rapbase::exportApp(teamName = "nord", dbName = "raplog", logAsJson = FALSE)

rapbase::statsApp()
