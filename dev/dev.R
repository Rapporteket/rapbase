source("dev/renv.R")

# Hvis man er inne i docker-compose-miljøet (ikke på HN-IKT-windows-maskin).
Sys.setenv(MYSQL_HOST = "db")
Sys.setenv(RUN_MYSQLDUMP = "true")

devtools::test()

rapbase::exportApp(teamName = "nord", dbName = "raplog", logAsJson = FALSE)

rapbase::statsApp()
