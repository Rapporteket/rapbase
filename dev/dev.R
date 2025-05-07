
source("dev/renv.R")

Sys.setenv(MYSQL_DB_DATA = "nakke")

Sys.setenv(FALK_EXTENDED_USER_RIGHTS = paste0(
  '[{\"A\":80,\"R\":\"SC\",\"U\":4219765},',
  '{\"A\":80,\"R\":\"LU\",\"U\":4219765},',
  '{\"A\":80,\"R\":\"LC\",\"U\":4219765},',
  '{\"A\":80,\"R\":\"LC\",\"U\":700328}]"'
)
)
Sys.setenv(FALK_APP_ID = "80")
Sys.setenv(FALK_USER_EMAIL = "jesus@sky.no")
Sys.setenv(FALK_USER_FULLNAME = "Arnie")

rapbase::exportApp(registryName = "nakke")
