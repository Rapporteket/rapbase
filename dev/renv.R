# Environment variables for testing

Sys.setenv(MYSQL_USER = "root")
Sys.setenv(MYSQL_PASSWORD = "root")
Sys.setenv(MYSQL_HOST = "localhost")
Sys.setenv(R_RAP_INSTANCE = "DEV")
Sys.setenv(RUN_DB_UNIT_TESTS = "true")
Sys.setenv(FALK_EXTENDED_USER_RIGHTS = "[{\"A\":99,\"R\":\"SC\",\"U\":0}]")
Sys.setenv(FALK_APP_ID="99")
