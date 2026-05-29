# Environment variables for testing

Sys.setenv(FALK_EXTENDED_USER_RIGHTS = "[
{\"A\":80,\"R\":\"SC\",\"U\":111961},
{\"A\":80,\"R\":\"LU\",\"U\":111961},
{\"A\":81,\"R\":\"LC\",\"U\":111961},
{\"A\":80,\"R\":\"SC\",\"U\":102467},
{\"A\":80,\"R\":\"LC\",\"U\":102467},
{\"A\":81,\"R\":\"LC\",\"U\":102467},
{\"A\":80,\"R\":\"SC\",\"U\":103240},
{\"A\":80,\"R\":\"LU\",\"U\":103240},
{\"A\":81,\"R\":\"LC\",\"U\":103240} ]")
Sys.setenv(FALK_APP_ID = "80")
Sys.setenv(FALK_USER_EMAIL = "blue@sky.no")
Sys.setenv(FALK_USER_FULLNAME = "Bob")

Sys.setenv(MYSQL_USER = "root")
Sys.setenv(MYSQL_PASSWORD = "root")
Sys.setenv(MYSQL_HOST = "localhost")
Sys.setenv(MYSQL_DB_LOG = "db_log")
Sys.setenv(MYSQL_DB_AUTOREPORT="db_autoreport")
#Sys.setenv(R_RAP_INSTANCE = "DEV")
Sys.setenv(RUN_DB_UNIT_TESTS = "true")
Sys.setenv(RUN_MYSQLDUMP = "true")
