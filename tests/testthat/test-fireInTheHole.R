currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")
currentContext <- Sys.getenv("R_RAP_INSTANCE")
tempdir <- tempdir()
Sys.setenv(R_RAP_CONFIG_PATH = tempdir)
Sys.setenv(R_RAP_INSTANCE = "")
file.copy(
  system.file(c("rapbaseConfig.yml", "autoReport.yml"),
    package = "rapbase"
  ),
  tempdir
)


# Restore env
Sys.setenv(R_RAP_INSTANCE = currentContext)
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
