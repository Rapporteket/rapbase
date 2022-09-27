accesstree <- function(unit,
                       what,
                       file = NULL,
                       path = Sys.getenv("R_RAP_CONFIG_PATH")) {

  conf <- getConfig(fileName = "rapbaseConfig.yml")$accesstree

  if (is.null(file)) {
    file <- conf$file
  }

  stopifnot(file.exists(file.path(path, file)))
  if (!what %in% names(conf$list)) {
    stop(
      paste0(
        "Argument what is not one of '",
        paste0(names(conf$list), collapse = "', '"),
        "'"
      )
    )
  }

  d <- jsonlite::read_json(file.path(path, file)) %>%
    unlist()

  as.vector(
    d[
      names(d) == conf$list[[what]]
    ][as.vector(d[names(d) == conf$list$unit]) == unit]
  )
}
