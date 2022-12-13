#' Send email from Rapporteket
#'
#' This function can be used to send email from within R at Rapporteket. It
#' relies on (and must hence be provided) specific settings through local
#' configuration to work properly.
#'
#' @param conf List containing (Rapporteket) config such as sender email
#' address, SMTP server url and port number
#' @param to Character vector containing email addresses. May also contain
#' full names like '\code{Jane Doe <janed@nowhere.com>}'
#' @param subject Character string providing email subject.
#' @param text Character string providing the plain email text
#' @param attFile Character string providing the full file path to an
#' attachment. Default is NULL in which case no attachment is made
#'
#' @return Invisible sending of email
#' @export

sendEmail <- function(conf, to, subject, text, attFile = NULL) {
  if (!is.null(attFile)) {
    stopifnot(file.exists(attFile))
  }

  # nocov start

  from <- conf$network$sender
  # escape spaces (e.g. when full name is added to <email>)
  to <- gsub(" ", "\\ ", to, fixed = TRUE)

  if (is.null(attFile)) {
    body <- list(text)
  } else {
    body <- list(text, sendmailR::mime_part(attFile))
  }

  sendmailR::sendmail(
    from, to, subject, body,
    control = list(
      smtpServer = conf$network$smtp$server,
      smtpPortSMTP = conf$network$smtp$port
    )
  )

  invisible()
  # nocov end
}
