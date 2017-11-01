#' Provide simple formatting of a sequential message
#' 
#' Wrap strings into a message for simple presentation
#' 
#' @param story String Text that the message is to be added to
#' @param message String Message to add
#' @param newLine Logical [TRUE] Start message at a new line. TRUE by default
#' @param newPara Logical [FALSE] Start message at new paragraph. Default FALSE
#' @param timeStamp Logical [TRUE] Add a time stamp to the message. TRUE by
#' default
#' @return story String The whole story...
#' @export


MakeMessage <- function(story, message, newLine = TRUE, newPara = FALSE,
                        timeStamp = TRUE) {
  
  if (timeStamp) {
    message <- paste(format(Sys.time(), "%F %H:%M:%S"), message)
  }
  
  if (newLine & !newPara) {
    message <- paste0("\n", message)
  }
  
  if (newPara) {
    message <- paste0("\n\n", message)
  }
  
  story <- paste(story, message)
  story
}