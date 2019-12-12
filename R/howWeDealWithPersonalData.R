#' Render text in pop-up
#' 
#' Render text on how Rapporteket deals witn personal data
#'
#' @param session A shiny session object used to personalize the text
#'
#' @return html pop-up text
#' @export

howWeDealWithPersonalData <- function(session) {
  
  . <- ""
  
  params <- list(session=session)
  system.file("howWeDealWithPersonalData.Rmd", package="rapbase") %>%
    knitr::knit(output = tempfile()) %>%
    markdown::markdownToHTML(.,
                             options = c('fragment_only',
                                         'base64_images',
                                         'highlight_code'),
                             encoding = "utf-8") %>%
    shiny::HTML()
  
}