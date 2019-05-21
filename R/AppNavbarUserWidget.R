appNavbarUserWidget <- function(session = NULL) {
  
  txtWidget <- paste0("var header = $('.navbar> .container-fluid');\n",
                      "header.append('<div class=\"navbar-brand\" style=\"float:right\">",
                      getUserName(session), "<br>",
                      getUserReshId(session), "<br>",
                      "<a href=\"https://www.w3schools.com\">Logg ut</a>",
                      "</div>');\n",
                      "console.log(header)")
  
  shiny::tags$script(HTML(txtWidget))
}