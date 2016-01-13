#' Finn ut om dato(er) er helligdag
#' 
#' Denne funksjonen gjør ting
#' 
#' @param dato Enkeltstående dato eller vektor av datoer
#' 
#' @return erHelligdag (vektor med) logisk indikator som angir om dato er helligdag
#' 
#' @export

FinnHelligdag <- function(dato) {
  
  erHelligdag <- dato %in% Helligdager2008til2022
  
  return(invisible(erHelligdag))
  
}