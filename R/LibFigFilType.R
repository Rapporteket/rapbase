#' Funksjon som angir hvilket format en figur skal lagres i.
#'
#' Formatet er spesifisert gjennom endinga til filnavnet
#' Tillatt: png, pdf, jpg, bmp, tif, svg og ps
#' Velge farger: BlaaRapp, BlaaHNpms287, GronnHNpms624, GronnHNpms342
#' 
#' @param outfile Navn på fil som skal skrives til. Tom tekststreng skriver til skjerm
#' @param res Oppløsningen på raster-output
#' @param width Bredde på output ved gitt oppløsning
#' @param height Høyde på output ved gitt oppløsning
#' @param pointsizePDF Skrifstørrelse i pdf-output 
#' @param fargepalett Hvilken fargepalett skal brukes
#' @return UtFigFil List med en del figurparametre
#' @keywords rapporteket norgast
#' @export

figtype <- function(outfile='', width=3*595, height=3*595, res=3*72, pointsizePDF=11, 
                    fargepalett='BlaaOff'){
  
  filtype <- substr(outfile,nchar(outfile)-2 ,nchar(outfile))
  if (substr(filtype,1,1)=='.') {filtype <- substr(filtype,2,3)}
  
  
  fonttype <- 'sans'	#windowsFont('Helvetica')
  
  switch(filtype, 
         png = png(filename = outfile, res=res, family=fonttype, width=width, height=height), #pointsize=5),
         #Helvetica, windowsFont('Helvetica')
         jpg = jpeg(filename = outfile, res=res, width=width, height=height),
         #		pdf = pdf(file = outfile, width=7*width/555, height=7*height/555, #family='arial', 
         pdf = pdf(file = outfile, width=7, height=7*height/width, family=fonttype, #family='arial', 
                   pointsize=pointsizePDF),		
         bmp = bmp(filename = outfile, res=res, width=width, height=height),
         tif = tiff(filename = outfile, res=res, width=width, height=height)
  )
  
  
  #col2rgb for å oversette hex	
  metning <- rev(c(1,0.7,0.45,0.25,0.1))*255
  #Offentliggjøringsfarger. Består av 6 blånyanser. 
  OffBlaaAlle <- rev(rgb(red=c(198, 107, 66, 33, 8, 0), green=c(219, 174, 146, 113, 69, 0), 
                         blue=c(239, 214, 198, 181, 148, 89), maxColorValue=255))	#6 nyanser av blå
  #Alle standard "offentliggjøringsfarger":
  #HEX: #c6dbef #6baed6 #4292c6 #2171b5 #084594 #000059 #FF7260 #4D4D4D #737373 #A6A6A6 #DADADA
  OffAlleFarger <- rev(rgb(red = c(198, 107, 66, 33, 8, 0, 255, 077, 115, 166, 204), 
                           green = c(219, 174, 146, 113, 69, 0, 114, 077, 115, 166, 204), 
                           blue = c(239, 214, 198, 181, 148, 89, 096, 077, 115, 166, 204), 
                           maxColorValue=255))
  UtFarger <- switch(fargepalett, 
                     BlaaRapp = rgb(red=c(0,86,149,218), green=c(79,139,189,230), blue=c(158,191,230,242), maxColorValue=255),
                     #hsv(7/12, s=c(1,0.55,0.35,0.1,0), v=c(0.62,0.75,0.9,0.95,1)),
                     #BlaaOff = OffBlaaAlle[c(2,3,5,6)],		#Velger farge nummer 1,2,4 og 5
                     BlaaOff = OffAlleFarger[c(2,3,5,6)],	
                     BlaaOffAlle = OffAlleFarger[1:6],
                     StotteOff = rgb(red=c(255,77,115,166,204), green=c(114,77,115,166,204), 
                                     blue=c(96,77,115,166,204), maxColorValue=255),	#rød, 4 grånyanser)
                     OffAlleFarger = OffAlleFarger,
                     BlaaHNpms287 = rgb(red=0,green=50,blue=131, maxColorValue=255, alpha=metning),
                     GronnHNpms624 = rgb(red=131,green=156,blue=143, maxColorValue=255, alpha=metning),
                     GronnHNpms342 = rgb(red=47,green=101,blue=74, maxColorValue=255, alpha=metning)
  )
  
  #NB: Mørke farger vil dekke over lysere når bruker samme farge med ulik metning!
  
  UtFigFil <- list(res, width, height, UtFarger)
  names(UtFigFil) <- c('res', 'width', 'height', 'farger')
  return(invisible(UtFigFil))
}
