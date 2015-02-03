#' Funksjon som angir hvilket format en figur skal lagres i.
#'
#' Formatet er spesifisert gjennom endinga til filnavnet
#' Tillatt: png, pdf, jpg, bmp, tif, svg og ps
#' Velge farger: BlaaRapp, BlaaHNpms287, GronnHNpms624, GronnHNpms342

#' @param outfile ??
#' @param res ??
#' @param width ??
#' @param height ??
#' @param pointsize ??
#' @param fargepalett ??
#' @keywords rapporteket norgast
#' @export

figtype <- function(outfile='', res=72, width=555, height=555, pointsize=12, fargepalett='BlaaOff'){	
UtData <- c(1,1,1)
filtype <- substr(outfile,nchar(outfile)-2 ,nchar(outfile))
if (substr(filtype,1,1)=='.') {filtype <- substr(filtype,2,3)}

switch(filtype, 
       #png = Cairo(width=width, height=height, file=outfile,
        #           type=filtype, bg="white", units='px', pointsize=pointsize, res=res),
		png = png(file = outfile, res=res, width=width, height=height), #pointsize=5),
		jpg = jpeg(file = outfile, res=res, width=width, height=height),
		pdf = pdf(file = outfile, width=7*width/555, height=7*height/555, pointsize=pointsize),
		bmp = bmp(file = outfile, res=res, width=width, height=height),
		tif = tiff(file = outfile, res=res, width=width, height=height),
		svg = Cairo(width=width, height=height, file=outfile,
                   type=filtype, bg="white", units='px', pointsize=pointsize, res=res),
		ps = Cairo(width=width, height=height, file=outfile,
                   type=filtype, bg="white", units='px', pointsize=pointsize, res=res))

# if (!filtype=='') {Cairo(width=width, height=height, file=outfile,
#                         type=filtype, bg="white", units='px', pointsize=pointsize, res=res)}

#col2rgb for å oversette hex	
metning <- rev(c(1,0.7,0.45,0.25,0.1))*255
	#Offentliggjøringsfarger. Består av 6 blånyanser. 
OffBlaaAlle <- rev(rgb(red=c(198, 107, 66, 33, 8, 0), green=c(219, 174, 146, 113, 69, 0), 
				blue=c(239, 214, 198, 181, 148, 89), max=255))	#6 nyanser av blå
#OffBlla1 <- c(198, 219, 239)/255
#OffBlla2 <- c(107, 174, 214)/255
#OffBlla3 <- c( 66, 146, 198)/255
#OffBlla4 <- c( 33, 113, 181)/255
#OffBlla5 <- c(  8,  69, 148)/255
#OffBlla6 <- c(  0,   0,  89)/255

				
UtFarger <- switch(fargepalett, 
	BlaaRapp = rgb(red=c(0,86,149,218), green=c(79,139,189,230), blue=c(158,191,230,242), max=255),
			#hsv(7/12, s=c(1,0.55,0.35,0.1,0), v=c(0.62,0.75,0.9,0.95,1)),
	BlaaOff = OffBlaaAlle[c(2,3,5,6)],		#Velger farge nummer 1,2,4 og 5
	StotteOff = rgb(red=c(255,77,115,166,204), green=c(114,77,115,166,204), 
				blue=c(96,77,115,166,204), max=255),	#rød, 4 grånyanser)
	BlaaHNpms287 = rgb(red=0,green=50,blue=131, max=255, alpha=metning),
	GronnHNpms624 = rgb(red=131,green=156,blue=143, max=255, alpha=metning),
	GronnHNpms342 = rgb(red=47,green=101,blue=74, max=255, alpha=metning)
)

#NB: Mørke farger vil dekke over lysere når bruker samme farge med ulik metning!

UtFigFil <- list(res, width, height, UtFarger)
names(UtFigFil) <- c('res', 'width', 'height', 'farger')
return(invisible(UtFigFil))
}

