#2015 - Federico Comoglio, D-BSSE, ETH Zurich
#'
#' Plot metaprofiles of DNA shape features
#'
#' @usage plotShape(shapeMatrix, background = NULL, colDots = rgb( 0, 0, 1, 0.1), colDotsBg = rgb( 0, 0, 0, 0.1), colLine = 'steelblue', colLineBg = 'gray50', cex = 0.5, lwd = 2, ...)
#'
#' @param shapeMatrix DESCR
#' @param background DESCR
#' @param colDots DESCR
#' @param colDotsBg DESCR
#' @param colLine DESCR
#' @param colLineBg DESCR
#' @param cex DESCR
#' @param lwd DESCR
#' @param ... DESCR
#'
#' @return Called for its effects
#' @note None.
#'
#' @author Federico Comoglio
#'
#' @keywords core
#'
#' @export plotShape

plotShape <- function( shapeMatrix, background = NULL, colDots = rgb( 0, 0, 1, 0.1), colDotsBg = rgb( 0, 0, 0, 0.1), colLine = 'steelblue', colLineBg = 'gray50', cex = 0.5, lwd = 2, ... ) {
	n <- nrow( shapeMatrix )
	mu <- colMeans( shapeMatrix, na.rm = TRUE )
	m <- length( mu )
	span <- round( m / 2)
	
	if( is.null( background ) ) {	
		yrange <- range( mu, na.rm = TRUE )
	
		plot( mu, 
			  col = colDots,
			  pch = 19, 
			  cex = cex,
			  xaxt = 'n', 
		 	  xlab = '', 
		 	  ylab = paste0( 'Mean value (n=', n, ')'), 
		 	  ylim = yrange, 
		 	  ... )
		axis( 1, at = c( 0, span, m ), labels = c( -span, 'Center', paste0( '+', span ) ) )
		abline( v = span, lty = 2, col = 'gray30' )
		lines( lowess( mu, f = 1/10 ), col = colLine, lwd = lwd ) 
	 } 
	 
	 else { #shape of random sample is provided
	 	mu1 <- mu
	 	mu2 <- colMeans( background, na.rm = TRUE )
		yrange <- range( mu1, mu2, na.rm = TRUE )
		
		plot( mu1, 
			  col = colDots,
			  pch = 19, 
			  cex = cex,
			  xaxt = 'n', 
		 	  xlab = '', 
		 	  ylab = paste0( 'Mean value (n=', n, ')'), 
		 	  ylim = yrange, 
			  ... )
		
		points( mu2, pch = 19, cex = cex, col = colDotsBg )
		axis( 1, at = c( 0, span, m ), labels = c( -span, 'Center', paste0( '+', span ) ) )
		abline( v = span, lty = 2, col = 'gray30' )
		lines( lowess( mu1, f = 1/10 ), col = colLine, lwd = lwd ) 
		lines( lowess( mu2, f = 1/10 ), col = colLineBg, lwd = lwd ) 	
	 }
}

#' Plot heatmap of DNA shape features
#'
#' @usage heatShape(shapeMatrix, nBins, ordRow = NULL, useRaster = TRUE, ... )
#'
#' @param shapeMatrix DESCR
#' @param nBins DESCR
#' @param ordRow DESCR
#' @param useRaster DESCR
#' @param ... DESCR
#'
#' @return Called for its effects
#' @note None.
#'
#' @author Federico Comoglio
#'
#' @keywords core
#'
#' @export heatShape
heatShape <- function( shapeMatrix, nBins, ordRow = NULL, useRaster = TRUE, ... ) {
	nc <- ncol( shapeMatrix )
	nr <- nrow( shapeMatrix )
	
	if( (nc %% nBins) != 0 )
		stop( 'The number of bases must be a multiple of the number of bins.' )
	
	d <- nc / nBins
	by <- rep( 1 : nBins, each = d * nr )
	matByBins <- split( shapeMatrix, by)
	matByBins <- lapply( matByBins, function(x) matrix( x, ncol = d, byrow = FALSE ) )
	bins <- do.call( 'cbind', lapply( matByBins, rowMeans, na.rm = TRUE ) )
	
	if( is.null( ordRow ) ) {
		sdRow <- apply( bins, 1, sd )
    		meanRow <- rowMeans( bins )
    		cvRow <- abs( sdRow/meanRow )
    		ordRow <- order( cvRow, decreasing = FALSE )
	}
	
	palCol <- c( 'navyblue', 'white', 'red4' )
	pal <- colorRampPalette( palCol )( 64 )
	span <- round( nc / 2 )
	
	image.plot( t( bins[ rev( ordRow ), ] ), axes = FALSE, col = pal, legend.shrink = 0.2, useRaster = useRaster, ... )
    abline(v = 0.5, lwd = 2, lty = 2, col = "white")
	axis( 1, at = c( 0, 0.5, 1 ), labels = c( -span, 'Center', paste0( '+', span ) ) )
}

# 2015 - Tsu-Pei Chiu, Rohs Lab, USC
#'
#' Plot track view of DNA shape features
#' 
#' @usage trackShape( filename, shapeList )
#' 
#' @param filename DESCR
#' @param shapeList DESCR
#' 
#' @return Called for its effects
#' @note None.
#' 
#' @author Tsu-Pei Chiu
#' 
#' @keywords core
#' 
#' @export trackShape

trackShape <- function( filename, shapeList ) {
   
  seq <- unlist(strsplit(scan(filename, what="character", sep="" , comment.char = ">" ), ""))
  
  mgw <- as.vector( shapeList$MGW )
  prot <- as.vector( shapeList$ProT )
  roll <- as.vector( shapeList$Roll )
  helt <- as.vector( shapeList$HelT )
  
  mgw <- mgw-2.5
  helt <- helt-30
  
  mgw[is.na(mgw)] <- 0
  prot[is.na(prot)] <- 0
  roll[is.na(roll)] <- 0
  helt[is.na(helt)] <- 0
  
  roll <- append(roll, 0, after = length(roll)-1)
  helt <- append(helt, 0, after = length(helt)-1)
    
  mydf <- data.frame(mgw, prot, roll, helt) 
  #print(mydf)
  
  par( mfrow = c( 4, 1), oma=c(1,1,1,1), mar=c(3,4,1,1), cex.axis = 1.25, cex.lab = 1.25 )

  barplot(mydf$mgw, space = 0, col = "#F59547", border = NA, names.arg = seq, ylab = 'MGW', cex.names = 1.2 )
  barplot(mydf$prot, space = 0, col = "#99BA5B", border = NA, names.arg = seq, ylab = 'ProT', cex.names = 1.2 )
  barplot(mydf$roll, space = 0, col = "#A1ACE6", border = NA, names.arg = seq, ylab = 'Roll', cex.names = 1.2 )
  barplot(mydf$helt, space = 0, col = "#7F64A1", border = NA, names.arg = seq, ylab = 'HelT', cex.names = 1.2 )
}
