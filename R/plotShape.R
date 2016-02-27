#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DNAshapeR
# 2015
# Tsu-Pei Chiu, Rohs Lab, USC
# Federico Comoglio, Green lab, CIMR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot metaprofiles of DNA shape features
#'
#' DNA shape features can be visualized as
#' aggregated line plots (also known as metaprofiles, see Comoglio et al., 2015), heat maps (Yang et al., 2014) and
#' genome browser tracks (Chiu et al., 2014).
#'
#' @usage plotShape(shapeMatrix, background = NULL,
#' colDots = rgb( 0, 0, 1, 0.1),
#' colDotsBg = rgb( 0, 0, 0, 0.1),
#' colLine = 'steelblue', colLineBg = 'gray50', cex = 0.5, lwd = 2, ylim, ...)
#'
#' @param shapeMatrix A matrix containing DNAshape prediction results
#' @param background A matrix containing DNAshape prediction results for a set of background regions. Default to NULL, i.e. background not provided.
#' @param colDots A character vector specifying the color of the points representing the column mean of shapeMatrix. Default to rgb( 0, 0, 1, 0.1).
#' @param colDotsBg A character vector specifying the color of the points representing the column mean of background. Default to rgb( 0, 0, 0, 0.1).
#' @param colLine A character string giving the color name of line representing the column mean of shapeMatrix. Default to 'steelblue'.
#' @param colLineBg A character string giving the color name of line representing the column mean of background. Default to 'gray50'.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default. Default to 0.5.
#' @param lwd A numerical value specifying the line width. Default to 2.
#' @param ylim A numerical vector of size 2 specifying the y-axis plot range.
#' @param ... Additional parameters to be passed to the R plot function.
#'
#' @return Called for its effects
#'
#' @author Federico Comoglio
#'
#' @keywords core
#'
#' @examples
#' fn <- system.file("extdata", "CGRsample.fa", package = "DNAshapeR")
#' pred <- getShape(fn)
#' plotShape(pred$MGW)
#' plotShape(pred$ProT)
#' plotShape(pred$Roll)
#' plotShape(pred$HelT)
#' @export plotShape

plotShape <- function( shapeMatrix, background = NULL,
                        colDots = rgb( 0, 0, 1, 0.1),
                        colDotsBg = rgb( 0, 0, 0, 0.1),
                        colLine = 'steelblue',
                        colLineBg = 'gray50', cex = 0.5, lwd = 2, ylim, ... ) {
    n <- nrow( shapeMatrix )
    mu <- colMeans( shapeMatrix, na.rm = TRUE )
    m <- length( mu )
    span <- round( m / 2)

    if( is.null( background ) ) {
        if( missing( ylim ) )
			ylim <- range( mu, na.rm = TRUE )

        plot( mu,
            col = colDots,
            pch = 19,
            cex = cex,
            xaxt = 'n',
            xlab = '',
            ylab = paste0( 'Mean value (n=', n, ')'),
            ylim = ylim,
            ... )
        axis( 1, at = c( 0, span, m ),
          labels = c( -span, 'Center', paste0( '+', span ) ) )
          abline( v = span, lty = 2, col = 'gray30' )
          lines( lowess( mu, f = 1/10 ), col = colLine, lwd = lwd )
    }

    else { #shape of random sample is provided
        mu1 <- mu
        mu2 <- colMeans( background, na.rm = TRUE )

        if( missing( ylim ) )
			ylim <- range( mu1, mu2, na.rm = TRUE )

        plot( mu1,
            col = colDots,
            pch = 19,
            cex = cex,
            xaxt = 'n',
            xlab = '',
            ylab = paste0( 'Mean value (n=', n, ')'),
            ylim = ylim,
            ... )

        points( mu2, pch = 19, cex = cex, col = colDotsBg )
        axis( 1, at = c( 0, span, m ),
            labels = c( -span, 'Center', paste0( '+', span ) ) )
        abline( v = span, lty = 2, col = 'gray30' )
        lines( lowess( mu1, f = 1/10 ), col = colLine, lwd = lwd )
        lines( lowess( mu2, f = 1/10 ), col = colLineBg, lwd = lwd )
    }
}

#' Plot heatmap of DNA shape features
#'
#' @usage heatShape(shapeMatrix, nBins, ordRow = NULL, useRaster = TRUE, ... )
#'
#' @param shapeMatrix A matrix containing DNAshape prediction results.
#' @param nBins An integer specifying the number of equally-sized bins in which shape predictions should be aggregated. Summarized predictions can be visualized by setting nBins=1.
#' @param ordRow A numeric vector (of the same length as the number of rows of shapeMatrix) defining the permutation of the rows of shapeMatrix to be used for plotting. Default to NULL, i.e. rows are ordered by coefficients of variation.
#' @param useRaster Logical, if TRUE a bitmap raster is used to plot the image instead of polygons (see ?graphics::image for details).
#' @param ... Additional parameters to be passed to the image.plot function (see ?fields::image.plot for details).
#'
#' @return Called for its effects
#'
#' @author Federico Comoglio
#'
#' @keywords core
#'
#' @examples
#' fn <- system.file("extdata", "CGRsample.fa", package = "DNAshapeR")
#' pred <- getShape(fn)
#' library(fields)
#' heatShape(pred$MGW, 20)
#' @export heatShape

heatShape <- function( shapeMatrix, nBins, ordRow = NULL,
                        useRaster = TRUE, ... ) {
    nc <- ncol( shapeMatrix )
    nr <- nrow( shapeMatrix )

    if( (nc %% nBins) != 0 )
        stop( 'The number of bases must be a multiple of the number of bins.' )

    d <- nc / nBins
    by <- rep( 1 : nBins, each = d * nr )
    matByBins <- split( shapeMatrix, by)
    matByBins <- lapply( matByBins,
                    function(x) matrix( x, ncol = d, byrow = FALSE ) )
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

    image.plot( t( bins[ rev( ordRow ), ] ), axes = FALSE, col = pal,
        legend.shrink = 0.2, useRaster = useRaster, ... )
    abline(v = 0.5, lwd = 2, lty = 2, col = "white")
    axis( 1, at = c( 0, 0.5, 1 ),
        labels = c( -span, 'Center', paste0( '+', span ) ) )
}

# 2015 - Tsu-Pei Chiu, Rohs Lab, USC
#'
#' Plot track view of DNA shape features
#'
#' @usage trackShape( filename, shapeList )
#'
#' @param filename The name of the input fasta format file, including
#' full path to file if it is located outside the current working directory
#' @param shapeList A list containing four DNAshape prediction results
#'
#' @return Called for its effects
#'
#' @note None.
#'
#' @author Tsu-Pei Chiu
#'
#' @keywords core
#'
#' @examples
#' fn2 <- system.file("extdata", "SingleSeqsample.fa", package = "DNAshapeR")
#' pred2 <- getShape(fn2)
#' trackShape(fn2, pred2) # Only for single sequence file
#' @export trackShape

trackShape <- function( filename, shapeList ) {

    records <- scan( filename, what = 'character' )
    recordStart <- grep( '>', records )

    if( length( recordStart ) > 1 ) { #multiple records
        stop("Only one sequence file is allowed!")

    }else{
        seq <- unlist(strsplit(scan(filename,
            what="character", sep="" , comment.char = ">" ), ""))

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

        pos = seq(1, length(seq), 1)
        pos2 = pos+0.5


        par( mfrow = c( 4, 1), oma=c(2,2,2,2), mar=c(4,5,2,2),
            cex.axis = 1.25, cex.lab = 1.25)

        barplot(mydf$mgw, space = 0, col = "#F59547", border = NA,
            names.arg = seq, ylab = 'MGW (angstrom)',
                cex.names = 1.2, offset = 2.5 )
        barplot(mydf$prot, space = 0, col = "#99BA5B", border = NA,
            names.arg = seq, ylab = 'ProT (degree)',
                cex.names = 1.2 )

        rollP <- barplot(mydf$roll, space = 0, col = "#A1ACE6", border = NA,
            ylab = 'Roll (degree)', cex.names = 1.2 )
                axis(1, at=rollP-(rollP[2]-rollP[1])/2,las=2,
                labels=seq, tick = FALSE, las=0)

        helTP <- barplot(mydf$helt, space = 0, col = "#7F64A1", border = NA,
                ylab = 'HelT (degree)', cex.names = 1.2, offset = 30 )
        axis(1, at=helTP-(helTP[2]-helTP[1])/2,las=2,
                labels=seq, tick = FALSE, las=0)
    }
}
