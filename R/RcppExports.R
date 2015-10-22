#2015 - Federico Comoglio, D-BSSE, ETH Zurich
#' Predict DNA shape from FASTA
#'
#' @usage getShape(filename, shapeType = 'All', parse = TRUE)
#'
#' @param filename DESCR
#' @param shapeType DESCR
#' @param parse DESCR
#'
#' @return txt file with shape predictions in the working directory
#' @note None.
#'
#' @author Federico Comoglio
#'
#' @keywords core
#'
#' @export getShape

getShape <- function(filename, shapeType = 'All', parse = TRUE) {
  opts <- c( 'MGW', 'HelT', 'ProT', 'Roll' )
  stopifnot( shapeType %in% c( opts, 'All' ) )

  if( shapeType != 'All' ) {
    .Call('DNAshapeR_getShape', PACKAGE = 'DNAshapeR', filename, shapeType)
  } else {
    .Call('DNAshapeR_getShape', PACKAGE = 'DNAshapeR', filename, shapeType = 'MGW' )
    .Call('DNAshapeR_getShape', PACKAGE = 'DNAshapeR', filename, shapeType = 'HelT' )
    .Call('DNAshapeR_getShape', PACKAGE = 'DNAshapeR', filename, shapeType = 'ProT' )
    .Call('DNAshapeR_getShape', PACKAGE = 'DNAshapeR', filename, shapeType = 'Roll' )
  }

  if( parse ) {
    message( 'Parsing files......' )
    if( shapeType == 'All' ) {
      ln <- paste0( filename, '.', opts )
      shapeList <- lapply( ln, parseShape )
      names( shapeList ) <- opts
    } else {
      ln <- paste0( filename, '.', shapeType )
      shapeList <- list( parseShape( ln ) )
      names( shapeList ) <- shapeType
    }
    message( 'Done' )
    return( shapeList )
  }
}

