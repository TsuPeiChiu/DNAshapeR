#2015 - Federico Comoglio, D-BSSE, ETH Zurich

#' Parse output files (internal)
#'
#' @usage parseShape( filename)
#'
#' @param filename DESCR
#'
#' @return Matrix
#' @note None.
#'
#' @author Federico Comoglio
#'
#' @keywords core
#'
#' @export parseShape

parseShape <- function( filename ) {
	
	#extract single record, count entries per record, delete tmp file
	#cmd <- paste( 'sed -n \'2,/>/p\'', filename, '> tmpsed.txt' )
	#system( cmd )
	#expLen <- length( strsplit( paste( readLines( 'tmpsed.txt' ), collapse = ',' ), ',' )[[ 1 ]] ) - 1
	#cmd <- 'rm tmpsed.txt'
	#system( cmd )
		
	#read file and parse
	records <- scan( filename, what = 'character' )	
	recordStart <- grep( '>', records )
	
	tmp <- paste( records[ ( recordStart[ 1 ] + 1) : (recordStart[ 2 ] - 1) ], collapse = ',')
	expLen <- length( strsplit(tmp, ',')[[1]] )
	message( 'Record length: ', expLen)
	
	if( length( recordStart ) > 1 ) { #multiple records
		diffrs <- diff( recordStart )
		d <- c( diffrs, diffrs[ 1 ] )
		indicator <- rep( 1 : length( recordStart ), times = d )
	} else { #single record
		indicator <- 1
	}
	
	records <- split( records, indicator )
	records <- lapply( records, function( x ) x[-1])
	suppressWarnings( records <-  lapply( records, function(x) as.numeric( unlist( strsplit( x, ',' ) ) ) ) )
	remove <- which( sapply(records, length) < expLen )
	if( length( remove ) > 0)
		records <- records[ -remove ]
	shapeMatrix <- do.call( 'rbind', records )
	return( shapeMatrix )
}