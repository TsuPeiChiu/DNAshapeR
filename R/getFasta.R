#2015 - Federico Comoglio & Tsu-Pei Chiu

#' Extract fasta sequence given a set of genomic intervals and a reference genome.
#' 
#' Extract fasta sequence given a set of genomic intervals and a reference genome. Intervals are resized to a fixed width.
#' 
#' @usage getFasta( GR, BSgenome, width = 1e3, filename = 'tmp.fa' )
#' @param GR
#' @param BSgenome
#' @param width
#' @param filename
#' @return writes a fasta file
#'
#' @note None
#'
#' @author Federico Comoglio
#'
#' @seealso \code{\link{}}
#'
#' @keywords core
#'
#' @export getFasta

getFasta <- function( GR, BSgenome, width = 1e3, filename = 'tmp.fa' ) {
	GR <- resize( GR, width = width, fix = 'center' )
	seqlengths( GR ) <- seqlengths( BSgenome )[ names( seqlengths( GR ) ) ]
	GR <- trim( GR )

	idx <- which( width( GR ) < width )
	if( length( idx ) > 0 )
		GR <- GR[ -idx ]
		message( 'Removed sequences #: ', list( idx ))

	sequences <- getSeq( BSgenome, names = GR )
	names( sequences ) <- paste0( 'seq', 1 : length(GR) )
	writeXStringSet(sequences, file = filename, format = "fasta")
}