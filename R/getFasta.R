#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DNAshapeR
# 2015
# Tsu-Pei Chiu, Rohs Lab, USC
# Federico Comoglio, Green lab, CIMR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Extract fasta sequence given a set of genomic intervals and a reference
#' genome.
#'
#' DNAshapeR can predict DNA shape features from custom FASTA files or directly
#' from genomic coordinates in the form of a GRanges object within BioConductor
#' (see
#' <https://bioconductor.org/packages/release/bioc/html/GenomicRanges.html>
#' for more information).
#'
#' @usage getFasta(GR, BSgenome, width = 1e3, filename = 'tmp.fa')
#'
#' @param GR A GRanges object indicating genomic coordinates
#' @param BSgenome A BSgenome object indicating the genome of interest
#' @param width A number indicating a fixed width of sequences
#' @param filename The Name of the input fasta format file, including
#' full path to file if it is located outside the current working directory
#' @return writes a fasta file
#'
#' @author Federico Comoglio
#'
#' @keywords core
#'
#' @examples
#' gr <- GRanges(seqnames = c("chrI"),
#' strand = c("+", "-", "+"),
#' ranges = IRanges(start = c(100, 200, 300), width = 100))
#' library(BSgenome.Scerevisiae.UCSC.sacCer3)
#' getFasta(gr, BSgenome = Scerevisiae, width = 100, filename = "tmp.fa")
#' fn <- "tmp.fa"
#' pred <- getShape(fn)
#' @export getFasta

getFasta <- function( GR, BSgenome, width = 1e3, filename = 'tmp.fa' ) {
    GR <- resize( GR, width = width, fix = 'center' )
    seqlengths <- NULL
    `seqlengths<-`<- NULL
    seqlengths( GR ) <- seqlengths( BSgenome )[ names( seqlengths( GR ) ) ]
    GR <- trim( GR )

    idx <- which( width( GR ) < width )
    if( length( idx ) > 0 )
        GR <- GR[ -idx ]
    message( 'Removed sequences #: ', list( idx ))

    sequences <- getSeq( BSgenome, names = GR )
    names( sequences ) <- paste0( 'seq', 1 : length(GR) )
    writeXStringSet(sequences, filepath = filename, format = "fasta")
}
