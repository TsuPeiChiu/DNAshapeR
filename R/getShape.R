#2015 - Tsu-Pei Chiu, Rohs Lab, USC & Federico Comoglio, D-BSSE, ETH Zurich


#' Predict DNA shape from a FASTA file
#'
#' The DNA prediction uses a sliding pentamer window where structural features
#' unique to each of the 512 distinct pentamers define a vector of minor
#' groove width (MGW), Roll, propeller twist (ProT), and helix twist (HelT) at
#' each nucleotide position (Zhou, et al., 2013). MGW and ProT define
#' base-pair parameter whereas Roll and HelT represent base pair-step
#' parameters. The values for each DNA shape feature as function of its
#' pentamer sequence were derived from all-atom Monte Carlo simulations where
#' DNA structure is sampled in collective and internal degrees of freedom in
#' combination with explicit counter ions (Zhang, et al., 2014). The Monte
#' Carlo simulations were analyzed with a modified Curves approach
#' (Zhou, et al., 2013). Through data mining, average values for each shape
#' feature were calculated for the on average 44 occurrences of each pentamer
#' in an ensemble of Monte Carlo trajectories for 2,121 DNA fragments of 12-27
#' base pairs in length. DNAshapeR predicts four DNA shape features, which were
#' observed in various co-crystal structures playing an important role in
#' specific protein-DNA binding. The core prediction algorithm enables
#' ultra-fast, high-throughput predictions of shape features for thousands of
#' genomic sequences and is implemented in C++. Since it is likely that
#' features describing additional structural properties or equivalent features
#' derived from different experimental or computational sources will become
#' available, the package has a flexible modular design that easily allows
#' future expansions.

#'
#' @usage getShape(filename, shapeType = 'All', parse = TRUE)
#'
#' @param filename The Name of the input fasta format file, including
#' full path to file if it is located outside the current working directory.
#' @param shapeType A character indicating the shape parameters which can be
#' "MGW", "ProT", "Roll", "HelT" or "All" (meaning all four shapes)
#' @param parse A logical value indicating whether parse the prediction
#' result
#'
#' @return shapeList A List containing shapre prediction result
#' @author Federico Comoglio & Tsu-Pei Chiu
#' @keywords core
#' #' @examples
#'
#' library(DNAshapeR)
#' fn <- system.file("extdata", "CGRsample.fa", package = "DNAshapeR")
#' pred <- getShape(fn)
#'
#' @export getShape

getShape <- function(filename, shapeType = 'All', parse = TRUE) {
# Run DNAshape prediction and store the result in a List
#
# Args:
#   filename: character, the filename and/or path to it
#   shapeType: character, the type of shape parameter of interest
#
# Returns:
#   a list of shape prediction result
#
# Error handling
#   ...
    opts <- c( 'MGW', 'HelT', 'ProT', 'Roll' )
    stopifnot( shapeType %in% c( opts, 'All' ) )

    if( shapeType != 'All' ) {
        getDNAShape(filename, 'MGW')

    } else {
        getDNAShape(filename, 'MGW')
        getDNAShape(filename, 'HelT')
        getDNAShape(filename, 'ProT')
        getDNAShape(filename, 'Roll')
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

