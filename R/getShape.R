#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DNAshapeR
# 2015
# Tsu-Pei Chiu, Rohs Lab, USC
# Federico Comoglio, Green lab, CIMR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
#'
#' @author Federico Comoglio & Tsu-Pei Chiu
#'
#' @keywords core
#'
#' @examples
#' fn <- system.file("extdata", "CGRsample.fa", package = "DNAshapeR")
#' pred <- getShape(fn)
#' @export getShape

getShape <- function(filename, shapeType = 'All', parse = TRUE) {

    opts <- c( 'MGW', 'HelT', 'ProT', 'Roll' )
    stopifnot( shapeType %in% c( opts, 'All' ) )

    if( shapeType != 'All' ) {
        getDNAShape(filename, shapeType)

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
            shapeList <- lapply( ln, readShape )
            names( shapeList ) <- opts
        } else {
            ln <- paste0( filename, '.', shapeType )
            shapeList <- list( readShape( ln ) )
            names( shapeList ) <- shapeType
        }
        message( 'Done' )
        return( shapeList )
    }
}

#' Read (parse) DNA shape predictions
#'
#' Read DNA shape predictions
#'
#' @usage readShape(filename)
#'
#' @param filename character name of the file containing shape predictions, including
#' full path to file if it is located outside the current working directory.
#'
#' @return shapeMatrix matrix containing the shape prediction result
#'
#' @author Federico Comoglio & Tsu-Pei Chiu
#'
#' @keywords core
#'
#' @examples
#' fn <- system.file("extdata", "CGRsample.fa", package = "DNAshapeR")
#' pred <- readShape(fn)
#' @export readShape

readShape <- function( filename ) {

    #read file and parse
    records <- scan( filename, what = 'character' )
    recordStart <- grep( '>', records )

    if( length( recordStart ) > 1 ) { #multiple records
        tmp <- paste( records[ ( recordStart[ 1 ] + 1) :
            (recordStart[ 2 ] - 1) ],
                collapse = ',')
    } else { #single record
        tmp <- paste( records[ ( recordStart[ 1 ] + 1) :
            length(records) ],
                collapse = ',')
    }

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
    suppressWarnings( records <-  lapply( records,
            function(x) as.numeric( unlist( strsplit( x, ',' ) ) ) ) )
    remove <- which( sapply(records, length) < expLen )
    if( length( remove ) > 0)
        records <- records[ -remove ]
    shapeMatrix <- do.call( 'rbind', records )
    return( shapeMatrix )
}
