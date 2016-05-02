#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DNAshapeR
# 2015
# Tsu-Pei Chiu, Rohs Lab, USC
# Federico Comoglio, Green lab, CIMR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Encode k-mer DNA sequence and n-th order DNA Shape features
#'
#' DNAshapeR can be used to generate feature vectors for a user-defined model.
#' These models can be based on DNA sequence (1-mer, 2-mer, 3-mer) or DNA
#' shape (MGW, Roll, ProT, HelT) features or any combination thereof. Sequence
#' is encoded as four binary features (i.e., 0001 for adenine, 0010 for
#' cytosine, 0100 for guanine, and 1000 for thymine, for encoding of 1-mers)
#' at each nucleotide position (Zhou, et al., 2015). Encoding of 2-mers and
#' 3-mers (16 and 64 binary features at each position, respectively) is also
#' supported. Shape features include first and second order (or higher order)
#' values for the four structural parameters MGW, Roll, ProT and HelT. The
#' second order shape features are product terms of values for the same
#' category of shape features at adjacent positions. The function allows to
#' generate any subset of these features, e.g. a given shape category or first
#' order shape features, and any desired combination of shape and sequence
#' features. Feature encoding returns a feature matrix for a dataset of
#' multiple sequences, in which each sequence generates a concatenated feature
#' vector. The output of this function can be used directly for any statistical
#' machine learning method.
#'
#'
#' @usage encodeSeqShape(fastaFileName, shapeMatrix, featureNames, normalize)
#'
#' @param fastaFileName A character name of the input fasta format file,
#' including full path to file if it is located outside the current working
#' directory.
#' @param shapeMatrix A matrix containing DNAshape prediction result
#' @param featureNames A vector containing a combination of user-defined
#' sequence and shape parameters. The parameters can be any combination of
#' "k-mer", "n-shape", "n-MGW", "n-ProT", "n-Roll", "n-HelT" (k, n are
#' integers)
#' @param normalize A logical indicating whether to perform
#' normalization. Default to TRUE.
#' @return featureVector A matrix containing encoded features. Sequence
#' features are represented as binary numbers, while shape features are
#' represented as real numbers.
#'
#' @author Tsu-Pei Chiu
#'
#' @keywords core
#'
#' @examples
#' fn <- system.file("extdata", "CGRsample_short.fa", package = "DNAshapeR")
#' pred <- getShape(fn)
#' featureNames <- c("1-shape")
#' featureVector <- encodeSeqShape(fn, pred, featureNames)
#' @export encodeSeqShape

encodeSeqShape <- function( fastaFileName, shapeMatrix, featureNames, normalize = TRUE ) {
    ds <- readDNAStringSet(fastaFileName, "fasta")

    featureVector <- c()
    n <- length( featureNames )

    for( i in seq_len(n) ){
        featureName <- unlist(strsplit(featureNames[i], "-"))
        switch( featureName[2],
            mer = { featureVector <- cbind( featureVector,
                    encodeKMerSeq( as.numeric( featureName[1] ), ds ) ) },
            MGW = { featureVector <- cbind( featureVector,
                        normalizeShape(
                            encodeNstOrderShape( as.numeric( featureName[1] ),
                            shapeMatrix$MGW, "MGW" ), as.numeric( featureName[1] ),
                            "MGW" , normalize) )
            },
            ProT = { featureVector <- cbind( featureVector,
                        normalizeShape(
                            encodeNstOrderShape( as.numeric( featureName[1] ),
                            shapeMatrix$ProT, "ProT" ), as.numeric( featureName[1] ),
                            "ProT" , normalize) )
            },
            Roll = { featureVector <- cbind( featureVector,
                        normalizeShape(
                            encodeNstOrderShape( as.numeric( featureName[1] ),
                            shapeMatrix$Roll, "Roll" ), as.numeric( featureName[1] ),
                            "Roll" , normalize) )
            },

            HelT = { featureVector <- cbind( featureVector,
                        normalizeShape(
                            encodeNstOrderShape( as.numeric( featureName[1] ),
                            shapeMatrix$HelT, "HelT" ), as.numeric( featureName[1] ),
                            "HelT" , normalize) )
            },

            shape = {
                    featureVector <- cbind( featureVector,
                        normalizeShape(
                            encodeNstOrderShape( as.numeric( featureName[1] ),
                            shapeMatrix$MGW, "MGW" ), as.numeric( featureName[1] ),
                            "MGW" , normalize ),

                        normalizeShape(
                            encodeNstOrderShape( as.numeric( featureName[1] ),
                            shapeMatrix$ProT, "ProT" ), as.numeric( featureName[1] ),
                            "ProT" , normalize ),

                        normalizeShape(
                            encodeNstOrderShape( as.numeric( featureName[1] ),
                            shapeMatrix$Roll, "Roll" ), as.numeric( featureName[1] ),
                            "Roll" , normalize ),

                        normalizeShape(
                            encodeNstOrderShape( as.numeric( featureName[1] ),
                            shapeMatrix$HelT, "HelT" ), as.numeric( featureName[1] ),
                            "HelT" , normalize )
                    )
              }
        )
    }

    return (featureVector)
}


#' Encode k-mer DNA sequence features
#'
#' DNAshapeR can be used to generate feature vectors for a user-defined model.
#' The model can be a k-mer sequence. Sequence is encoded in four binary
#' features (i.e., in terms of 1-mers, 0001 for adenine, 0010 for cytosine,
#' 0100 for guanine, and 1000 for thymine) at each nucleotide position
#' (Zhou, et al., 2015). The function permits an encoding of 2-mers and 3-mers
#' (16 and 64 binary features at each position, respectively).
#'
#' @usage encodeKMerSeq(k, dnaStringSet)
#'
#' @param k A number indicating k-mer sequence encoding
#' @param dnaStringSet A DNAStringSet object of the inputted fasta file
#' @return featureVector A matrix containing encoded features. Sequence
#' feature is represented as binary numbers
#' @author Tsu-Pei Chiu

encodeKMerSeq <- function( k, dnaStringSet ){
    # create a lookup table
    lookupTable <- diag( 4**k )
    row.names( lookupTable ) <- mkAllStrings( c( "A", "C", "G", "T" ), k )

    # pre-allocate the featureVector
    l <- nchar( toString( dnaStringSet[1] ) ) # get sequence length
    n <- ( l - k + 1 ) * ( 4 ** k )
    m <- length( dnaStringSet )
    featureVector <- matrix(0L, m, n)

    for( i in 1 : length( dnaStringSet ) ){
        # encode k-mer feature
        features <- c()
        seq <- toupper( toString( dnaStringSet[i] ) )
        for ( j in 1 : ( nchar( seq )-k+1) ){

            if( is.na( match( substr( seq, j, j+k-1),
                    row.names( lookupTable ) ) ) ){
                # do nothing

            }else{
                featureVector[i, ((j-1)*(4**k)+1):(j*(4**k))] <-
                    lookupTable[ substr( seq, j, j+k-1), ]
            }
        }
    }
    row.names( featureVector ) <- names( dnaStringSet )

    return ( featureVector )
}


#' Encode n-st order shape features
#
#' DNAshapeR can be used to generate feature vectors for a user-defined model.
#' The model can be a shape model. There are four structural parameters
#' including MGW, Roll, ProT and HelT. The second order shape features are
#' product terms of values for the same category of shape features at adjacent
#' positions.
#'
#' @usage encodeNstOrderShape(n, shapeMatrix, shapeType)
#'
#' @param n A number indicating n-st order shape encoding
#' @param shapeMatrix A matrix containing DNAshape prediction result
#' @param shapeType A character name of shape (MGW, Roll, ProT, HelT) features
#' @return featureVector A matrix containing encoded features. shape feature is
#' represented as continuous numbers
#' @author Tsu-Pei Chiu

encodeNstOrderShape <- function( n, shapeMatrix, shapeType ){
    # trim end columns with NA
    shapeMatrix[ is.na( shapeMatrix ) ] <- 0

    singleSeq <- FALSE
    if( nrow(shapeMatrix)[1] == 1 )
        singleSeq <- TRUE

    if( shapeType == "MGW" || shapeType == "ProT" ){
        shapeMatrix <- shapeMatrix[, -c(1, 2, ncol( shapeMatrix )-1,
            ncol( shapeMatrix ))]

    }else if( shapeType == "Roll" || shapeType == "HelT" ){
      shapeMatrix <- shapeMatrix[, -c(1, ncol( shapeMatrix ))]
    }

    if(singleSeq)
        shapeMatrix <- t(shapeMatrix)

    # encode k-st feature
    featureVector <- NULL
    if( n == 1 ){
        featureVector = shapeMatrix

    }else{
        m <- ncol( shapeMatrix )
        # normalization
        shapeMatrix <- normalizeShape( featureVector = shapeMatrix,
                                       thOrder = 1, shapeType = shapeType,
                                       normalize = TRUE )

        for ( i in 1 : ( m-n+1 )){
            feature <- shapeMatrix[, i]
            for ( j in ( i+1 ) : ( i+n-1 ) )
                feature <- feature * shapeMatrix[, j]

            featureVector <- cbind( featureVector, unlist( feature ) )
        }
    }

    return ( featureVector )
}


#' Normalize n-st order shape features
#'
#' @usage normalizeShape(featureVector, thOrder, shapeType, normalize)
#'
#' @param featureVector A matrix containing encoded features.
#' @param thOrder A number indicating n-st order shape encoding
#' @param shapeType A character name of shape (MGW, Roll, ProT, HelT) features
#' @param normalize A logical indicating whether to perform
#' normalization. Default to TRUE.
#' @return featureVector A matrix containing encoded features.
#' @author Tsu-Pei Chiu

normalizeShape <- function( featureVector, thOrder, shapeType, normalize ){
    minMGW <- 2.85
    maxMGW <- 6.2
    minProT <- -16.51
    maxProT <- -0.03
    minRoll <- -8.57
    maxRoll <- 8.64
    minHelT <- 30.94
    maxHelT <- 38.05

    if ( normalize  ){
        if( thOrder == 1){
            switch( shapeType,
                    MGW = {
                        featureVector <- normalize( featureVector,
                                                    maxMGW, minMGW )
                    },
                    ProT = {
                        featureVector <- normalize( featureVector,
                                                    maxProT, minProT )
                    },
                    Roll = {
                        featureVector <- normalize( featureVector,
                                                    maxRoll, minRoll )
                    },
                    HelT = {
                        featureVector <- normalize( featureVector,
                                                    maxHelT, minHelT )
                    }
                )

        }else{
            featureVector <- normalize( featureVector, max(featureVector),
                                        min(featureVector) )
        }
    }

    return ( featureVector )
}


#' Min-Max normalization
#'
#' @usage normalize(x, max, min)
#'
#' @param x A matrix containing encoded features
#' @param max A number maximum number for Min-Max Normalization
#' @param min A number minimum number for Min-Max Normalization
#' @return  featureVector A matrix containing encoded features. shape feature is
#' represented as continuous numbers
#' @author Tsu-Pei Chiu

normalize <- function( x, max, min ){
  return ( (x-min)/(max-min) )
}

