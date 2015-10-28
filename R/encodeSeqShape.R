# 2015 - Tsu-Pei Chiu, Rohs Lab, USC


#' Encode k-mer DNA sequence and n-st order DNA Shape features
#'
#' DNAshapeR can be used to generate feature vectors for a user-defined model.
#' These models can be sequence (1-mer, 2-mer, 3-mer) or shape (MGW, Roll,
#' ProT, HelT) models or any combination thereof. Sequence is encoded in four
#' binary features (i.e., in terms of 1-mers, 0001 for adenine, 0010 for
#' cytosine, 0100 for guanine, and 1000 for thymine) at each nucleotide
#' position (Zhou, et al., 2015). The function permits an encoding of 2-mers
#' and 3-mers (16 and 64 binary features at each position, respectively).
#' Shape features include first and second order (or higher order) values for
#' the four structural parameters MGW, Roll, ProT and HelT. The second order
#' shape features are product terms of values for the same category of shape
#' features at adjacent positions. The function permits the generation of any
#' subset of these features, either only a selected shape category or first
#' order shape features, and any combination with shape or sequence features.
#' The result of the feature encoding is a concatenated feature vector list
#' for each sequence, which results in a feature matrix for a dataset of
#' multiple sequences, which in turn can serve as input for any statistical
#' machine learning method.
#'
#'
#' @usage encodeSeqShape(fastaFileName, shapeMatrix, featureNames)
#'
#' @param fastaFileName The Name of the input fasta format file, including
#' full path to file if it is located outside the current working directory.
#' @param shapeMatrix A matrix containing DNAshape prediction result
#' @param featureNames A vector containing a combination of user-defined
#' sequence and shape parameters. The parameters can be any combination of
#' "k-mer", "n-shape", "n-MGW", "n-ProT", "n-Roll", "n-HelT" (k, n are
#' integers)
#' @return featureVector A matrix containing encoded features. Sequence
#' feature is represented as binary numbers, while shape feature is
#' represented as continuous numbers.
#' @author Tsu-Pei Chiu
#' @keywords core
#' @examples
#'
#' library(DNAshapeR)
#' fn <- system.file("extdata", "CGRsample.fa", package = "DNAshapeR")
#' pred <- getShape(fn)
#' featureNames <- c("1-mer", "1-shape")
#' featureVector <- encodeSeqShape(fn, pred, featureNames)
#'
#' @export encodeSeqShape

encodeSeqShape <- function( fastaFileName, shapeMatrix, featureNames){
# Generate sequence and shape feature matrix
#
# Args:
#   fastaFileName: character, the filename and/or path to it
#   shapeMatrix: a matrix containing shape prediction result
#
# Returns:
#   a matrix of encoded features
#
# Error handling
#   ...
  ds <- Biostrings::readDNAStringSet(fastaFileName, "fasta")

  featureVector <- c()
  for( i in 1:length( featureNames ) ){
    featureName <- unlist(strsplit(featureNames[i], "-"))
    switch( featureName[2],
            mer = { featureVector <- cbind( featureVector, encodeKMerSeq( as.numeric( featureName[1] ), ds ) ) },
            hbond = { featureVector <- cbind( featureVector, encodeKMerHbond( as.numeric( featureName[1] ), ds ) ) },
            MGW = { featureVector <- cbind( featureVector, encodeNstOrderShape( as.numeric( featureName[1] ), shapeMatrix$MGW ) ) },
            ProT = { featureVector <- cbind( featureVector, encodeNstOrderShape( as.numeric( featureName[1] ), shapeMatrix$ProT ) ) },
            Roll = { featureVector <- cbind( featureVector, encodeNstOrderShape( as.numeric( featureName[1] ), shapeMatrix$Roll ) ) },
            HelT = { featureVector <- cbind( featureVector, encodeNstOrderShape( as.numeric( featureName[1] ), shapeMatrix$HelT ) ) },
            shape = {
              featureVector <- cbind( featureVector,
                                      encodeNstOrderShape( as.numeric( featureName[1] ), shapeMatrix$MGW ),
                                      encodeNstOrderShape( as.numeric( featureName[1] ), shapeMatrix$ProT ),
                                      encodeNstOrderShape( as.numeric( featureName[1] ), shapeMatrix$Roll ),
                                      encodeNstOrderShape( as.numeric( featureName[1] ), shapeMatrix$HelT ) )
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
#' @usage encodeShape(k, dnaStringSet)
#'
#' @param k A number indicating k-mer sequence encoding
#' @param dnaStringSet A DNAStringSet object of the inputted fasta file
#' @return featureVector A matrix containing encoded features. Sequence
#' feature is represented as binary numbers
#' @author Tsu-Pei Chiu
#'
#' @export encodeKMerSeq

encodeKMerSeq <- function( k, dnaStringSet ){
  # create a lookup table
  lookupTable <- diag( 4**k )
  row.names( lookupTable ) <- mkAllStrings( c( "A", "C", "G", "T" ), k )

  featureVector <- c()

  for( j in 1 : length( dnaStringSet ) ){
    # encode k-mer feature
    features <- c()
    seq <- toString( dnaStringSet[j] )
    for ( j in 1 : ( nchar( seq )-k+1) ){
      if( is.na( match( substr( toupper( seq ), j, j+k-1), row.names( lookupTable ) ) ) ){
        features <- c( features, rep( 0, 4**k ) )

      }else{
        features <- c( features, lookupTable[ substr( toupper( seq ), j, j+k-1), ] )
      }
    }

    featureVector <- rbind( featureVector, features )
  }
  row.names( featureVector ) <- names( dnaStringSet )

  return ( featureVector )
}


#' Encode n-st order shape features
#'
#' DNAshapeR can be used to generate feature vectors for a user-defined model.
#' The model can be a shape model. There are four structural parameters
#' including MGW, Roll, ProT and HelT. The second order shape features are
#' product terms of values for the same category of shape features at adjacent
#' positions.
#'
#' @usage encodeNstOrderShape(n, shapeMatrix)
#'
#' @param n A number indicating n-st order shape encoding
#' @param shapeMatrix A matrix containing DNAshape prediction result
#' @return  featureVector A matrix containing encoded features. shape feature is
#' represented as continuous numbers
#' @author Tsu-Pei Chiu
#'
#' @export encodeNstOrderShape

encodeNstOrderShape <- function( n, shapeMatrix ){
  # trim end columns with NA
  shapeMatrix[ is.na( shapeMatrix ) ] <- 0
  for( i in c( ncol( shapeMatrix ), ncol( shapeMatrix )-1, 2, 1 ) ){
    if( all( shapeMatrix[ , i ] == 0 ) == TRUE )
      shapeMatrix <- shapeMatrix[ , -i ]
  }

  # encode k-st feature
  featureVector <- NULL
  if( n == 1 ){
    featureVector = shapeMatrix

  }else{
    m <- ncol( shapeMatrix )
    # normalization
    shapeMatrix <- ( shapeMatrix - min( shapeMatrix ) ) / ( max( shapeMatrix ) - min( shapeMatrix ))

    for ( i in 1 : ( m-n+1 )){
      feature <- shapeMatrix[, i]
      for ( j in ( i+1 ) : ( i+n-1 ) )
        feature <- feature * shapeMatrix[, j]

      featureVector <- cbind( featureVector, unlist( feature ) )
    }
  }

  return ( featureVector )
}


##' encode Hbond
##'
##' @usage encodeKmerHbond (filepath)
##' @param k k-mer sequence
##' @param dnaStringSet
##' @return featureVector

#encodeKmerHbond <- function ( k, dnaStringSet ){
#  # create a lookup table
#  k <- 1
#  lookupTable <- rbind( c(1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1), c(0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0),
#                        c(1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0), c(0,0,0,1,1,0,0,0,0,1,0,0,1,0,0,0) )
#  row.names( lookupTable ) <- mkAllStrings( c( "A", "C", "G", "T" ), k )
#
#  featureVector <- c()
#
#  for( j in 1 : length( dnaStringSet ) ){
#    # encode k-mer feature
#    features <- c()
#    seq <- toString( dnaStringSet[j] )
#    for ( j in 1 : ( nchar( seq )-k+1) ){
#      if( is.na( match( substr( toupper( seq ), j, j+k-1), row.names( lookupTable ) ) ) ){
#        features <- c( features, rep( 0, 16**k ) )
#
#      }else{
#        features <- c( features, lookupTable[ substr( toupper( seq ), j, j+k-1), ] )
#      }
#    }
#
#    featureVector <- rbind( featureVector, features )
#  }
#  row.names( featureVector ) <- names( dnaStringSet )
#
#  return ( featureVector )
#
#}
