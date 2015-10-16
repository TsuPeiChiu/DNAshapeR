# 2015 - Tsu-Pei Chiu, Rohs Lab, USC

#' encode k-mer DNA sequence and n-st DNA Shape features
#'
#' @usage encodeSeqShape
#'
#' @param fastaFileName DESCR
#' @param shapeMatrix DESCR
#' @param featureNames DESCR
#'
#' @return featureVector
#' @note None
#'
#' @author Tsu-Pei Chiu
#'

encodeSeqShape <- function( fastaFileName, shapeMatrix, featureNames){
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


#' encode k-mer DNA sequence features
#'
#' @usage encodeShape(filepath)
#'
#' @param k k-mer sequence
#' @param dnaStringSet
#'
#' @return featureVector
#' @note None

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


#' encode k-mer DNA sequence features
#'
#' @usage encodeKmerHbond (filepath)
#'
#' @param k k-mer sequence
#' @param dnaStringSet
#'
#' @return featureVector
#' @note None

encodeKmerHbond <- function ( k, dnaStringSet ){
  # create a lookup table
  k <- 1
  lookupTable <- rbind( c(1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1), c(0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0),
                        c(1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0), c(0,0,0,1,1,0,0,0,0,1,0,0,1,0,0,0) )
  row.names( lookupTable ) <- mkAllStrings( c( "A", "C", "G", "T" ), k )

  featureVector <- c()

  for( j in 1 : length( dnaStringSet ) ){
    # encode k-mer feature
    features <- c()
    seq <- toString( dnaStringSet[j] )
    for ( j in 1 : ( nchar( seq )-k+1) ){
      if( is.na( match( substr( toupper( seq ), j, j+k-1), row.names( lookupTable ) ) ) ){
        features <- c( features, rep( 0, 16**k ) )

      }else{
        features <- c( features, lookupTable[ substr( toupper( seq ), j, j+k-1), ] )
      }
    }

    featureVector <- rbind( featureVector, features )
  }
  row.names( featureVector ) <- names( dnaStringSet )

  return ( featureVector )

}


#' encode n-st order shape features
#'
#' @usage encodeNstOrderShape
#'
#' @param n n-st order shape
#' @param shapeMatrix DESCR
#'
#' @return  featureVector
#' @note None
#'

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
