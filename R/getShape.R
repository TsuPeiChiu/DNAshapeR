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

getShape <- function(filename, shapeType = 'Default', parse = TRUE,
                        methylate = FALSE, methylatedPosFile = NULL) {

    # without methylation
    if( methylate == FALSE ){
        defaultOpts <- c( 'MGW', 'HelT', 'ProT', 'Roll', 'EP')
        additionalOpts <- c('Stretch', 'Tilt', 'Buckle', 'Shear', 'Opening',
                            'Rise', 'Shift', 'Stagger', 'Slide')
        stopifnot( shapeType %in% c( defaultOpts, additionalOpts, 'Default' ) )

        if( length(shapeType) == 1 && shapeType == 'Default' ) {
            lapply(defaultOpts, getDNAShape, fastaFilePath = filename)

        } else {
            lapply(shapeType, getDNAShape, fastaFilePath = filename)
        }

        if( parse ) {
            message( 'Parsing files......' )
            if( length(shapeType) == 1 && shapeType == 'Default' ) {
                ln <- paste0( filename, '.', defaultOpts )
                shapeList <- lapply( ln, readShape )
                names( shapeList ) <- defaultOpts

            } else {
                ln <- paste0( filename, '.', shapeType )
                shapeList <- lapply( ln, readShape )
                names( shapeList ) <- shapeType
            }

            message( 'Done' )
            return( shapeList )
        }


    # with methylation
    } else {
        defaultOpts <- c( 'MGW', 'HelT', 'ProT', 'Roll')
        stopifnot( shapeType %in% c( defaultOpts, 'Default' ) )

        # file format converting (Satya)
        convertFileName <- convertMethFile ( filename, methylatedPosFile )


        if( length(shapeType) == 1 && shapeType == 'Default' ) {
            lapply(defaultOpts, getDNAShape, fastaFilePath = convertFileName)

        } else {
            lapply(shapeType, getDNAShape, fastaFilePath = convertFileName)
        }

        if( parse ) {
            message( 'Parsing files......' )
            if( length(shapeType) == 1 && shapeType == 'Default' ) {
                ln <- paste0( convertFileName, '.', defaultOpts )
                shapeList <- lapply( ln, readShape )
                names( shapeList ) <- defaultOpts

            } else {
                ln <- paste0( convertFileName, '.', shapeType )
                shapeList <- lapply( ln, readShape )
                names( shapeList ) <- shapeType
            }

            message( 'Done' )
            return( shapeList )
        }
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



#' File converter (Satya)
#'
#' Convert file
#'

convertMethFile <- function( fastaFileName, methPositionFileName = NULL ) {

  convertFileName <- fastaFileName
  # read the sequence fasta file first
  fastaFile <- readDNAStringSet(fastaFileName)
  seq_name = names(fastaFile)
  sequence = paste(fastaFile)
  dfSequenceFastaFile <- data.frame(seq_name, sequence)
  dfSequenceFastaFile$seq_name = as.character (dfSequenceFastaFile$seq_name)
  dfSequenceFastaFile$sequence = as.character (dfSequenceFastaFile$sequence)
  #dfSequenceFastaFile = readPositionFastaFile(fastaFileName)
  #names (dfSequenceFastaFile) = c("seq_name", "sequence")
  rownames(dfSequenceFastaFile) = dfSequenceFastaFile[["seq_name"]]

  # New file name
  fname_without_extension = tools::file_path_sans_ext(fastaFileName,
                                                      compression = FALSE)
  extension = tools::file_ext(fastaFileName)
  newfastaFileName = paste0(fname_without_extension, "_", "methylated")
  newfastaFileName = paste (newfastaFileName, extension, sep=".")
  # No matter user provides methPositionFileName, if sequences contain MG or Mg, then these words will be replaced by MQ
  IdxOfsequenceWithM = which (grepl ("M", dfSequenceFastaFile[["sequence"]]) == TRUE)
  IdxOfsequenceWithoutM = which (grepl ("M", dfSequenceFastaFile[["sequence"]]) == FALSE)
  NumberOfSequences = dim(dfSequenceFastaFile)[1]
  arrayOfNumbers = 1:NumberOfSequences
  for (i in IdxOfsequenceWithM) {
    dfSequenceFastaFile[i, "sequence"] = gsub ("MG",
                                               "MQ",
                                               dfSequenceFastaFile[i, "sequence"])
  }
  if (is.null (methPositionFileName)) {
    # No changes to be done at any position
    # Check if the fasta file contains sequences with letter "M", or "g".
    # The target is to return the a filename containing sequences with only captial letter and "g" replaced by "Q"
    # Checking if the input fasta sequences have letter M present in them. If a sequence has M letter,
    # then the conversion will not be done for that sequence, otherwise all the CpG positions in that sequence will be
    #

    # Checking what all sequences have M:

    for (i  in IdxOfsequenceWithoutM){
      dfSequenceFastaFile[i, "sequence"] = gsub ("CG",
                                                 "MQ",
                                                 dfSequenceFastaFile[i, "sequence"])
    }

    # seqinr::write.fasta(sequences = as.list (dfSequenceFastaFile[["sequence"]]),
    #                     names = as.list (dfSequenceFastaFile[["seq_name"]]),
    #                     file.out = newfastaFileName)
    OutputSequences = Biostrings::BStringSet(dfSequenceFastaFile[["sequence"]])
    names (OutputSequences) = dfSequenceFastaFile[["seq_name"]]
    Biostrings::writeXStringSet(OutputSequences, newfastaFileName)

  } else {
    # Calling readPositionFastaFile
    df.CpGtoMpQ = readNonStandardFastaFile (methPositionFileName)
    names (df.CpGtoMpQ) = c ("seq_name", "positions_to_convert_CpG_MpQ")
    rownames (df.CpGtoMpQ) = df.CpGtoMpQ[["seq_name"]]
    for (name in df.CpGtoMpQ[["seq_name"]]){
      positions_to_convert = df.CpGtoMpQ [name, "positions_to_convert_CpG_MpQ"]
      positions_to_convert = strtoi (unlist (strsplit (positions_to_convert, ",")))
      targetSequence = dfSequenceFastaFile[name, "sequence"]
      targetSequence = unlist (strsplit(targetSequence, split = ""))
      lengthOftagetSequence = length (targetSequence)
      for (j in positions_to_convert) {

        if (j <=lengthOftagetSequence && targetSequence[j] == "C") {
          targetSequence[j] = "M"
          if (j + 1 <= lengthOftagetSequence) {
            targetSequence[j+1] = "Q"
          }
        }else {
          msg = paste ("ERROR:", "position mentioned in",
                       methPositionFileName,
                       "for sequence",
                       name,
                       "is not valid. Letter 'C' was not found at position",
                       j, sep = " ")
          message (msg)
          stop ()
        }
      }
      targetSequence = paste0(targetSequence, collapse = "")
      dfSequenceFastaFile[name, "sequence"] = targetSequence
    }
    # seqinr::write.fasta(sequences = as.list(dfSequenceFastaFile[["sequence"]]),
    #                     names = as.list (dfSequenceFastaFile[["seq_name"]]),
    #                     file.out = newfastaFileName)
    OutputSequences = Biostrings::BStringSet(dfSequenceFastaFile[["sequence"]])
    names (OutputSequences) = dfSequenceFastaFile[["seq_name"]]
    Biostrings::writeXStringSet(OutputSequences, newfastaFileName)


  }
  convertFileName <- newfastaFileName
  return (newfastaFileName)
}

#' A method to read the position fasta file
readNonStandardFastaFile <- function (filename) {
  keys = c()
  content = c ()
  con = file (filename, "r")
  value_for_key = ""
  while (TRUE) {
    line = gsub ("\n", "", readLines(con, n = 1 ))

    if (length(line) == 0 ){
      break
    }
    if (line== "") {
      next
    } else if (">" == substring(line,1,1)) {
      key = substring(line,2)
      while (TRUE){
        tmp_value_for_key = gsub("\n","",readLines(con, n = 1 ))
        if (length(tmp_value_for_key)  == 0 ){
          break
        }
        if ( tmp_value_for_key==""){
          next
        } else if (">" == substring (tmp_value_for_key,1,1)) {

          content = c (content, value_for_key)
          keys = c (keys, key)
          value_for_key = ""
          key = substring(tmp_value_for_key ,2)
        } else {
          value_for_key = paste0(value_for_key, tmp_value_for_key, collapse="")
        }
      }
      content = c (content, value_for_key)
      keys = c (keys, key)
    } else {
      key = "1"
      value_for_key = line
      while (TRUE) {
        line = readLines(con, n = 1 )
        if (length(line) == 0 ){
          break
        }
        value_for_key = paste0(value_for_key, line, collapse="")
      }
      content = c (line_to_append)
      keys = c (key)
    }
  }
  close (con)
  # the data frame colum names are sigular
  df = data.frame (keys = keys, content = content)
  df$keys = as.character(df$keys)
  df$content = as.character(df$content)
  return (df)
}
