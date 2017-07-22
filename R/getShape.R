#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DNAshapeR
# 2015, 2017
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
#' In the latest version, we further added additional 9 DNA shape features
#' beyond our previous set of 4 features, and expanded our available repertoire
#' to a total of 13 features, including 6 inter-base pair or base pair-step
#' parameters (HelT, Rise, Roll, Shift, Slide, and Tilt), 6 intra-base pair or
#' base pair-step parameters (Buckle, Opening, ProT, Shear, Stagger,
#' and Stretch), and MGW.
#'
#' Predict biophysical feature
#'
#' Our previous work explained protein-DNA binding specificity based on
#' correlations between MGW and electrostatic potential (EP) observed in
#' experimentally available structures (Joshi, et al., 2007). However, A/T
#' and C/G base pairs carry different partial charge distributions in the
#' minor groove (due primarily to the guanine amino group), which will affect
#' minor-groove EP. We developed a high-throughput method to predict
#' minor-groove EP based on data mining of results from solving the nonlinear
#' Poisson-Boltzmann calculations (Honig & Nicholls, 1995) on 2,297 DNA
#' structures derived from Monte Carlo simulations. DNAshapeR includes EP
#' as an additional feature.
#'
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

    # read file and parse
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


#' Convert fasta file to methylated file format
#'
#' @usage convertMethFile(fastaFileName, methPositionFileName)
#'
#' @param fastaFileName The name of the input fasta format file, including
#' full path to file if it is located outside the current working directory.
#' @param methPositionFileName The name of the input position file
#' indicating the methlation position
#'
#' @return methFileName fasta file containing methylated Cytosine
#'
#' @author Satyanarayan Rao & Tsu-Pei Chiu

convertMethFile <- function( fastaFileName, methPositionFileName = NULL ) {

    #convertFileName <- fastaFileName
    # read the sequence fasta file first
    fastaFile <- readDNAStringSet( fastaFileName )
    seqName = names( fastaFile )
    seq = paste( fastaFile )
    dfFastaFile <- data.frame( seqName, seq )

    dfFastaFile$seq_name <- as.character ( dfFastaFile$seqName )
    dfFastaFile$seq <- as.character ( dfFastaFile$seq )
    rownames( dfFastaFile ) <- dfFastaFile[["seqName"]]

    # prepare the output file name
    filenameWoExt <- tools::file_path_sans_ext( fastaFileName,
                                                compression = FALSE )
    extension <- tools::file_ext( fastaFileName )
    methFileName <- paste0( filenameWoExt, "_", "methylated" )
    methFileName <- paste( methFileName, extension, sep="." )

    # according to the methPositionFileName convert the methylated nucleotide
    # to MG/Mg and then replace the letter with MQ
    idxM <- which( grepl( "M", dfFastaFile[["seq"]] ) == TRUE )
    idxWoM <- which( grepl("M", dfFastaFile[["seq"]] ) == FALSE )
    seqNum <- dim(dfFastaFile)[1]

    arrayOfNumbers = 1:seqNum
    for ( i in idxM ) {
        dfFastaFile[i, "seq"] = gsub( "MG", "MQ", dfFastaFile[i, "seq"] )
    }

    if (is.null (methPositionFileName)) {
    # no changes to be done at any position
    # check if the fasta file contains sequences with letter "M", or "g".
    # the target is to return the a filename containing sequences with
    # only captial letter and "g" replaced by "Q".
    # Checking if the input fasta sequences have letter M present in them.
    # If a sequence has M letter then the conversion will not be done for
    # that sequence, otherwise all the CpG positions in that sequence
    # will be checking what all sequences have M
        for ( i in idxWoM ){
            dfFastaFile[i, "sequence"] = gsub ("CG", "MQ",
                                               dfFastaFile[i, "seq"])
        }
        OutputSeqs = Biostrings::BStringSet(dfFastaFile[["seq"]])
        names (OutputSeqs) = dfFastaFile[["seqName"]]
        Biostrings::writeXStringSet(OutputSeqs, methFileName)

    } else {
    # mark the methlation according to methPositionFileName
        df.CpGtoMpQ = readNonStandardFastaFile (methPositionFileName)
        names (df.CpGtoMpQ) = c ("seqName", "positions_to_convert_CpG_MpQ")
        rownames (df.CpGtoMpQ) = df.CpGtoMpQ[["seqName"]]
        for (name in df.CpGtoMpQ[["seqName"]]){
            positions_to_convert = df.CpGtoMpQ [name, "positions_to_convert_CpG_MpQ"]
            positions_to_convert = strtoi (unlist (strsplit (positions_to_convert, ",")))
            targetSequence = dfFastaFile[name, "seq"]
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
            dfFastaFile[name, "seq"] = targetSequence
        }
        # seqinr::write.fasta(sequences = as.list(dfFastaFile[["sequence"]]),
        #                     names = as.list (dfFastaFile[["seq_name"]]),
        #                     file.out = methFileName)
        OutputSequences = Biostrings::BStringSet(dfFastaFile[["seq"]])
        names (OutputSequences) = dfFastaFile[["seqName"]]
        Biostrings::writeXStringSet(OutputSequences, methFileName)
    }
    convertFileName <- methFileName
    return (convertFileName)
}


#' Read the position fasta file
#'
#' @usage readNonStandardFastaFile(filename)
#'
#' @param filename The name of the input position file
#' indicating the methlation position
#'
#' @return df dataframe
#'
#' @author Satyanarayan Rao & Tsu-Pei Chiu
#'

readNonStandardFastaFile <- function( filename ) {
    keys = c()
    content = c ()
    con = file (filename, "r")
    valueKey = ""
    while (TRUE) {
        line = gsub ("\n", "", readLines( con, n = 1 ))

        if ( length( line ) == 0 ){
            break
        }
        if ( line == "" ) {
            next
        } else if( ">" == substring( line, 1, 1 ) ) {
            key = substring( line, 2 )

            while( TRUE ){
                tmpKey = gsub( "\n","",readLines(con, n = 1 ) )
                if (length( tmpKey ) == 0 ){
                    break
                }

                if ( tmpKey == ""){
                    next

                } else if( ">" == substring( tmpKey, 1, 1 ) ) {
                    content <- c( content, valueKey )
                    keys <- c( keys, key )
                    valueKey <- ""
                    key <- substring(tmpKey ,2)

                } else {
                    valueKey <- paste0( valueKey, tmpKey,
                                 collapse="" )
                }
            }
            content <- c( content, valueKey )
            keys <- c( keys, key )
        } else {
            key <- "1"
            valueKey <- line

            while( TRUE ) {
                line <- readLines( con, n = 1 )
                if ( length(line) == 0 ){
                    break
                }
                valueKey <- paste0(valueKey, line, collapse="")
            }
            content <- c (line_to_append)
            keys <- c (key)
        }
    }
    close (con)

    df <- data.frame (keys = keys, content = content)
    df$keys <- as.character(df$keys)
    df$content <- as.character(df$content)
    return (df)
}
