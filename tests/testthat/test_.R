#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DNAshapeR Unit testing
# 2015 
# Tsu-Pei Chiu, Rohs Lab, USC
# Federico Comoglio, Green lab, CIMR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fn <- system.file( 'extdata', 'SingleSeqsample.fa.MGW', package = 'DNAshapeR' )
mgw <- readShape(fn)
expect_equal( length(mgw), 24 )