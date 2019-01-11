# Introduction

### Introduction
DNAshapeR predicts DNA shape features in an ultra-fast, high-throughput manner from genomic sequencing data. The package takes either nucleotide sequence or genomic intervals as input, and generates various graphical representations for further analysis. DNAshapeR further encodes DNA sequence and shape features for statistical learning applications by concatenating feature matrices with user-defined combinations of k-mer and DNA shape features that can be readily used as input for machine learning algorithms.


### Version
Current stable version is 1.9.5

### Download and Installation

Please make sure you have **the most recent R version (3.4 or later)**.

The latest (devel) version of DNAshapeR can be installed from this GitHub repository with:

* Without documentation

```{r}
 library(devtools)
 install_github(repo = "TsuPeiChiu/DNAshapeR")
```

* With documentation

```{r}
 library(devtools)
 install_github(repo = "TsuPeiChiu/DNAshapeR", build_vignettes = TRUE)
```

### Documentation
The package provides a vignette detailing all steps required to analyze your DNA shape data. To view this documentation for the version of DNAshapeR installed in your system, start R and enter:
```{r}
browseVignettes("DNAshapeR")
```

or **you can [Download the PDF documentation](./DNAshapeR_document.pdf)**.

### Citation
If you are using DNAshapeR for your analysis, please cite:
* Chiu T, Comoglio F, Zhou T, Yang L, Paro R, Rohs R (2016). “DNAshapeR: an R/Bioconductor package for DNA shape prediction and feature encoding.” Bioinformatics, 32, 1211-1213. doi: 10.1093/bioinformatics/btv735.

* Chiu T, Rao S, Mann R, Honig B, Rohs R (2017). “Genome-wide prediction of minor-groove electrostatic potential enables biophysical modeling of protein-DNA binding.” Nucleic Acids Res., 45(21), 12565-12576. doi: 10.1093/nar/gkx915, https://academic.oup.com/nar/article-lookup/doi/10.1093/nar/gkx915.

* Li J, Sagendorf J, Chiu T, Pasi M, Perez A, Rohs R (2017). “Expanding the repertoire of DNA shape features for genome-scale studies of transcription factor binding.” Nucleic Acids Res., 45(22), 12877-12887. doi: 10.1093/nar/gkx1145, https://academic.oup.com/nar/article-lookup/doi/10.1093/nar/gkx1145.

* Rao S, Chiu T, Kribelbauer J, Mann R, Bussemaker H, Rohs R (2018). “Systematic prediction of DNA shape changes due to CpG methylation explains epigenetic effects on protein-DNA binding.” Epigenetics Chromatin, 11:6. doi: 10.1186/s13072-018-0174-4, https://epigeneticsandchromatin.biomedcentral.com/articles/10.1186/s13072-018-0174-4.





### Contact
Please do not hesitate to contact us: tsupeich@usc.edu federico.comoglio@bsse.ethz.ch
