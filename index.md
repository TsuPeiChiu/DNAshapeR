# Introduction

### Introduction
DNAshapeR predicts DNA shape features in an ultra-fast, high-throughput manner from genomic sequencing data. The package takes either nucleotide sequence or genomic intervals as input, and generates various graphical representations for further analysis. DNAshapeR further encodes DNA sequence and shape features for statistical learning applications by concatenating feature matrices with user-defined combinations of k-mer and DNA shape features that can be readily used as input for machine learning algorithms.


### Version
Current stable version is 1.0

### Download and Installation

Please make sure you have **the most recent R version (3.3 or later)**.

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

or **you can [Download the PDF documentation](http://rohslab.cmb.usc.edu/Documents/DNAshapeR_document.pdf)**.

### Citation
If you are using DNAshapeR for your analysis, please cite:
* T.-P. Chiu , F. Comoglio, T. Zhou, L. Yang, R. Paro, and R. Rohs: DNAshapeR: an R/Bioconductor package for DNA shape prediction and feature encoding. Bioinformatics 32, 1211-1213 (2016). 

### Contact
Please do not hesitate to contact us: tsupeich@usc.edu federico.comoglio@bsse.ethz.ch
