
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diemr: Genome polarisation in R

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/diemr)](https://CRAN.R-project.org/package=diemr)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/diemr)](https://cran.r-project.org/package=diemr)
[![Monthly
downloads](https://cranlogs.r-pkg.org/badges/last-month/diemr)](https://cran.r-project.org/package=diemr)
<!-- badges: end -->

*diemr* implements the **Diagnostic Index Expectation Maximization
(diem) algorithm** for **genome polarization** in `R`. It estimates
which alleles of single nucleotide variant (SNV) sites belong to either
side of a barrier to gene flow, co-estimates individual assignment, and
infers barrier strength and divergence. These tools are designed for
studies of **hybridization**, **speciation**, and population divergence,
and extend the methods described in Baird et al. (2023) Genome
polarisation for detecting barriers to geneflow. *Methods in Ecology and
Evolution 14*, 512-528 <doi:10.1111/2041-210X.14010>. For the original
algorithm description and implementations in `Python` and `Mathematica`,
see the *diem* repository at <https://github.com/StuartJEBaird/diem>.

## Installation

To start using *diemr*, load the package or install it from CRAN if it
is not yet available:

``` r
if(!require("diemr", character.only = TRUE)){
    install.packages("diemr", dependencies = TRUE)
    library("diemr", character.only = TRUE)
}
# Loading required package: diemr
```

Set working directory to a location with read and write privileges.

## Check data format

Next, assemble paths to all files containing the data to be used by
*diemr*. Here, we will use a tiny example dataset for illustration that
is included in the package. A good practice is to check that all files
contain data in correct format for all individuals and markers.

``` r
filepaths <- system.file("extdata", "data7x3.txt",
                         package = "diemr")
CheckDiemFormat(filepaths, ploidy = list(rep(2, 6)), ChosenInds = 1:6)
# File check passed: TRUE
# Ploidy check passed: TRUE
```

If the `CheckDiemFormat()` function fails, work through the error
messages and fix the stored input files accordingly. The algorithm
repeatedly accesses data from the harddisk, so seeing the passed file
check prior to analysis is critical.

``` r
diem.res <- diem(files = filepaths,
                 ploidy = list(rep(2, 6)), 
                 ChosenInds = 1:6,
                 verbose = TRUE,
                 nCores = 1)
```

The results including marker polarisation, marker diagnostic index and
its support will be included in the list element `diem.res$DI`.
Additional elements in the results list contain basic tracking
information about the expectation maximisation iterations, with details
provided in the folder *diagnostics* in the working directory.
