# diemr 1.5

- Added iris plots to the `plotPolarized`.
- Updated internal function `markerAxis` to avoid plotting multiple tick labels over one another.
- Updated `plotMarkerAxis` to check the expected user input.
- Added `variantSites` function that selects sites with required number of homozygous genotypes in a subset of individuals.
- Removed function shorthand `\(x)` syntax.
- Updated `plotPolarized` to unlist and unname the values in `HI`.
- Updated documentation.


# diemr 1.4.3, released 20-01-2025

- Updated `vcf2diem` to allow a user-defined minimum number of homozygous individuals, and to allow subsetting the conversion to `ChosenInds`.
- Updated documentation.


# diemr 1.4.2, released 02-12-2024

- Updated `rank2mapChr` internal function to be more efficient. It now uses the `Inchworm` algorithm by Stuart J.E. Baird.
- Modified `rank2map` to allow also a data.frame in the `inclucedSites` argument.
- Added `vcf2diem` obligatory output of sample names.
- Changed `smoothPolarizedGenotypes` to return a weighted mode, added corresponding internal functions `truncatedLaplace` and `unbiasedWeightedStateChoice`.


# diemr 1.4.1, released 23-09-2024

- Updated vignette authorship, and a link to a reference. 
- Fixed error message formatting in `CheckDiemFormat`.
- Added `rank2map` and `smoothPolarizedGenotypes`.
- Improved memory handling in `diem`.
- Fixed exit bug in `diem` when number of iterations reaches `maxIterations`. 

# diemr 1.4, released 16-07-2024

- Updated reference to vignette **Understanding genome polarisation output files**.
- Modified `importPolarized` to accept multiple files and a logical vector indicating which sites to import. The file processing is parallelised.
- Added functions `plotDeFinetti` and `plotMarkerAxis`.
- Modified `plotPolarized` to show colored tick marks for individuals and to accept selected graphical parameters.
- Fixed bug in `vcf2diem` when `requireHomozygous = TRUE`.
- Added `ChosenSites` argument in `diem` that allows to select sites for identifying a barrier to geneflow. Polarity is calculated for all sites, but only `ChosenSites` influence the likelihood in each EM iteration.
- Updated vignettes to reflect new functionality.

# diemr 1.3, released 14-06-2024

- Fixed bug in `vcf2diem` for multiallelic markers where REF allele is rare.
- Improved user-flexibility in reporting verbose EM search in `diem`. As a consequence `ModelOfDiagnostic` is no longer exported.
- Added vignette **Understanding genome polarisation output files**.
- Removed warnings when generating random null polarities.
- Hybrid indices and the 4-genomic state count matrix I4 are now calculated from the polarised genomes for all individuals. As a result, `diem` now requires ploidies for all individuals in the input files listed in the `files` argument.
- Set default `ploidy = FALSE` in `diem` that assumes all individuals are diploid across all compartments. 



# diemr 1.2.3, released 28-04-2024

- Updated `vcf2diem` functionality to optionally require homozygous individuals for both most frequent alleles.



# diemr 1.2.2, released 11-07-2023 

- Modified `vcf2diem` so that markers without homozygous genotypes or markers with only one heterozygous genotype are removed as non-informative for genome polarisation.
- Updated vignette **Importing data for genome polarisation**.
- Fixed formatting in `vcf2diem` documentation.


# diemr 1.2.1, released 19-04-2023

- Added link to bug reports page.
- Fixed bug in `vcf2diem` for multiallelic SNPs.


# diemr 1.2, released 21-03-2023

- Added `vcf2diem` to convert SNP in vcf format to diem genotypes.
- Added `myotis` vcf dataset with documentation.
- Added vignette **Importing data for genome polarisation**.
- Updated CITATION.



# diemr 1.1.1, released 02-02-2023

- Fixed potential infinite loop error in halting condition of `diem`.



# diemr 1.1, released 19-10-2022

- Updated README file.
- Added user-choice of colours in `plotPolarized`.
- Updated CITATION.
- Added efficient correction for other markers in likelihood calculation. 



# diemr 1.0, released 28-03-2022

- First public release.
- Updated citation.


# diemr 0.3, build 12-01-2022

- Added user choice of null polarities in the diem argument `markerPolarity`.
- Added FAQ to vignette.
- Added functions `importPolarized` and `plotPolarized`.


# diemr 0.2.1, build 02-12-2021

- Added examples to documentation.


# diemr 0.2, build 01-12-2021

- Added ploidy flexibility across compartments and across individuals.
- Updated documentation.


# diemr 0.1.2, build 09-07-2021

- Updated hybrid index calculation for consistency with diploid/haploid individuals.


# diemr 0.1.1, build 09-07-2021

- Added tolerance for unknown genotypes encoded either as `_` or `U`.
- Added vignette **diemr: Diagnostic index expectation maximisation in R**.
- Added a `NEWS.md` file to track changes to the package.
- Added `README.md` with quick start analysis instructions.
- Added default `ChosenInds` to include all individuals to `diem`. 


# diemr 0.1, build 29-06-2021

- First build