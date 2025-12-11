#' Read genomic sites from a BED-like file
#'
#' Imports a BED-like file and optionally subsets sites.
#'
#' @param includedSites Path to a BED-like file, ideally output from `vcf2diem`.
#' @param ChosenSites Logical or numeric vector specifying sites to retain, or `"all"` to keep all sites.
#'
#' @return A data frame containing the selected sites with 1-based coordinates.
#' @export
#' @importFrom data.table fread
#' @examples
#' \dontrun{
#' # Run this example in a working directory with write permissions
#' myo <- system.file("extdata", "myotis.vcf", package = "diemr")
#' vcf2diem(myo, "myo")
#' readIncludedSites("myo-001.txt")
#' readIncludedSites("myo-001.txt", ChosenSites = 2:7)
#' }
readIncludedSites <- function(includedSites, ChosenSites = "all") {
  if (!grepl("CHROM", readLines(includedSites, n = 1)[[1]])) {
    bed <- data.table::fread(includedSites, header = FALSE, sep = "\t", data.table = FALSE)[, c(1:2)]
    # convert to 1-based
    bed[, 2] <- bed[, 2] + 1
  } else {
    bed <- data.table::fread(includedSites, header = TRUE, sep = "\t", data.table = FALSE)
  }
  if (inherits(ChosenSites, "logical") || inherits(ChosenSites, "numeric")) {
    bed <- bed[ChosenSites, ]
  }

  return(bed)
}
