#' Identify Variant Sites in Genotype Files
#'
#' This function processes genotype data from multiple files to identify variant markers 
#' based on homozygosity thresholds for a set of individuals.
#' It supports parallel processing to improve performance when handling large datasets.
#'
#' The results are written to a specified output file and also returned as a logical vector.
#'
#'
#' @inheritParams diem
#' @inheritParams vcf2diem
#' @importFrom parallel mclapply detectCores
#'
#' @return A logical vector indicating whether each marker in the dataset is a variant 
#'  site (`TRUE`) or not (`FALSE`).
#' The same results are also written to the specified output file.
#'
#' @details
#'  A marker is considered a variant if at least `requireHomozygous` individuals are 
#'  homozygous for each of the two alleles encoded in the diem-formatted input files.
#'
#' Parallel processing when `nCores > 1` is available only for non-Windows operation 
#'  Windows computers must use `nCores = 1`. systems. 
#'
#' @examples
#' # Run this example in a folder with write permission
#' files <- c(system.file("extdata", "data7x3.txt", package = "diemr"),
#'            system.file("extdata", "data7x10.txt", package = "diemr"))
#' \dontrun{
#' 
#' variant1 <- variantSites(files, filename = "v1.txt")
#' variant2 <- variantSites(files, filename = "v2.txt", requireHomozygous = 2)
#' }
#'
#' @export

variantSites <- function(files, filename = "variantSites.txt", ChosenInds = "all", requireHomozygous = TRUE, nCores = 1) {


  #############################
  ####  Internal function  ####
  #############################

  variantSitesFile <- function(file) {
    gen <- sImport(file, ChosenInds = ChosenInds)

    # variantne markre
    I4 <- data.frame(
      colSums(gen == "_"),
      colSums(gen == "0"),
      colSums(gen == "1"),
      colSums(gen == "2")
    )
    variant <- I4[, 2] >= minHomozygous & I4[, 4] >= minHomozygous
    return(variant)
  }

  ##########################
  ####  End functions  #####
  ##########################

  if (is.na(nCores)) nCores <- 1
  if (nCores > parallel::detectCores()) nCores <- parallel::detectCores()

  minHomozygous <- as.numeric(requireHomozygous)

  fileConn <- file(filename, open = "w")
  on.exit(close(fileConn))

  res <- parallel::mclapply(
    files,
    FUN = variantSitesFile, mc.cores = nCores
  )

  res <- unlist(res)
  writeLines(as.character(res), fileConn)

  return(res)
}
