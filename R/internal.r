#' Import a single diem-formatted genotype file
#'
#' Reads a diem genotype file, splits character data into a matrix, and optionally subsets individuals.
#' Unknown genotypes (`U`) are converted to `_`.
#'
#' @param file Path to the diem-formatted genotype file.
#' @param ChosenInds A vector of individual indices to keep, or `"all"` to retain all individuals.
#'
#' @return A character matrix with individuals in rows and markers in columns.
#' @keywords internal
#' @importFrom data.table fread

sImport <- function(file, ChosenInds = "all") {
  genotypes <- data.table::fread(file = file, header = FALSE, colClasses = "character")[[1]]
  genotypes <- do.call(cbind, lapply(genotypes, function(x) strsplit(x, "", fixed = TRUE)[[1]][-1]))
  if (ChosenInds[1] != "all") {
    genotypes <- genotypes[ChosenInds, , drop = FALSE]
  }
  if ("U" %in% genotypes) {
    genotypes[genotypes == "U"] <- "_"
  }
  return(genotypes)
}



#' Resolve compartments for diem input files
#'
#' Generates a list of logical vectors indicating which sites should be compartmentalized.
#' If `toBeCompartmentalized` is `"all"`, all sites are marked as `TRUE`. Otherwise, a logical vector
#' must be provided with a length matching the total number of sites.
#'
#' @param files Paths to diem input files.
#' @param toBeCompartmentalized Either `"all"` or a logical vector specifying sites to compartmentalize.
#' @param compartmentSizes Optional vector specifying the number of sites per file.
#'
#' @return A list of logical vectors corresponding to each file.
#' @keywords internal

resolveCompartments <- function(files, toBeCompartmentalized, compartmentSizes = NULL) {
  if (missing(files)) stop("Provide paths to diem input files in the `files` argument.")
  if (is.null(compartmentSizes)) {
    compartmentSizes <- unname(sapply(files, FUN = function(x) {
      length(readLines(x))
    }))
  }

  nMarkers <- sum(compartmentSizes)

  if (inherits(toBeCompartmentalized, "character")) {
    toBeCompartmentalized <- lapply(compartmentSizes, FUN = function(x) rep(TRUE, x))
  } else {
    if (inherits(toBeCompartmentalized, "logical")) {
      if (length(toBeCompartmentalized) != nMarkers) {
        compartmentSizes <- unname(sapply(files, FUN = function(x) {
          length(readLines(x))
        }))
        if (length(toBeCompartmentalized) != nMarkers) {
          stop("toBeCompartmentalized does not have the same length (", nMarkers, ") as the number of sites is the files.")
        }
      }

      compartmentLabels <- rep(seq_along(compartmentSizes), compartmentSizes)
      toBeCompartmentalized <- unname(split(toBeCompartmentalized, compartmentLabels))
    } else {
      stop("toBeCompartmentalized must be either `all` or a logical vector with the length equal to the total number of markers in `files`")
    }
  }

  return(toBeCompartmentalized)
}


#' Compute chromosome breakpoints and tick positions
#'
#' Identifies chromosome breaks and calculates tick positions based on physical distances.
#' Used for plotting genomic axes with consistent spacing.
#'
#' @param includedSites BED-like file defining genomic sites, ideally output from `vcf2diem`.
#' @param ChosenSites Logical vector of sites to be analyzed.
#' @param tickDist Distance between tick marks along the chromosome in bp.
#'
#' @return A list containing:
#'   - `CHROMbreaks`: Positions of chromosome breaks.
#'   - `CHROMnamesPos`: Midpoints for chromosome labels.
#'   - `CHROMnames`: Unique chromosome names.
#'   - `ticksPos`: Positions for tick marks.
#'   - `ticksNames`: Tick labels in megabases.
#'
#' @keywords internal
#' @importFrom zoo rollmean
markerAxis <- function(includedSites, ChosenSites = "all", tickDist) {
  if (missing(includedSites)) {
    stop("Provide both 'includedSites' and 'ChosenSites'.")
  }
  bed <- readIncludedSites(includedSites = includedSites, ChosenSites = ChosenSites)

  # chromosome labels
  CHROMbreaks <- c(which(!duplicated(bed$CHROM)) - 0.5, nrow(bed) + 0.5)
  CHROMnamesPos <- zoo::rollmean(CHROMbreaks, k = 2)
  CHROMnames <- unique(bed$CHROM)

  # tick positions

  ticksPos <- integer(0)
  ticksNames <- integer(0)

  for (i in 1:length(CHROMnames)) { # i - chromosome
    pos <- bed$POS[bed$CHROM == CHROMnames[i]] # positions in a chromosome
    if (max(pos) > tickDist) {
      ticksFor <- seq(tickDist, max(pos), by = tickDist)
      for (k in ticksFor) {
        ticksAt <- which(pos > k)[1]
        if (length(ticksAt) > 0) {
          ticksPos <- c(ticksPos, which(bed$CHROM == CHROMnames[i])[ticksAt])
          ticksNames <- c(ticksNames, k)
        }
      }
    }
  }
  ticksPos <- ticksPos - 0.5
  ticksNames <- ticksNames / 1000000
  checkDuplicated <- duplicated(ticksPos)
  ticksPos <- ticksPos[!checkDuplicated]
  ticksNames <- ticksNames[!checkDuplicated]

  return(list(
    CHROMbreaks = CHROMbreaks,
    CHROMnamesPos = CHROMnamesPos,
    CHROMnames = CHROMnames,
    ticksPos = ticksPos,
    ticksNames = ticksNames
  ))
}



#' Read genomic sites from a BED-like file
#'
#' Imports a BED-like file and optionally subsets sites.
#'
#' @param includedSites Path to a BED-like file, ideally output from `vcf2diem`.
#' @param ChosenSites Logical or numeric vector specifying sites to retain, or `"all"` to keep all sites.
#'
#' @return A data frame containing the selected sites.
#' @keywords internal
#' @importFrom data.table fread
readIncludedSites <- function(includedSites, ChosenSites = "all") {
  bed <- data.table::fread(includedSites, header = TRUE, sep = "\t", data.table = FALSE)

  if (inherits(ChosenSites, "logical") || inherits(ChosenSites, "numeric")) {
    bed <- bed[ChosenSites, ]
  }

  return(bed)
}


# Original up to the diemr 1.4.1
# rank2mapChr <- function(x, windowSize = 3) {
#  n <- length(x)
#  if(is.null(windowSize)){
#    warning("windowSize is NULL. Using windowSize = 1e+07.")
#    windowSize <- 1e+07
#  }
#  halfSize <- windowSize / 2
#  res <- matrix(NA, ncol = 2, nrow = n, dimnames = list(NULL, c("start", "end")))
#
#  for (i in seq_len(n)) {
#    backwardPos <- i
#    forwardPos <- i
#
#    while (backwardPos > 1 && abs(x[i] - x[backwardPos - 1]) <= halfSize) {
#      backwardPos <- backwardPos - 1
#    }
#    res[i, 1] <- backwardPos
#
#    while (forwardPos < n && abs(x[i] - x[forwardPos + 1]) <= halfSize) {
#      forwardPos <- forwardPos + 1
#    }
#    res[i, 2] <- forwardPos
#  }
#
#  return(res)
# }
#' Map SNP positions to ranks within a genomic window
#'
#' Determines the start and end positions of SNPs within a sliding window along a chromosome.
#' Uses a two-pointer approach for efficiency.
#'
#' @param x A numeric vector of sorted SNP positions.
#' @param windowSize The size of the genomic window in base pairs.
#'
#' @details New from Inchworm by Stuart J.E. Baird.
#'  Implemented from diemr 1.4.2
#'
#' @return A matrix with two columns:
#'   - `start`: The index of the leftmost SNP within the window.
#'   - `end`: The index of the rightmost SNP within the window.
#'
#' @keywords internal
rank2mapChr <- function(x, windowSize) {
  lengthX <- length(x)
  reach <- windowSize / 2
  focus <- 1
  left <- 1
  right <- 1
  lesseq <- x[1] - 1
  ans <- matrix(0, nrow = lengthX, ncol = 2)

  for (xfocus in x) {
    if (lesseq > xfocus) stop("unsorted argument")

    while (x[right] < xfocus + reach && right < lengthX) {
      right <- right + 1
    }

    if (x[right] > xfocus + reach) {
      right <- right - 1
    }

    while (x[left] + reach < xfocus && left < focus) {
      left <- left + 1
    }

    ans[focus, ] <- c(left, right)
    lesseq <- xfocus
    focus <- focus + 1
  }

  return(ans)
}



#' Compute truncated Laplace distribution weights
#'
#' Evaluates a Laplace distribution truncated to 95% of its area, with weights scaled to `10/19`.
#'
#' @param x A vector of integers representing centered SNP positions.
#' @param laplaceScale Scale parameter of the Laplace distribution.
#'
#' @details `x` must be within `+- laplaceScale * log(20)`.
#' @return A numeric vector of weights.
#' @keywords internal
truncatedLaplace <- function(x, laplaceScale) {
  if (min(x) < -laplaceScale * log(20)) {
    stop("Minimum value of x for the trunctated Laplace distribution needs to be ", -laplaceScale * log(20))
  }
  if (max(x) > laplaceScale * log(20)) {
    stop("Maximum value of x for the trunctated Laplace distribution needs to be ", laplaceScale * log(20))
  }
  laplaceWeights <- ifelse(x >= 0,
    (10 / 19) * exp(-x / laplaceScale),
    (10 / 19) * exp(x / laplaceScale)
  )
  return(laplaceWeights)
}


#' Select the weighted mode of genomic states
#'
#' Determines the most frequent genomic state in a given interval, weighted by Laplace-scaled probabilities.
#'
#' @param genomicStates A character vector of genomic states.
#' @param laplaceWeights A numeric vector of weights.
#'
#' @details In case of ties, states are ranked by:
#'   1. Total sum of weights (higher is preferred).
#'   2. Highest single weight assigned to any occurrence of the state (higher is preferred).
#'   3. Lexicographic order (earlier in the alphabet is preferred).
#'
#' @return A character representing the most probable genomic state.
#' @keywords internal
unbiasedWeightedStateChoice <- function(genomicStates, laplaceWeights) {
  stateSummary <- aggregate(laplaceWeights ~ genomicStates,
    FUN = sum, na.rm = TRUE
  )
  stateSummary[, "max"] <- aggregate(laplaceWeights ~ genomicStates,
    FUN = max, na.rm = TRUE
  )[, 2]

  stateSummary <- stateSummary[order(stateSummary[, 2], stateSummary[, 3], stateSummary[, 1], decreasing = TRUE), ]

  return(stateSummary[1, 1])
}
