#' Compute hybrid index from genotypes, files, or numeric values
#'
#' Provides a unified way to obtain the hybrid index regardless of input format.
#' The function accepts:
#' * a genotype matrix or data frame (ideally polarised);
#' * a 4-column numeric matrix in the I4 format (summaries of genotype counts);
#' * a file containing hybrid index values produced by `diem`;
#' * one or more genotype files in diem format together with ploidy information;
#' * or a numeric vector of hybrid index values.
#'
#' The function returns a numeric vector of hybrid indices and can optionally
#' subset individuals or rescale the values to the interval 0-1.
#'
#' @param x Either a genotype matrix/data.frame, a path to a text file
#'   containing hybrid indices, one or more genotype files in diem format,
#'   or a numeric vector of hybrid index values.
#' @inheritParams diem
#' @inheritParams importPolarized
#' @param rescale Logical, whether to linearly rescale the resulting hybrid
#'   indices to the interval 0–1. Defaults to `FALSE`.
#'
#' @details
#' Input type is detected automatically:
#'
#' * **Hybrid-index file** - the last column is extracted. The file may optionally
#'   contain the header `"HybridIndex"`. No filtering is applied unless
#'   `ChosenInds` is specified.
#'
#' * **Numeric vector** - values are returned unchanged (except optional
#'   subsetting and rescaling).
#'
#' * **I4 matrix** - a 4-column numeric matrix where each row contains
#'   genotype summary counts. Each row is processed directly by
#'   `pHetErrOnStateCount(row)`.
#'
#' * **Genotype matrix** - typically polarised genotypes from `importPolarized`.
#'   Each row is converted to state counts via `sStateCount()` and then passed to
#'   `pHetErrOnStateCount()`.
#'
#' * **Ploidy-aware multi-file input** - if `x` is a character vector of files
#'   and `ploidy` and `changePolarity` are supplied, ploidy-aware hybrid indices
#'   are calculated for an optional subset of individuals (`ChosenInds`) and
#'   sites (`ChosenSites`). Note that if the filename in `x` is not supplied together
#'   with `ploidy` and `changePolarity` arguments, the function assumes the file contains
#'   hybrid indices such as those saved by `diem()` to *HIwithOptimalPolarities.txt*.
#'
#' If `rescale = TRUE`, the hybrid index is mapped to \eqn{[0,1]}. If all values
#' are equal or non-finite, the original scale is preserved and a warning issued.
#'
#' Missing values are replaced with `0.5`, reflecting the default hybrid index
#' for samples with no usable genotype information.
#'
#' @return
#' A numeric vector of hybrid index values. Names are not preserved.
#'
#' @seealso \link{pHetErrOnStateCount}, \link{sStateCount}, \link{importPolarized}
#'
#' @examples
#' hybridIndex(c(0.3, 0.5, 0.7))
#' hybridIndex(c(0.3, 0.5, 0.7), rescale = TRUE)
#'
#' hybridIndex(1:10, ChosenInds = 1:5, rescale = TRUE)
#'
#' filepaths <- c(
#'   system.file("extdata", "data7x3.txt", package = "diemr"),
#'   system.file("extdata", "data7x10.txt", package = "diemr")
#' )
#'
#' ploidies <- list(
#'   rep(2, 7),
#'   c(2, 1, 2, 2, 2, 1, 2)
#' )
#'
#' hybridIndex(x = filepaths, ploidy = ploidies, 
#'   changePolarity = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, 
#'                      TRUE, TRUE, FALSE, TRUE, TRUE))
#'
#' @export
hybridIndex <- function(x, ChosenInds = "all", rescale = FALSE, ploidy = NULL, ChosenSites = "all", changePolarity = NULL) {
  # File path
  if (is.character(x) && length(x) == 1L && is.null(ploidy)) {
    if (!file.exists(x)) stop("File not found: ", x)
    hasHeader <- ifelse(grepl("HybridIndex", readLines(x, n = 1)[[1]]), TRUE, FALSE)
    HI <- read.table(x, header = hasHeader)
    HI <- unname(unlist(HI[, ncol(HI)]))
  } else {
    # Numeric vector
    if (is.numeric(x) && is.null(dim(x))) {
      HI <- unname(as.numeric(x))
    } else {
      # Imported genotypes
      if (is.matrix(x) || is.data.frame(x)) {
        gen <- if (is.data.frame(x)) as.matrix(x) else x
        # I4 matrix
        if (ncol(gen) == 4 && is.numeric(gen[1, 1])) {
          HI <- unname(unlist(apply(gen, 1L, function(row) pHetErrOnStateCount(row))[1, ]))
        } else {
          HI <- unname(unlist(apply(gen, 1L, function(row) pHetErrOnStateCount(sStateCount(row)))[1, ]))
        }
      } else {
        # ploidy-aware from file
        if (is.character(x) && !is.null(ploidy) && !is.null(changePolarity)) {
          CheckDiemFormat(files = x, ploidy = ploidy, ChosenInds = ChosenInds, quiet = TRUE)
          if (ChosenInds[1] == "all") {
            ChosenInds <- resolveChosenInds(x[1])
          }
          gen <- importPolarized(x,
            changePolarity = changePolarity,
            ChosenInds = ChosenInds,
            ChosenSites = ChosenSites,
            simplify = FALSE
          )
          keepCompartments <- !vapply(gen, anyNA, logical(1L))
          gen <- gen[keepCompartments]
          ploidy <- ploidy[keepCompartments]
          I4 <- lapply(gen, FUN = function(x) t(apply(x, 1, sStateCount)))
          if (ChosenInds[1] == "all") {
            A4compartments <- Map("*", I4, ploidy)
          } else {
            A4compartments <- Map("*", I4, lapply(ploidy, "[", ChosenInds))
          }
          A4 <- Reduce("+", A4compartments)

          HI <- unname(unlist(apply(A4, 1L, function(row) pHetErrOnStateCount(row))[1, ]))
        } else {
          stop("Unsupported input. Check ?hybridIndex for supported input data.")
        }
      }
    }
  }
  # subset ChosenInds
  if (length(ChosenInds) > 0L && ChosenInds[1] != "all" && is.null(ploidy)) {
    if (any(ChosenInds < 1L | ChosenInds > length(HI))) {
      stop("ChosenInds out of bounds for length ", length(HI), ".")
    }
    HI <- HI[ChosenInds]
  }

  # rescale
  if (isTRUE(rescale)) {
    r <- range(HI, na.rm = TRUE)
    if (is.finite(r[1]) && is.finite(r[2]) && r[1] != r[2]) {
      HI <- (HI - r[1]) / (r[2] - r[1])
    } else {
      warning("Rescale requested but all values equal or non-finite; returning unscaled hybrid indices.")
    }
  }
  HI[is.na(HI)] <- 0.5
  return(HI)
}
