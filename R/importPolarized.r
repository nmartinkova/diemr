#' Imports genomic data polarized according to the specification
#'
#' Reads genotypes from a file and changes marker polarity.
#'
#' @inheritParams diem
#' @param changePolarity A logical vector or a list of logical vectors with length equal
#'   to the number of markers.
#' @param verbose Logical whether to show messages on import progress.
#' @param simplify Logical. If `TRUE`, the function returns a matrix.
#' @param ... Optional numeric vector of \code{compartmentSizes}.
#' @details For details on the input data format, check the \code{file} with
#'   \link{CheckDiemFormat}.
#'
#'   The `changePolarity` argument influences how each marker is imported. Value
#'   `FALSE` means that the marker will be imported as it is saved in the `file`. Value
#'   `TRUE` means that the genotypes encoded as `0` will be imported as `2`, and genotypes
#'   encoded in the `file` as `2` will be imported as `0`.
#' @return Returns a character matrix with rows containing individual genotypes and columns
#'   containing markers. If `simplify = FALSE`, returns a list of matrices corresponding to
#'   compartments in files.
#' @seealso \link{diem} for determining appropriate marker polarity with
#'   respect to a barrier to gene flow.
#' @export
#' @examples
#' dat <- importPolarized(
#'   files <- system.file("extdata", "data7x3.txt", package = "diemr"),
#'   changePolarity = c(FALSE, TRUE, TRUE),
#'   ChosenInds = 1:6,
#'   ChosenSites = "all",
#'   simplify = TRUE
#' )
#' dat
#' #    m1  m2  m3
#' # 1 "0" "1" "2"
#' # 2 "0" "0" "0"
#' # 3 "1" "1" "0"
#' # 4 "1" "2" "0"
#' # 5 "2" "2" "1"
#' # 6 "2" "2" "_"
importPolarized <- function(files, changePolarity, ChosenInds, ChosenSites = "all", nCores = 1, verbose = FALSE, simplify = TRUE, ...) {
  ChosenSites <- resolveCompartments(files = files, toBeCompartmentalized = ChosenSites, ...)
  if (verbose) message("ChosenSites for compartments done ", Sys.time())

  markerLabels <- which(unlist(ChosenSites))

  # check changePolarity type
  if (is.list(changePolarity)) {
    if (!all(vapply(changePolarity, function(x) is.logical(x), logical(1L)))) {
      stop("Argument 'changePolarity' must be a logical vector or a list of logical vectors.")
    }
  } else if (!is.logical(changePolarity)) {
    stop("Argument 'changePolarity' must be a logical vector or a list of logical vectors.")
  }

  changePolarity <- resolveCompartments(files = files, toBeCompartmentalized = changePolarity, ...)
  if (verbose) message("changePolarity for compartments done ", Sys.time())

  allCompartments <- parallel::mclapply(1:length(files),
    mc.cores = nCores,
    FUN = function(i) {
      if (sum(ChosenSites[[i]]) == 0) {
        return(NA)
      }

      genotypes <- sImport(file = files[i], ChosenInds = ChosenInds)
      genotypes <- genotypes[, ChosenSites[[i]], drop = FALSE]

      # polarise chosen markers
      genotypes <- apply(cbind(as.matrix(changePolarity[[i]][ChosenSites[[i]]]), t(genotypes)),
        MARGIN = 1,
        FUN = function(x) emPolarise(origM = x[2:length(x)], changePolarity = as.logical(x[1]))
      )
      return(genotypes)
    }
  )

  if (verbose) message("Importing all compartments done ", Sys.time())

  if (simplify) {
    allCompartments <- Filter(Negate(anyNA), allCompartments)

    allCompartments <- do.call(cbind, allCompartments)
    if (ChosenInds[1] != "all") {
      rownames(allCompartments) <- ChosenInds
    }
    colnames(allCompartments) <- paste0("m", markerLabels)
  }

  return(allCompartments)
}
