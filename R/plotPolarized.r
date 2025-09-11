#' Plot Polarized Genotypes
#'
#' Plots genotypes that can be optionally polarized and visualized as either a rectangular
#' or circular (iris-style) plot.
#'
#' @param genotypes A character matrix with _012 encoding of genotypes. Rows represent
#'   individuals, columns represent markers.
#' @param HI A numeric vector of hybrid indices, one per individual (row in 
#'   \code{genotypes}).
#' @param cols A vector of four colors representing: missing data, homozygotes for 0,
#'   heterozygotes, and homozygotes for 2 (in that order).
#' @param type Character string specifying the layout of the plot. Accepted values are
#'   \code{"rectangle"}, \code{"iris"}, or \code{"circular"}. Partial matching is allowed.
#' @param showProgress Logical. If \code{TRUE}, prints a percentage indicator as individuals
#'   are plotted (applies only to iris plots).
#' @param addMarkerAxis Logical. If \code{TRUE}, a default marker axis is added below the
#'   genotype matrix (in rectangular layout) or as a radial axis (in circular layout).
#'   Requires either \code{includedSites} or \code{axisInfo}. For full customization of the
#'   axis (e.g., styling, label orientation), use \code{plotMarkerAxis} manually.
#' @param ... Additional arguments passed to internal plotting functions. See Details.
#'
#' @details To import and polarize genotypes, use \link{importPolarized}.
#'
#' When using \link{diem}, the hybrid indices (\code{HI}) are saved in the file
#' \code{"HIwithOptimalPolarities.txt"}. Alternatively, you can compute \code{HI} directly
#' from polarized genotypes (see Examples).
#'
#' By default, the function plots colored tick marks for individuals, with a color change at
#' the steepest hybrid index gradient. The second and fourth colors in \code{cols} are used
#' for these ticks. You can:
#' * Disable the ticks using \code{tick = FALSE},
#' * Provide a vector of tick colors (must match the number of individuals), 
#'   **ordered according to \code{order(HI)}**,
#' * Provide individual \code{labels} (e.g., accession numbers) in the same order as the
#'   rows in \code{genotypes}.
#'
#' If \code{addMarkerAxis = TRUE}, you must also supply either:
#' * \code{includedSites} — a file path containing columns \code{CHROM} and \code{POS}, or
#' * \code{axisInfo} — a precomputed axis information list, as described in
#'   \code{\link{plotMarkerAxis}}.

#'
#' Additional graphical parameters are passed to internal calls to \code{image()},
#' \code{axis()}, and \code{plotMarkerAxis()}. The following arguments are supported:
#'
#' \strong{Image arguments:} \code{zlim}, \code{xlim}, \code{ylim}, \code{add}, \code{xaxs},
#' \code{yaxs}, \code{xlab}, \code{ylab}, \code{breaks}, \code{useRaster}, \code{asp},
#' \code{cex}, \code{cex.lab}, \code{cex.main}, \code{cex.sub}, \code{axes},
#' \code{col.axis}, \code{cex.axis}, \code{family}, \code{font}, \code{font.axis},
#' \code{font.lab}, \code{font.main}, \code{font.sub}, \code{lab}, \code{xpd}.
#'
#' \strong{Axis arguments:} \code{side}, \code{at}, \code{col.ticks}, \code{labels},
#' \code{las}, \code{tick}, \code{line}, \code{pos}, \code{outer}, \code{font},
#' \code{lty}, \code{lwd}, \code{lwd.ticks}, \code{hadj}, \code{padj}, \code{gap.axis},
#' \code{xpd}, \code{cex.axis}.
#'
#' @return No return value. Called for its side effects - a visual plot of polarized genotypes.
#' In the default color scheme:
#' * Purple and teal represent homozygotes (`0` and `2`),
#' * Yellow represents heterozygotes (`1`),
#' * White indicates missing or undetermined genotypes (`_`).
#' Individuals are ordered by increasing \code{HI} (bottom up in rectangular plots and 
#' inside out in circular plots).
#'
#' @seealso \link{plotMarkerAxis} for adding chromosome information as a custom
#'   axis to the rectangle genotype plot.
#' @importFrom graphics image axis
#' @importFrom grDevices col2rgb
#' @importFrom utils modifyList flush.console
#' @import circlize
#' @export
#' @examples
#' gen <- importPolarized(
#'   file = system.file("extdata", "data7x10.txt", package = "diemr"),
#'   changePolarity = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
#'   ChosenInds = 1:7
#' )
#'
#' h <- apply(gen, 1, FUN = function(x) pHetErrOnStateCount(sStateCount(x)))[1, ]
#'
#' plotPolarized(genotypes = gen, HI = h)
#'
#' # Incorrect tick color order
#' plotPolarized(gen, h, col.ticks = c(rep("purple", 5), "green", "purple"), lwd = 3)
#'
#' # Correct tick color order
#' plotPolarized(gen, h, col.ticks = c(rep("purple", 5), "green", "purple")[order(h)], lwd = 3)
#'
#' # Correct individual label order
#' plotPolarized(gen, h, labels = c(paste("purple", 1:5), "green 1", "purple 6"))
#' plotPolarized(gen, h, labels = c(paste("purple", 1:5), "green 1", "purple 6"), type = "iris")
plotPolarized <- function(
    genotypes, HI, cols = c("#FFFFFF", "#800080", "#FFE500", "#008080"),
    type = "rectangle", showProgress = FALSE, addMarkerAxis = FALSE, ...) {
  userArgs <- list(...)
  type <- match.arg(tolower(type), choices = c("rectangle", "iris", "circular"))

  # check arguments
  nMarkers <- ncol(genotypes)
  nInds <- nrow(genotypes)

  # check colors
  areColors <- sapply(cols, function(X) tryCatch(is.matrix(col2rgb(X)), error = function(e) FALSE))
  if (sum(areColors) != 4) {
    warning("Argument cols must contain four valid colors. Review the input, now using default colors")
    cols <- c("#FFFFFF", "#800080", "#FFE500", "#008080")
  }

  # check HI
  HI <- unname(unlist(HI))
  if (length(HI) != nInds) stop("Provide HI for all genotypes. Now ", length(HI), " HI for ", nInds, " genotypes.")



  # plotting arguments
  plottingArgs <- utils::modifyList(list(
    # image defaults
    xlab = "Markers",
    ylab = "Individuals",
    axes = FALSE,
    useRaster = TRUE,
    breaks = 0:4,
    # axis defaults
    side = 2,
    at = 1:nInds,
    col.ticks = c(rep(cols[2], which.max(diff(sort(HI)))), rep(cols[4], length(HI) - which.max(diff(sort(HI))))),
    labels = "",
    las = 1,
    lwd = 2,
    # circlize defaults
    track.height = 0.55,
    start.degree = 90,
    gap.after = 20,
    tick.length = 5,
    # markerAxis defaults
    includedSites = NULL,
    ChosenSites = "all",
    axisInfo = NULL,
    tickDist = 1e6
  ), userArgs)
  acceptedImageArgs <- c(
    "zlim", "xlim", "ylim", "add", "xaxs", "yaxs", "xlab", "ylab", "breaks",
    "useRaster", "asp", "cex", "cex.lab", "cex.main", "cex.sub", "axes", "col.axis",
    "cex.axis", "family", "font", "font.axis", "font.lab", "font.main", "font.sub", "lab",
    "xpd"
  )
  acceptedAxisArgs <- c(
    "side", "at", "col.ticks", "labels", "las", "tick", "line",
    "pos", "outer", "font", "lty", "lwd", "lwd.ticks", "hadj", "padj", "gap.axis",
    "xpd", "cex.axis"
  )
  acceptedCirclizeArgs <- c(
    "track.height", "start.degree", "gap.after", "tick.length"
  )
  acceptedMarkerAxisArgs <- c(
    "includedSites", "ChosenSites", "axisInfo", "tickDist"
  )
  imageArgs <- plottingArgs[names(plottingArgs) %in% acceptedImageArgs]
  axisArgs <- plottingArgs[names(plottingArgs) %in% acceptedAxisArgs]
  circlizeArgs <- plottingArgs[names(plottingArgs) %in% acceptedCirclizeArgs]
  markerAxisArgs <- plottingArgs[names(plottingArgs) %in% c(acceptedAxisArgs, acceptedMarkerAxisArgs)]
  if (length(axisArgs$labels) > 1) {
    axisArgs$labels <- axisArgs$labels[order(HI)]
    imageArgs$ylab <- ""
    circlizeArgs$labels <- rev(axisArgs$labels)
  }

  # check markerAxis
  if (addMarkerAxis) {
    if (is.null(markerAxisArgs$includedSites) && is.null(markerAxisArgs$axisInfo)) {
      warning("To plot marker axis, provide either 'axisInfo' or 'includedSites' and 'ChosenSites' identical to those used to subset 'genotypes'. Marker axis will not be plotted.")
      addMarkerAxis <- FALSE
    }
    imageArgs$xlab <- ""
  }

  # order genotypes
  if(type == "rectangle"){
  genotypes <- genotypes[order(HI), ]
  } else {
  genotypes <- genotypes[rev(order(HI)), ]
  axisArgs$col.ticks <- rev(axisArgs$col.ticks)
  }

  # plot rectangular plot
  if (type == "rectangle") {
    do.call(image, c(list(x = 1:nMarkers, y = 1:nInds, z = t(matrix(
      as.numeric(factor(genotypes,
        levels = c("_", "0", "1", "2")
      )),
      ncol = nMarkers
    )), col = cols), imageArgs))


    if (length(axisArgs$col.ticks) == 1 && length(axisArgs$labels) == 1) {
      do.call(axis, modifyList(axisArgs, list(labels = rep(axisArgs$labels, length(axisArgs$at)))))
    } else if (length(axisArgs$col.ticks) == 1 && length(axisArgs$labels) > 1) {
      for (i in seq_along(axisArgs$labels)) {
        do.call(axis, modifyList(axisArgs, list(at = axisArgs$at[i], labels = axisArgs$labels[i], col.ticks = axisArgs$col.ticks)))
      }
    } else if (length(axisArgs$col.ticks) > 1 && length(axisArgs$labels) == 1) {
      for (i in seq_along(axisArgs$col.ticks)) {
        do.call(axis, modifyList(axisArgs, list(col.ticks = axisArgs$col.ticks[i], at = axisArgs$at[i], labels = axisArgs$labels)))
      }
    } else if (length(axisArgs$col.ticks) > 1 && length(axisArgs$labels) > 1) {
      for (i in seq_along(axisArgs$col.ticks)) {
        do.call(axis, modifyList(axisArgs, list(col.ticks = axisArgs$col.ticks[i], at = axisArgs$at[i], labels = axisArgs$labels[i])))
      }
    }
  }

  # plot iris plot
  if (type == "iris" || type == "circular") {
    # initialize iris plot
    if (length(circlizeArgs$labels) == 1) circlizeArgs$labels <- rep(axisArgs$labels, nInds)

    circlize::circos.clear()
    circlize::circos.par(
      start.degree = circlizeArgs$start.degree,
      gap.after = circlizeArgs$gap.after,
      track.height = circlizeArgs$track.height / nInds, track.margin = c(0, 0),
      points.overflow.warning = FALSE,
      cell.padding = rep(0, 4)
    )
    circlize::circos.initialize(factors = "RLE", xlim = c(0, nMarkers))

    RLE <- rle(genotypes[1, ])
    RLE$cols <- cols[factor(RLE$values, levels = c("_", "0", "1", "2"))]
    RLE$start <- c(0, cumsum(RLE$lengths[-length(RLE$lengths)]))
    RLE$end <- cumsum(RLE$lengths)

    # polarised genomes
    circlize::circos.track(ylim = c(0, 1), bg.border = NA, panel.fun = function(x, y) {
      circlize::circos.rect(
        xleft = RLE$start,
        xright = RLE$end,
        ybottom = 0,
        ytop = 1,
        col = RLE$cols,
        border = RLE$cols
      )
    })
    circlize::circos.yaxis(side = "left", at = .5, labels = circlizeArgs$labels[1], col = axisArgs$col.ticks[1], lwd = axisArgs$lwd, tick.length = convert_x(circlizeArgs$tick.length, "mm"))

    for (ind in 2:nInds) {
      RLE <- rle(genotypes[ind, ])
      RLE$cols <- cols[factor(RLE$values, levels = c("_", "0", "1", "2"))]
      RLE$start <- c(0, cumsum(RLE$lengths[-length(RLE$lengths)]))
      RLE$end <- cumsum(RLE$lengths)

      circlize::circos.track(ylim = c(0, 1), bg.border = NA, panel.fun = function(x, y) {
        circlize::circos.rect(
          xleft = RLE$start,
          xright = RLE$end,
          ybottom = 0,
          ytop = 1,
          col = RLE$cols,
          border = RLE$cols
        )
      })
      circlize::circos.yaxis(side = "left", at = .5, labels = circlizeArgs$labels[ind], col = axisArgs$col.ticks[ind], lwd = axisArgs$lwd, tick.length = convert_x(circlizeArgs$tick.length, "mm"))
      # progress indicator
      if (showProgress) {
        cat(sprintf("\r%.1f%%", ind * 100 / nInds))
        flush.console()
      }
    }
  }


  # add marker axis
  if (addMarkerAxis) {
    plotMarkerAxis(
      includedSites = markerAxisArgs$includedSites,
      ChosenSites = markerAxisArgs$ChosenSites,
      tickDist = markerAxisArgs$tickDist,
      axisInfo = markerAxisArgs$axisInfo
    )
  }
}
