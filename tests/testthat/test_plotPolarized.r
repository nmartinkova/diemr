test_that("error on arguments in plotPolarized", {
  dat <- importPolarized(
    files = system.file("extdata", "data7x3.txt", package = "diemr"),
    changePolarity = rep(FALSE, 3),
    ChosenInds = 1:2,
    ChosenSites = "all"
  )

  expect_error(
    object = plotPolarized(dat, HI = seq(0, 1, length.out = 3)),
    regexp = "Provide HI for all genotypes"
  )

  expect_warning(
    object = plotPolarized(dat, HI = c(0, 1), cols = rep(TRUE, 4)),
    regexp = "Argument cols must contain four valid colors"
  )

  expect_warning(
    object = plotPolarized(dat, HI = c(0, 1), cols = c("#FFFFFF", "#800080", "#FFE500")),
    regexp = "Argument cols must contain four valid colors"
  )

  expect_warning(
    object = plotPolarized(dat, HI = c(0, 1), addMarkerAxis = TRUE),
    regexp = "Marker axis will not be plotted"
  )

  expect_error(
    object = plotPolarized(dat, HI = c(0, 1), addMarkerAxis = TRUE, axisInfo = list(CHROMbreaks = 1)),
    regexp = "The 'axisInfo' is missing"
  )
})


test_that("error on arguments in plotMarkerAxis", {
  axisInfo <- list(ticksPos = 1:5)
  expect_error(
    object = plotMarkerAxis(axisInfo = axisInfo),
    regexp = "The 'axisInfo' is missing: ticksNames, CHROMbreaks, CHROMnamesPos, CHROMnames"
  )

  expect_error(
    object = plotMarkerAxis(),
    regexp = "Provide both 'includedSites' and 'ChosenSites'"
  )
})
