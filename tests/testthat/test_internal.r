test_that("resolveCompartments", {
  filenames <- c(
    system.file("extdata", "data7x3.txt", package = "diemr"),
    system.file("extdata", "data7x10.txt", package = "diemr")
  )

  expect_equal(
    object = resolveCompartments(files = filenames, toBeCompartmentalized = rep(TRUE, 13)),
    expected = list(rep(TRUE, 3), rep(TRUE, 10)),
    ignore_attr = TRUE
  )

  expect_equal(
    object = resolveCompartments(files = filenames, toBeCompartmentalized = rep(TRUE, 13), compartmentSizes = c(3, 10)),
    expected = list(rep(TRUE, 3), rep(TRUE, 10)),
    ignore_attr = TRUE
  )

  expect_equal(
    object = resolveCompartments(files = filenames, toBeCompartmentalized = "all"),
    expected = list(rep(TRUE, 3), rep(TRUE, 10)),
    ignore_attr = TRUE
  )

  expect_equal(
    object = resolveCompartments(files = filenames, toBeCompartmentalized = "no", compartmentSizes = c(3, 10)),
    expected = list(rep(TRUE, 3), rep(TRUE, 10)),
    ignore_attr = TRUE
  )


  expect_error(
    object = resolveCompartments(, TRUE),
    regexp = "Provide paths to diem input files"
  )

  expect_error(
    object = resolveCompartments(files = filenames, toBeCompartmentalized = list("all")),
    regexp = "toBeCompartmentalized must be"
  )

  expect_error(
    object = resolveCompartments(files = filenames, toBeCompartmentalized = data.frame("all")),
    regexp = "toBeCompartmentalized must be"
  )

  expect_error(
    object = resolveCompartments(files = filenames, toBeCompartmentalized = TRUE),
    regexp = "toBeCompartmentalized does not have the same length"
  )
})


test_that("truncatedLaplace", {
  expect_equal(
    object = truncatedLaplace(x = c(-100, -50, 0, 20, 80), laplaceScale = 50),
    expected = c(0.0712, 0.1936, 0.5263, 0.3528, 0.1062),
    ignore_attr = TRUE,
    tolerance = 0.001
  )
  expect_equal(
    object = truncatedLaplace(x = c(-100, -50, 0, 20, 80), laplaceScale = 5000),
    expected = c(0.516, 0.521, 0.526, 0.524, 0.518),
    ignore_attr = TRUE,
    tolerance = 0.001
  )
  expect_equal(
    object = truncatedLaplace(x = c(-100, -50, 0, 20, 80), laplaceScale = 500000),
    expected = c(0.5262, 0.5262, 0.5263, 0.5263, 0.5262),
    ignore_attr = TRUE,
    tolerance = 0.0001
  )

  expect_error(
    object = truncatedLaplace(x = c(-100, -50, 0, 20, 80), laplaceScale = 0.3),
    regexp = "Minimum value of x"
  )

  expect_error(
    object = truncatedLaplace(x = c(-70, -50, 0, 20, 80), laplaceScale = 25),
    regexp = "Maximum value of x"
  )
})

test_that("unbiasedWeightedStateChoice", {
  gen <- c("1", "_", "1", "2", "_", "0", "2", "_", "0", "_")

  expect_equal(
    object = unbiasedWeightedStateChoice(gen, c(0.0712291, 0.1062613, 0.1585233, 0.2364889, 0.3528000, 0.5263158, 0.3528000, 0.2364889, 0.1585233, 0.1062613)),
    expected = "_"
  )
  expect_equal(
    object = unbiasedWeightedStateChoice(gen, c(0.1063, 0.1585, 0.2365, 0.3528, 0.5263, 0.3528, 0.2365, 0.1585, 0.1063, 0.0712)),
    expected = "_"
  )
  expect_equal(
    object = unbiasedWeightedStateChoice(gen[1:4], c(1, 2, 3, 2)),
    expected = "1"
  )
  expect_equal(
    object = unbiasedWeightedStateChoice(gen[1:4], c(1, 3, 2, 1)),
    expected = "_"
  )
  expect_equal(
    object = unbiasedWeightedStateChoice(gen[1:4], c(1, 3, 2.5, 1)),
    expected = "1"
  )
})


test_that("markerAxis", {
  expect_error(
    object = markerAxis(),
    regexp = "Provide both 'includedSites' and 'ChosenSites'."
  )
  expect_error(
    object = markerAxis(ChosenSites = rep(TRUE, 10)),
    regexp = "Provide both 'includedSites' and 'ChosenSites'."
  )
})
