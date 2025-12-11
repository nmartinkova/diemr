test_that("error on data type in hybridIndex", {
  expect_error(
    object = hybridIndex(x = TRUE),
    regexp = "Unsupported input"
  )

  expect_error(
    object = hybridIndex(x = list(TRUE, FALSE)),
    regexp = "Unsupported input"
  )

  expect_error(
    object = hybridIndex(x = LETTERS[1:3]),
    regexp = "Unsupported input"
  )

  expect_error(
    object = hybridIndex(x = c("1", "0", "2")),
    regexp = "Unsupported input"
  )

  expect_error(
    object = hybridIndex(x = matrix(1:20, ncol = 4), ChosenInds = 2:6),
    regexp = "out of bounds"
  )

  expect_error(
    object = hybridIndex(x = matrix(1:20, ncol = 4), ChosenInds = "every"),
    regexp = "out of bounds"
  )
})


test_that("correct solution of hybridIndex", {
  expect_equal(
    object = hybridIndex(1:3),
    expected = 1:3
  )

  expect_equal(
    object = hybridIndex(matrix(1:20, ncol = 4), rescale = TRUE),
    expected = c(1, .69, .42, .2, 0), tolerance = 1e-2
  )

  expect_equal(
    object = hybridIndex(matrix(1:20, ncol = 4), ChosenInds = 1:3, rescale = TRUE),
    expected = c(1, .46, 0), tolerance = 1e-2
  )

  dat <- diemr:::sImport(system.file("extdata", "testBarrier.txt", package = "diemr"))

  expect_equal(
    object = hybridIndex(dat, ChosenInds = 2:4),
    expected = c(0.68, 0.5, 0.32), tolerance = 1e-2
  )

  expect_equal(
    object = hybridIndex(dat),
    expected = c(0.27, 0.68, 0.5, 0.32, 0.42, 0.5), tolerance = 1e-2
  )

  expect_equal(
    object = hybridIndex(dat, ChosenInds = 2:4, rescale = TRUE),
    expected = c(1, 0.5, 0), tolerance = 1e-2
  )

  expect_warning(
    object = hybridIndex(c(1, 1), rescale = TRUE),
    regexp = "values equal"
  )

  expect_warning(
    object = hybridIndex(c(1, NA), rescale = TRUE),
    regexp = "values equal"
  )
})


test_that("ploidy-aware hybridIndex errors on ploidy length mismatch", {
  filepaths <- c(
    system.file("extdata", "data7x3.txt", package = "diemr"),
    system.file("extdata", "data7x10.txt", package = "diemr")
  )

  wrong_ploidies <- list(rep(2, 6), rep(2, 7))

  expect_error(
    hybridIndex(
      x = filepaths,
      ploidy = wrong_ploidies,
      changePolarity = rep(TRUE, 13)
    ),
    regexp = "Ploidy"
  )
})

test_that("ploidy-aware multi-file input matches manual I4 computation", {
  filepaths <- c(
    system.file("extdata", "data7x3.txt", package = "diemr"),
    system.file("extdata", "data7x10.txt", package = "diemr")
  )

  ploidies <- list(
    rep(2, 7),
    c(2, 1, 2, 2, 2, 1, 2)
  )

  changePolarity <- c(
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, TRUE, TRUE, FALSE, TRUE, TRUE
  )

  # manual construction of A4 (mirrors hybridIndex internals)
  CheckDiemFormat(files = filepaths, ploidy = ploidies, ChosenInds = "all")

  gen_list <- importPolarized(
    files = filepaths,
    changePolarity = changePolarity,
    ChosenInds = "all",
    ChosenSites = "all",
    simplify = FALSE
  )

  keepCompartments <- !vapply(gen_list, anyNA, logical(1L))
  gen_list <- gen_list[keepCompartments]
  ploidies <- ploidies[keepCompartments]

  I4_list <- lapply(gen_list, function(x) t(apply(x, 1L, sStateCount)))
  A4compartments_manual <- Map("*", I4_list, ploidies)
  A4_manual <- Reduce("+", A4compartments_manual)

  HI_manual <- apply(A4_manual, 1L, function(row) {
    pHetErrOnStateCount(row)[1L]
  })

  HI_wrapper <- hybridIndex(
    x = filepaths,
    ploidy = ploidies,
    changePolarity = changePolarity
  )

  expect_equal(
    object = HI_wrapper,
    expected = as.numeric(HI_manual),
    tolerance = 1e-5
  )
})


test_that("ploidy-aware hybridIndex respects ChosenInds subsetting", {
  filepaths <- c(
    system.file("extdata", "data7x3.txt", package = "diemr"),
    system.file("extdata", "data7x10.txt", package = "diemr")
  )

  ploidies <- list(
    rep(2, 7),
    c(2, 1, 2, 2, 2, 1, 2)
  )

  changePolarity <- c(
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, TRUE, TRUE, FALSE, TRUE, TRUE
  )

  hi_all <- hybridIndex(
    x = filepaths,
    ploidy = ploidies,
    changePolarity = changePolarity,
    ChosenInds = "all"
  )

  hi_subset <- hybridIndex(
    x = filepaths,
    ploidy = ploidies,
    changePolarity = changePolarity,
    ChosenInds = 2:5
  )

  expect_equal(hi_subset, hi_all[2:5], tolerance = 1e-5)
})
