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
