test_that("error on data type of sCount", {
  expect_error(
    object = pHetErrOnStateCount(sCount = c("a", 10, 10, 10)),
    regexp = "sCount contains non numeric data"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = data.frame(a = "a", b = 10, c = 10, d = 10)),
    regexp = "sCount contains non numeric data"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = data.frame(a = c("a", 10, 10, 10))),
    regexp = "sCount contains non numeric data"
  )
})


test_that("error on sCount vector length", {
  expect_error(
    object = pHetErrOnStateCount(sCount = c("a", 10, 10)),
    regexp = "sCount length is not 4"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = data.frame(a = "a", b = 10, c = 10)),
    regexp = "sCount length is not 4"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = data.frame(a = c("a", 10, 10))),
    regexp = "sCount length is not 4"
  )


  expect_error(
    object = pHetErrOnStateCount(sCount = c(10, 10, 10)),
    regexp = "sCount length is not 4"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = data.frame(a = 10, b = 10, c = 10)),
    regexp = "sCount length is not 4"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = data.frame(a = c(10, 10, 10))),
    regexp = "sCount length is not 4"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = matrix(c(10, 10, 10))),
    regexp = "sCount length is not 4"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = matrix(c(10, 10, 10, 10, 10, 10), 2)),
    regexp = "sCount length is not 4"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = NULL),
    regexp = "sCount length is not 4"
  )
})


test_that("error on NA in sCount", {
  expect_error(
    object = pHetErrOnStateCount(sCount = c(NA, 10, 10, 10)),
    regexp = "sCount contains NA"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = data.frame(a = NA, b = 10, c = 10, d = 10)),
    regexp = "sCount contains NA"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = data.frame(a = c(NA, 10, 10, 10))),
    regexp = "sCount contains NA"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = matrix(c(NA, 10, 10, 10))),
    regexp = "sCount contains NA"
  )

  expect_error(
    object = pHetErrOnStateCount(sCount = matrix(c(NA, 10, 10, 10), 2)),
    regexp = "sCount contains NA"
  )
})



test_that("numeric results", {
  expect_equal(
    ignore_attr = TRUE,
    object = pHetErrOnStateCount(sCount = c(20, 20, 20, 20)),
    expected = c(.5, .333, .25),
    tolerance = 1e-2
  )

  expect_equal(
    ignore_attr = TRUE,
    object = pHetErrOnStateCount(sCount = c(10, 10, 10, 20)),
    expected = c(.62, .25, .2),
    tolerance = 1e-2
  )

  expect_equal(
    ignore_attr = TRUE,
    object = pHetErrOnStateCount(sCount = c(10, 10, 20, 10)),
    expected = c(.5, .5, .2),
    tolerance = 1e-2
  )
})
