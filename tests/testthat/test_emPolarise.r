test_that("error on data type of origM in emPolarise", {
  expect_error(
    object = emPolarise(origM = c(0, 10, 10, 10)),
    regexp = "orgiM must be a character vector"
  )

  expect_error(
    object = emPolarise(origM = data.frame(x = as.character(c(0, 10, 10, 10)))),
    regexp = "orgiM must be a character vector"
  )


  expect_error(
    object = emPolarise(origM = as.character(c(0, 10, 10, 10))),
    regexp = "origM must contain only characters"
  )

  expect_error(
    object = emPolarise(origM = c("0", "1", "a")),
    regexp = "origM must contain only characters"
  )
})


test_that("correct solution of emPolarise", {
  expect_identical(
    object = emPolarise(c("_", "1", "0", "2"), TRUE),
    expected = c("_", "1", "2", "0")
  )

  expect_identical(
    object = emPolarise(c("_", "1", "0", "2"), FALSE),
    expected = c("_", "1", "0", "2")
  )
})
