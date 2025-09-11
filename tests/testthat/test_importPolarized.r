test_that("error on arguments in importPolarized", {
  expect_error(
    object = importPolarized(
      files = system.file("extdata", "data7x10.txt", package = "diemr"),
      changePolarity = rep(FALSE, 3),
      ChosenInds = 1:7,
      ChosenSites = "all"
    ),
    regexp = "does not have the same length"
  )

  expect_error(
    object = importPolarized(
      files = system.file("extdata", "data7x10.txt", package = "diemr"),
      changePolarity = rep(FALSE, 10),
      ChosenInds = 1:8,
      ChosenSites = "all"
    ),
    regexp = "contains fewer individuals"
  )
})


test_that("correct solution of importPolarized", {
  local_edition(3)

  expect_equal(
    object = importPolarized(
      files = system.file("extdata", "data7x3.txt", package = "diemr"),
      changePolarity = rep(FALSE, 3),
      ChosenInds = 1:6,
      ChosenSites = "all"
    ),
    expected = matrix(c(
      "0", "0", "1", "1", "2", "2", "1", "2", "1",
      "0", "0", "0", "0", "2", "2", "2", "1", "_"
    ), ncol = 3),
    ignore_attr = TRUE
  )

  expect_equal(
    object = importPolarized(
      files = system.file("extdata", "data7x3.txt", package = "diemr"),
      changePolarity = c(FALSE, FALSE, TRUE),
      ChosenInds = 1:6,
      ChosenSites = "all"
    ),
    expected = matrix(c(
      "0", "0", "1", "1", "2", "2",
      "1", "2", "1", "0", "0", "0",
      "2", "0", "0", "0", "1", "_"
    ), ncol = 3),
    ignore_attr = TRUE
  )

  expect_equal(
    object = importPolarized(
      files = system.file("extdata", "data7x3.txt", package = "diemr"),
      changePolarity = c(TRUE, TRUE, TRUE),
      ChosenInds = 1:6,
      ChosenSites = "all"
    ),
    expected = matrix(c(
      "2", "2", "1", "1", "0", "0",
      "1", "0", "1", "2", "2", "2",
      "2", "0", "0", "0", "1", "_"
    ), ncol = 3),
    ignore_attr = TRUE
  )
})


test_that("correct dimnames in importPolarized", {
  expect_equal(
    object = importPolarized(
      files = system.file("extdata", "data7x3.txt", package = "diemr"),
      changePolarity = c(TRUE, TRUE, TRUE),
      ChosenInds = c(1:2, 4:6),
      ChosenSites = "all"
    ),
    expected = matrix(
      c(
        "2", "2", "1", "0", "0",
        "1", "0", "2", "2", "2",
        "2", "0", "0", "1", "_"
      ),
      ncol = 3,
      dimnames = list(c("1", "2", "4", "5", "6"), c("m1", "m2", "m3"))
    )
  )

  expect_equal(
    object = importPolarized(
      files = system.file("extdata", "data7x3.txt", package = "diemr"),
      changePolarity = c(TRUE, TRUE, TRUE),
      ChosenInds = c(1, 4, 2, 3, 6, 5),
      ChosenSites = "all"
    ),
    expected = matrix(
      c(
        "2", "1", "2", "1", "0", "0",
        "1", "2", "0", "1", "2", "2",
        "2", "0", "0", "0", "_", "1"
      ),
      ncol = 3,
      dimnames = list(c("1", "4", "2", "3", "6", "5"), c("m1", "m2", "m3"))
    )
  )

  expect_equal(
    object = importPolarized(
      files = c(
        system.file("extdata", "data7x3.txt", package = "diemr"),
        system.file("extdata", "data7x10.txt", package = "diemr")
      ),
      changePolarity = c(TRUE, rep(FALSE, 12)),
      ChosenInds = c(1, 5),
      ChosenSites = c(TRUE, FALSE, FALSE, TRUE, rep(FALSE, 9))
    ),
    expected = matrix(
      c(
        "2", "0",
        "1", "2"
      ),
      ncol = 2,
      dimnames = list(c("1", "5"), c("m1", "m4"))
    )
  )
})
