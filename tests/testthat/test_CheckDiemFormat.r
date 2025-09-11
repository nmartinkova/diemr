test_that("error on file names", {
  expect_error(
    object = CheckDiemFormat(
      files = 4,
      ChosenInds = 1:6,
      ploidy = list(rep(2, 7))
    ),
    regexp = "file argument needs to be a character string"
  )

  expect_error(
    object = CheckDiemFormat(
      files = "dummyfilename.txt",
      ChosenInds = 1:6,
      ploidy = list(rep(2, 7))
    ),
    regexp = "cannot be found"
  )
})


test_that("error on ChosenInds", {
  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:8,
      ploidy = list(rep(2, 7))
    ),
    regexp = "contains fewer individuals than the maximum index specified in ChosenInds"
  )

  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 2:8,
      ploidy = list(rep(2, 7))
    ),
    regexp = "contains fewer individuals than the maximum index specified in ChosenInds"
  )
})


test_that("Ploidy default", {
  expect_equal(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:6,
      ploidy = FALSE
    ),
    expected = TRUE
  )
})


test_that("error on Ploidy", {
  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:6,
      ploidy = TRUE
    ),
    regexp = "Ploidy must be a list"
  )


  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:6,
      ploidy = rep(2, 7)
    ),
    regexp = "Ploidy must be a list"
  )

  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:6,
      ploidy = list(rep(2, 6), rep(1, 6))
    ),
    regexp = "Length of ploidy"
  )

  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:6,
      ploidy = list(rep(2, 5))
    ),
    regexp = "is not a numeric vector of length"
  )

  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:6,
      ploidy = list(rep(2, 8))
    ),
    regexp = "is not a numeric vector of length"
  )

  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:6,
      ploidy = list(c(0:6))
    ),
    regexp = "contain other characters"
  )

  expect_error(
    object = CheckDiemFormat(
      files = system.file("extdata", "data7x3.txt",
        package = "diemr"
      ),
      ChosenInds = 1:6,
      ploidy = list(c(1, 1, 2, 2, 0, 3, 2))
    ),
    regexp = "contain other characters"
  )
})
