test_that("rank2map returns valid windows for myotis.vcf (windowSize = 100)", {
  myoFile <- system.file("extdata", "myotis.vcf", package = "diemr")
  tmpDir <- tempdir()
  oldWd <- getwd()
  setwd(tmpDir)
  on.exit(setwd(oldWd), add = TRUE)

  vcf2diem(myoFile, "myo")
  bed <- readIncludedSites("myo-includedSites.txt")

  res <- rank2map("myo-includedSites.txt", windowSize = 100)

  expect_equal(nrow(res), nrow(bed))
  expect_equal(ncol(res), 2L)
  expect_true(all(res[, 1] >= 1L))
  expect_true(all(res[, 2] <= nrow(bed)))
  expect_true(all(res[, 1] <= seq_len(nrow(bed))))
  expect_true(all(res[, 2] >= seq_len(nrow(bed))))
  
  expect_equal(
  	object = res,
  	expected = matrix(c(1, 2, 2, 2, 2, 6, 7, 8, 1, 5, 5, 5, 5, 6, 7, 8), 
  		ncol = 2),
  		ignore.attr = TRUE
  )

})

test_that("rank2map respects ChosenSites for myotis.vcf (windowSize = 150)", {
  myoFile <- system.file("extdata", "myotis.vcf", package = "diemr")
  tmpDir <- tempdir()
  oldWd <- getwd()
  setwd(tmpDir)
  on.exit(setwd(oldWd), add = TRUE)

  vcf2diem(myoFile, "myo")
  chosenSites <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)

  res <- rank2map("myo-includedSites.txt", ChosenSites = chosenSites, windowSize = 150)

  expect_equal(nrow(res), sum(chosenSites))
  expect_equal(ncol(res), 2L)
  
    expect_equal(
  	object = res,
  	expected = matrix(c(1, 2, 2, 4, 1, 3, 3, 4), 
  		ncol = 2),
  		ignore.attr = TRUE
  )


})
