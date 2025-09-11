test_that("error on SNP file type", {
  filename <- system.file("extdata", "myotis.vcf", package = "diemr")
  filename2 <- as.matrix(filename)

  expect_error(
    object = vcf2diem(
      SNP = "filename.txt", filename = "filename"
    ),
    regexp = "should have an extension"
  )

  expect_error(
    object = vcf2diem(
      SNP = "filename.vcf", filename = "filename"
    ),
    regexp = "not found"
  )

  expect_error(
    object = vcf2diem(
      SNP = filename2, filename = "filename"
    ),
    regexp = "string with path to a vcf file"
  )
})


test_that("error on missing filename for output", {
  filename <- system.file("extdata", "myotis.vcf", package = "diemr")
  mockobject <- list(filename)

  expect_error(
    object = vcf2diem(
      SNP = filename
    ),
    regexp = "Provide a filename"
  )

  expect_error(
    object = vcf2diem(
      SNP = filename, filename = 1
    ),
    regexp = "Provide a filename"
  )

  expect_error(
    object = vcf2diem(
      SNP = filename, filename = mockobject
    ),
    regexp = "Provide a filename"
  )
})


test_that("incorrect chunk size", {
  filename <- system.file("extdata", "myotis.vcf", package = "diemr")

  expect_snapshot(
    x = vcf2diem(SNP = filename, filename = "test.txt", chunk = 3)
  )

  expect_error(
    object = vcf2diem(SNP = c(filename, filename), filename = "test.txt", chunk = c(1:3)),
    regexp = "Provide a single object"
  )
})


test_that("resolve multiallelic markers", {
  brenthis <- system.file("extdata", "brenthis.vcf", package = "diemr")
  myotis <- system.file("extdata", "myotis.vcf", package = "diemr")

  expect_snapshot(
    x = vcf2diem(SNP = brenthis, filename = "brenthis.txt", chunk = 1)
  )

  expect_snapshot(
    x = vcf2diem(SNP = myotis, filename = "myotis.txt", chunk = 1)
  )
})



test_that("higher number of homozygotes", {
  brenthis <- system.file("extdata", "brenthis.vcf", package = "diemr")
  myotis <- system.file("extdata", "myotis.vcf", package = "diemr")

  expect_snapshot_value(
    x = vcf2diem(SNP = brenthis, filename = "brenthis2.txt", chunk = 1, requireHomozygous = 2),
    style = "deparse"
  )

  expect_snapshot_value(
    x = vcf2diem(SNP = myotis, filename = "myotis2.txt", chunk = 1, requireHomozygous = 2),
    style = "deparse"
  )
})


test_that("incorrect ChosenInds", {
  brenthis <- system.file("extdata", "brenthis.vcf", package = "diemr")
  myotis <- system.file("extdata", "myotis.vcf", package = "diemr")

  expect_warning(
    object = vcf2diem(SNP = brenthis, filename = "brenthis2.txt", chunk = 1, ChosenInds = 1:20),
    regexp = "Converting to diem for all"
  )

  expect_warning(
    object = vcf2diem(SNP = brenthis, filename = "brenthis2.txt", chunk = 1, ChosenInds = rep(TRUE, 20)),
    regexp = "Converting to diem for all"
  )

  expect_warning(
    object = vcf2diem(SNP = brenthis, filename = "brenthis2.txt", chunk = 1, ChosenInds = rep(TRUE, 10)),
    regexp = "Converting to diem for all"
  )
})
