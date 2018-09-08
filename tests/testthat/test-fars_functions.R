context("fars_functions")

test_that("make_filename works", {
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})

test_that("fars_read works as expected", {
  path <- paste0(system.file(package = "fars"), "/extdata")
  good_file <- paste0(path, "/accident_2014.csv.bz2")
  bad_file <- paste0(path, "/accident_2012.csv.bz2")
  expect_error(fars_read(bad_file))
  good_data <- fars_read(good_file)
  expect_equal(dim(good_data), c(30056, 50))
})

