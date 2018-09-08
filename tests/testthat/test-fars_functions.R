context("fars_functions")

test_that("make_filename works", {
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})
