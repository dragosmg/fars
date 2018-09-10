context("fars_functions")

test_that("make_filename works", {
  expect_match(make_filename(2014), "accident_2014.csv.bz2")
  expect_match(make_filename(2013), "accident_2013.csv.bz2")
})

test_that("fars_read works as expected", {
  path <- paste0(system.file(package = "fars"), "/extdata")
  good_file <- paste0(path, "/accident_2014.csv.bz2")
  bad_file <- paste0(path, "/accident_2012.csv.bz2")
  expect_error(fars_read(bad_file))
  good_data <- fars_read(good_file)
  expect_equal(dim(good_data), c(30056, 50))
})

test_that("fars_read_years works", {
  data <- fars_read_years(2013)
  expect_is(data, "list")
  expect_equal(length(data), 1)
  expect_equal(dim(data[[1]]), c(30202, 2))
  expect_warning(fars_read_years(2012), "invalid year: 2012")
})

test_that("fars_summarize_years() works", {
 summary_2013 <- fars_summarize_years(2013)
 expect_is(summary_2013, "tbl_df")
 expect_equal(dim(summary_2013), c(12, 2))
 expect_equal(names(summary_2013), c("MONTH", "2013"))
})

test_that("fars_map_state", {
  expect_error(fars_map_state(state.num = 70, year = 2013),
               "invalid STATE number: 70")
  expect_null(fars_map_state(11, 2013))
})
