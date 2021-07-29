library(testthat)
library(MapAgora)

test_that("Check whether get_scheduleO returns all entries", {
  expect_equal(length(grepl("From FORM", (get_scheduleO("061553389")), TRUE)) >= 2, TRUE)
})

test_that("Check whether get_scheduleO returns all entries from the 2018 IRS data", {
  expect_equal(class(get_scheduleO("611085915", 2018))[2], "character")
})
