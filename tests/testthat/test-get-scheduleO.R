library(testthat)
library(MapAgora)

context("Check whether parsing ScheduleO works. The default year is 2019.")

test_that("Check whether the exact document source is provided when inspecting Schedule O", {

    expect_equal(
        unique(grepl("From FORM", get_scheduleO("061553389"))),
        TRUE)

})

test_that("Check whether get_scheduleO returns all entries", {

    expect_equal(length(grepl("From FORM", (get_scheduleO("061553389")), TRUE)) >= 2, TRUE)

})

context("Check whether parsing ScheduleO works. This test is based on 2018.")

test_that("Check whether get_scheduleO returns all entries from the 2018 IRS data", {

    expect_equal(class(get_scheduleO("611085915", 2018))[2], "character")

})
