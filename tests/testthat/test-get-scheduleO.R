library(testthat)
library(MapAgora)

#idx <- import_idx(2019)
#usethis::use_data(idx, internal = TRUE) # Stores all data in a single object

test_that("Check whether the exact document source is provided when inspecting Schedule O", {

    expect_equal(unique(grepl("From FORM", get_scheduleO("061553389"))), TRUE)

})

test_that("Check whether get_scheduleO returns all entries", {

    expect_equal(length(grepl("From FORM", (get_scheduleO("061553389")), TRUE)) >= 2, TRUE)

})
