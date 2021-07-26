library(testthat)
library(MapAgora)

test_that("MoveOn example check", {
    expect_equal(grepl("http", standardize_url("www.moveon.org")), TRUE)
})

test_that("Whether indexing works", {
    expect_equal(class(get_aws_url(ein = "311810938", year = 2019))[2], "character")
})

test_that("Check data availability (IRS)", {
    expect_equal(ncol(check_data_availability(ein = "311810938", source = "irs")), 4)
})

test_that("Check data availability (website)", {
    expect_equal(ncol(check_data_availability(ein = "311810938", source = "website")), 4)
})

test_that("Check data availability (all)", {
    expect_equal(ncol(check_data_all(ein = "311810938")), 4)
})
