library(testthat)
library(MapAgora)

context("Standaridze an organization's website address (URL)")

test_that("MoveOn example check", {
    expect_equal(grepl("http", standardize_url("www.moveon.org")), TRUE)
})

context("Reindexing EINs and tax periods")

test_that("Whether indexing works", {
    expect_equal(class(get_aws_url(ein = "311810938", year = 2019))[2], "character")
})
