library(testthat)
library(MapAgora)

context("Parse Twitter handle from page")

test_that("MoveOn example check", {
    expect_equal(parse_twitter_handle_from_page("https://www.moveon.org"), "moveon")
})

test_that("Filtering rule for twitter links should be specific enough", {
    expect_equal(parse_twitter_handle_from_page("https://www.chshartford.org/"), "chshartford")
})
