library(testthat)
library(MapAgora)

context("Extract about links")

test_that("Extract about links should not return NA (with tree search)", {
    expect_equal(unique(extract_about_links("http://www.saltpondscoalition.org/")$href), "AboutUs.html")
})

#test_that("Extract about links should not return NA (without tree search)", {
#  expect_equal(unique(extract_about_links("https://snfagora.jhu.edu/")$link), "https://snfagora.jhu.edu/about")
#})

test_that("Expect two columns from the successful case", {
    expect_equal(ncol(extract_about_links("http://saltpondscoalition.org")), 2)
})
