library(testthat)
library(MapAgora)

context("Get about page content")

test_that("Extract about links should not return NA (with tree search)", {

    # Test 1
    expect_equal(class(get_about_page_content("http://www.saltpondscoalition.org/")), "data.frame")

    # Test 2
    expect_equal(class(get_about_page_content("https://front.moveon.org")), "data.frame")
})

test_that("Get about page content returns a data.frame (without tree search)", {
    expect_equal(class(get_about_page_content("https://snfagora.jhu.edu/")), "data.frame")
})

#test_that("Get about page from a website built by PHP", {
#  expect_equal(class(get_about_page_content("http://www.egacademyfoundation.org")), "character")
#}) # This website no longer exists.

test_that("Get about page from a website that doesn't use about to identify about page", {
    expect_equal(class(get_about_page_content("https://www.tomorrowfund.org")), "data.frame")
})

test_that("Get about from a website built by Wix", {

    expect_equal(class(get_about_page_content("https://acarts.org")), "data.frame")

    expect_equal(class(get_about_page_content("https://www.gbsa.info/")), "data.frame")

})

test_that("Check out timeout filtering", {
    expect_error(get_about_page_content("http://lungbanksofamerica.org/", 3))
})

test_that("Check whether stripping / is done properly", {
    expect_equal(class(get_about_page_content("https://dcecenter.com/")), "data.frame")
})

test_that("Check whether index.html is included in a base URL", {
    expect_equal(class(get_about_page_content("http://fourcornersarts.org/index.html")), "data.frame")
})
