library(testthat)
library(MapAgora)

test_that("Check relative link cases", {
  expect_equal(find_about_link("http://www.saltpondscoalition.org/"), "http://www.saltpondscoalition.org/AboutUs.html")
})

test_that("Check absolute link cases", {

  # Case 1
  expect_equal(find_about_link("https://www.hhsyc.org/"), "http://hhsyc.org/About_HHSYC/AboutUs.html")

  # Case 2
  expect_equal(find_about_link("http://www.mythsandfacts.com"), "http://www.mythsandfacts.org/content/about.asp")
})
