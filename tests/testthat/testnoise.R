library(rpatrec)
context("Output_of_noise")

test_that("output length is the same as input length",{
  a <- seq(1:100)
  expect_equal(length(noise(a,"white",2)),length(a))

})
