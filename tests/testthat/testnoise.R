library(rpatrec)
context("Output_of_noise")

test_that("output given standard inputs",{
  a <- seq(1:100)
  expect_equal(length(noise(a,"white",2)),length(a))
  expect_error(noise("ASD","white",2))
})

test_that("output standard deviation is as required",{
  a <- seq(1:10000)
  b <- noise(a,"white",1)
  c <- sd(b-a)
  d <- (c > 0.85 && c < 1.15)
  expect_true(d)
})
