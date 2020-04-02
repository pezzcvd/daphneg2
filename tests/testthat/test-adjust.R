test_that("g.input is integer", {
  par1 = 1
  testthat::expect_error(object = adjust(par1))
})

test_that("g.input is boolean", {
  par1 = T
  testthat::expect_error(object = adjust(par1))
})

test_that("g.input is longer vector", {
  par1 = c("CO_Spring", "CO_Summer")
  testthat::expect_error(object = adjust(par1))
})
