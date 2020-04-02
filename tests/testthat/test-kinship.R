test_that("k.input is integer", {
  par1 = 1
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  testthat::expect_error(object = kinship(par1, par2))
})

test_that("k.input is boolean", {
  par1 = T
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  testthat::expect_error(object = kinship(par1, par2))
})

test_that("k.input is longer vector", {
  par1 = c("CO_Spring", "CO_Summer")
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  testthat::expect_error(object = kinship(par1, par2))
})




test_that("k.gemma is integer", {
  par1 = "CO_Spring"
  par2 = 1
  testthat::expect_error(object = kinship(par1, par2))
})

test_that("k.gemma is boolean", {
  par1 = "CO_Spring"
  par2 = T
  testthat::expect_error(object = kinship(par1, par2))
})
