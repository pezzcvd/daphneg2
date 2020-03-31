test_that("s.mode is integer", {
  par1 = 123

  testthat::expect_error(object = synchro(par1, par2, par3))
})

test_that("s.mode is character", {
  par1 = "ciao"

  testthat::expect_error(object = synchro(par1, par2, par3))
})

test_that("s.mode is longer vector", {
  par1 = c(T,T)

  testthat::expect_error(object = synchro(par1))
})

