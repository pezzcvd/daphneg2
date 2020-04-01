test_that("oc.input is integer", {
  par1 = 1
  par2 = "ET"
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("oc.input is boolean", {
  par1 = T
  par2 = "ET"
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("oc.input is longer vector", {
  par1 = c("CO_Spring", "CO_Summer")
  par2 = "ET"
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})



test_that("oc.par is integer", {
  par1 = "CO_Spring"
  par2 = 1
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("oc.par is boolean", {
  par1 = "CO_Spring"
  par2 = T
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("oc.par is longer vector", {
  par1 = "CO_Spring"
  par2 = c("ET", "CO_Summer")
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})



test_that("oc.table is integer", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = 1
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("oc.table is boolean", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = T
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("oc.table is string", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = "environmental"
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("oc.table is wrong table", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = ecotypes
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})



test_that("x is single integer", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = environmental
  par4 = 1
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("x is boolean", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = environmental
  par4 = T
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("x is string", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = environmental
  par4 = "ciao"
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})



test_that("xn is single integer", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = environmental
  par5 = 1
  par4 = environmental$CO_Spring[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("xn is boolean", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = environmental
  par5 = T
  par4 = environmental$CO_Spring[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})

test_that("xn is string", {
  par1 = "CO_Spring"
  par2 = "ET"
  par3 = environmental
  par5 = "ciao"
  par4 = environmental$CO_Spring[1:50]
  testthat::expect_error(object = oneCov(par1, par2, par3, par4, par5))
})
