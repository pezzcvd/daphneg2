test_that("nc.input is integer", {
  par1 = 1
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("nc.input is boolean", {
  par1 = T
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("nc.input is longer vector", {
  par1 = c("CO_Spring", "CO_Summer")
  par3 = environmental
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})



test_that("nc.table is integer", {
  par1 = "CO_Spring"
  par3 = 1
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("nc.table is boolean", {
  par1 = "CO_Spring"
  par3 = T
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("nc.table is string", {
  par1 = "CO_Spring"
  par3 = "environmental"
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("nc.table is wrong table", {
  par1 = "CO_Spring"
  par3 = ecotypes
  par4 = environmental$CO_Spring[1:50]
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})



test_that("xp is single integer", {
  par1 = "CO_Spring"
  par3 = environmental
  par4 = 1
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("xp is boolean", {
  par1 = "CO_Spring"
  par3 = environmental
  par4 = T
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("xp is string", {
  par1 = "CO_Spring"
  par3 = environmental
  par4 = "ciao"
  par5 = environmental$ET[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})



test_that("xn is single integer", {
  par1 = "CO_Spring"
  par3 = environmental
  par5 = 1
  par4 = environmental$CO_Spring[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("xn is boolean", {
  par1 = "CO_Spring"
  par3 = environmental
  par5 = T
  par4 = environmental$CO_Spring[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})

test_that("xn is string", {
  par1 = "CO_Spring"
  par3 = environmental
  par5 = "ciao"
  par4 = environmental$CO_Spring[1:50]
  testthat::expect_error(object = nCov(par1,  par3, par4, par5))
})
