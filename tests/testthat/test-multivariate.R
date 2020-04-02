

test_that("mv.input is integer", {
  par1 = 1
  par2 = "all"
  par3 = environmental
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.input is boolean", {
  par1 = T
  par2 = "all"
  par3 = environmental
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.input is wrong vector", {
  par1 = "ciao"
  par2 = "all"
  par3 = environmental
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.input is longer vector", {
  par1 = c("CO_Spring", "CO_Spring")
  par2 = "all"
  par3 = environmental
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})



test_that("mv.par is integer", {
  par1 = "CO_Spring"
  par2 = 1
  par3 = environmental
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.par is boolean", {
  par1 = "CO_Spring"
  par2 = T
  par3 = environmental
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.par is wrong vector", {
  par1 = "CO_Spring"
  par2 = "ciao"
  par3 = environmental
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.par is longer vector", {
  par1 = "CO_Spring"
  par2 = c("ET", "all")
  par3 = environmental
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})




test_that("mv.pheno is integer", {
  par1 = "CO_Spring"
  par2 = "all"
  par3 = 1
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.pheno is boolean", {
  par1 = "CO_Spring"
  par2 = "all"
  par3 = T
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.pheno is string", {
  par1 = "CO_Spring"
  par2 = "all"
  par3 = "environmental"
  par4 = F
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})




test_that("mv.div is integer", {
  par1 = "CO_Spring"
  par2 = "all"
  par3 = environmental
  par4 = 1
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.div is string", {
  par1 = "CO_Spring"
  par2 = "all"
  par3 = environmental
  par4 = "ciao"
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})

test_that("mv.div is longer vector", {
  par1 = "CO_Spring"
  par2 = "all"
  par3 = environmental
  par4 = c(F, F)
  testthat::expect_error(object = multivariate(par1, par2, par3, par4))
})




test_that("mv.env_altern is integer", {
  par1 = "100_4W"
  par2 = "CO_Spring"
  par3 = phenotypical
  par4 = T
  par5 = 1
  testthat::expect_error(object = multivariate(par1, par2, par3, par4, par5))
})

test_that("mv.env_altern is boolean", {
  par1 = "100_4W"
  par2 = "CO_Spring"
  par3 = phenotypical
  par4 = T
  par5 = T
  testthat::expect_error(object = multivariate(par1, par2, par3, par4, par5))
})

test_that("mv.env_altern is string", {
  par1 = "100_4W"
  par2 = "CO_Spring"
  par3 = phenotypical
  par4 = T
  par5 = "environmental"
  testthat::expect_error(object = multivariate(par1, par2, par3, par4, par5))
})

test_that("mv.env_altern is wrong table", {
  par1 = "100_4W"
  par2 = "CO_Spring"
  par3 = phenotypical
  par4 = T
  par5 = phenotypical
  testthat::expect_error(object = multivariate(par1, par2, par3, par4, par5))
})
