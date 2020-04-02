load("/home/pejo/daphneg2/inst/extdata/example_geno.RData")

test_that("pp.par is integer", {
  par1 = 1
  par2 = "E"
  par3 = genotype
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.par is boolean", {
  par1 = T
  par2 = "E"
  par3 = genotype
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.par is longer vector", {
  par1 = c("CO_Spring", "CO_Summer")
  par2 = "E"
  par3 = genotype
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.par is wrong string", {
  par1 = "ciao"
  par2 = "E"
  par3 = genotype
  testthat::expect_error(object = preproc(par1, par2, par3))
})




test_that("pp.option is integer", {
  par1 = "CO_Spring"
  par2 = 1
  par3 = genotype
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.option is boolean", {
  par1 = "CO_Spring"
  par2 = T
  par3 = genotype
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.option is longer vector", {
  par1 = "CO_Spring"
  par2 = c("E", "P")
  par3 = genotype
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.option is wrong string", {
  par1 = "CO_Spring"
  par2 = "ciao"
  par3 = genotype
  testthat::expect_error(object = preproc(par1, par2, par3))
})




test_that("pp.geno is integer", {
  par1 = "CO_Spring"
  par2 = "E"
  par3 = 1
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.geno is boolean", {
  par1 = "CO_Spring"
  par2 = "E"
  par3 = T
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.geno is string", {
  par1 = "CO_Spring"
  par2 = "E"
  par3 = "genotype"
  testthat::expect_error(object = preproc(par1, par2, par3))
})




test_that("pp.cvt is integer", {
  par1 = "CO_Spring"
  par2 = "E"
  par3 = genotype
  par4 = 1
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.cvt is boolean", {
  par1 = "CO_Spring"
  par2 = "E"
  par3 = genotype
  par4 = T
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.cvt is longer vector", {
  par1 = "CO_Spring"
  par2 = "E"
  par3 = genotype
  par4 = c("ET", "CO_Summer")
  testthat::expect_error(object = preproc(par1, par2, par3))
})

test_that("pp.cvt is wrong string", {
  par1 = "CO_Spring"
  par2 = "E"
  par3 = genotype
  par4 = "ciao"
  testthat::expect_error(object = preproc(par1, par2, par3))
})
