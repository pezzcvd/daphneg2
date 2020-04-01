load("/home/pejo/daphneg2/inst/extdata/example_geno.RData")

test_that("par is not numeric", {
  par1 = "CO_Spring"
  par2 = 123
  testthat::expect_error(univariate(par1,par2, genotype))
})

test_that("par is wrong character", {
  par1 = "CO_Spring"
  par2 = "CIAO"
  testthat::expect_error(univariate(par1,par2,genotype))
})

test_that("par is not logical", {
  par1 = "CO_Spring"
  par2 = T
  testthat::expect_error(univariate(par1,par2,genotype))
})

test_that("par is longer vector", {
  par1 = "CO_Spring"
  par2 = T
  testthat::expect_error(univariate(par1,par2,genotype))
})



test_that("uv.pheno is not numeric", {
  par1 = 123
  par2 = environmental
  testthat::expect_error(univariate(par1,par2,genotype))
})

test_that("uv.pheno is wrong character", {
  par1 = "Ciao"
  par2 = environmental
  testthat::expect_error(univariate(par1,par2,genotype))
})

test_that("uv.pheno is not logical", {
  par1 = T
  par2 = environmental
  testthat::expect_error(univariate(par1,par2,genotype))
})

test_that("uv.pheno is longer vector", {
  par1 = c("CO_Spring", "CO_Summer")
  par2 = environmental
  testthat::expect_error(univariate(par1,par2,genotype))
})
