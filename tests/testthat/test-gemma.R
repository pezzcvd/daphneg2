test_that("g.input is integer", {
  par1 = 1
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par4 = 0.05
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.input is boolean", {
  par1 = T
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par4 = 0.05
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.input is longer vector", {
  par1 = c("CO_Spring", "CO_Summer")
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par4 = 0.05
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})




test_that("g.gemma is integer", {
  par1 = "CO_Spring"
  par2 = 1
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par4 = 0.05
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.gemma is boolean", {
  par1 = "CO_Spring"
  par2 = T
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par4 = 0.05
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})




test_that("g.annot is integer", {
  par1 = "CO_Spring"
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = 1
  par4 = 0.05
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.annot is boolean", {
  par1 = "CO_Spring"
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = T
  par4 = 0.05
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.annot is longer vector", {
  par1 = "CO_Spring"
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = c("/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv",
           "/home/pejo/daphneg2/inst/extdata/CO_Spring.csv")
  par4 = 0.05
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})




test_that("g.miss is string", {
  par1 = 1
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par4 = "ciao"
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.miss is boolean", {
  par1 = 1
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par4 = T
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.miss is longer vector", {
  par1 = 1
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par4 = c(0.05, 0.01)
  par5 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})




test_that("g.maf is string", {
  par1 = 1
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par5 = "ciao"
  par4 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.maf is boolean", {
  par1 = 1
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par5 = T
  par4 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})

test_that("g.maf is longer vector", {
  par1 = 1
  par2 = "/home/pejo/gemma-0.98.1-linux-static"
  par3 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  par5 = c(0.05, 0.01)
  par4 = 0.05
  testthat::expect_error(object = gemma(par1, par2, par3, par4, par5))
})
