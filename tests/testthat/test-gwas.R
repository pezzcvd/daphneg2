#par2 = "/home/pejo/gemma-0.98.1-linux-static"
test_that("gw.gemma is integer", {
  par1 = 1
  par2 = "CO_Spring"
  par3 = 0
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})

test_that("gw.gemma is boolean", {
  par1 = T
  par2 = "CO_Spring"
  par3 = 0
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})




test_that("gw.input is integer", {
  par1 = "/home/pejo/gemma-0.98.1-linux-static"
  par2 = 1
  par3 = 0
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})

test_that("gw.input is boolean", {
  par1 = "/home/pejo/gemma-0.98.1-linux-static"
  par2 = T
  par3 = 0
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})

test_that("gw.input is longer vector", {
  par1 = "/home/pejo/gemma-0.98.1-linux-static"
  par2 = c("CO_Spring", "CO_Summer")
  par3 = 0
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})




test_that("gw.cv is wrong integer", {
  par1 = "/home/pejo/gemma-0.98.1-linux-static"
  par2 = "CO_Spring"
  par3 = 10
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})

test_that("gw.cv is boolean", {
  par1 = "/home/pejo/gemma-0.98.1-linux-static"
  par2 = "CO_Spring"
  par3 = T
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})

test_that("gw.cv is string", {
  par1 = "/home/pejo/gemma-0.98.1-linux-static"
  par2 = "CO_Spring"
  par3 = "ciao"
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})

test_that("gw.cv is longer vector", {
  par1 = "/home/pejo/gemma-0.98.1-linux-static"
  par2 = "CO_Spring"
  par3 = c(0,1)
  par4 = "/home/pejo/daphneg2/inst/extdata/gemma_annot_example.csv"
  testthat::expect_error(object = gwas(par1, par2, par3, par4))
})

