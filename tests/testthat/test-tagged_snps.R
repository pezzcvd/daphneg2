test_that("tg.input is integer", {
  par1 = 1
  par2 = "/home/pejo/daphneg2/inst/extdata/tags_example.list"
  testthat::expect_error(object = tagged_snps(par1, par2))
})

test_that("tg.input is boolean", {
  par1 = T
  par2 = "/home/pejo/daphneg2/inst/extdata/tags_example.list"
  testthat::expect_error(object = tagged_snps(par1, par2))
})

test_that("tg.input is longer vector", {
  par1 = c("CO_Spring", "CO_Summer")
  par2 = "/home/pejo/daphneg2/inst/extdata/tags_example.list"
  testthat::expect_error(object = tagged_snps(par1, par2))
})




test_that("tg.tags is integer", {
  par1 = "CO_Spring"
  par2 = 1
  testthat::expect_error(object = tagged_snps(par1, par2))
})

test_that("tg.tags is boolean", {
  par1 = "CO_Spring"
  par2 = T
  testthat::expect_error(object = tagged_snps(par1, par2))
})
