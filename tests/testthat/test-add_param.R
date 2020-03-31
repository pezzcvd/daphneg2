test_that("ap.mode is integer", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = 123
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.mode is string", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = "Ciao"
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.mode is a longer vector", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = c(T,T)
  testthat::expect_error(object = add_param(par1, par2, par3))
})



test_that("ap.newpar is integer", {
  par1 = 123
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.newpar is boolean", {
  par1 = T
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.newpar is longer vector", {
  par1 = c("ciao1.csv", "ciao2.csv")
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.newpar has wrong extension", {
  par1 = "inst/extdata/example_add_param.txt"
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.newpar is not a file", {
  par1 = "inst/extdata/not_a_file.csv"
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})



test_that("ap.metadata is integer", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = 123
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.metadata is boolean", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = T
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.metadata is longer vector", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = c("ciao1", "ciao2")
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.metadata has wrong extension", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = "inst/extdata/example_add_param_meta.txt"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.metadata doesn't have meta before the extension", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = "inst/extdata/example_add_param_menta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.metadata is not a file", {
  par1 = "inst/extdata/example_add_param.csv"
  par2 = "inst/extdata/not_a_file.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})



test_that("ap.newparam and ap.metadata don't share same prefix", {
  par1 = "inst/extdata/example_wrong.csv"
  par2 = "inst/extdata/example_add_param_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.newparam doesn't have correct fields inside (acc instead of accession and phn intead of phenotype)", {
  par1 = "inst/extdata/example_wrong.csv"
  par2 = "inst/extdata/example_wrong_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("ap.metadata has wrong number of fields", {
  par1 = "inst/extdata/example_wrong2.csv"
  par2 = "inst/extdata/example_wrong_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})

test_that("data already present in the dataset", {
  par1 = "inst/extdata/CO_Spring.csv"
  par2 = "inst/extdata/CO_Spring_meta.csv"
  par3 = T
  testthat::expect_error(object = add_param(par1, par2, par3))
})
