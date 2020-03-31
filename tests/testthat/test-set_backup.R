test_that("input in in the set of allowed characters", {
  par = "/home/ciao!"
  testthat::expect_error(object = set_backup(par) )
})


test_that("output is a string", {
  checkmate::expect_character(x = set_backup())
})
