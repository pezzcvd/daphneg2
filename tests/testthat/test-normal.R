test_that("input is not character vector", {
  #character
  par = c("a", "b")
  testthat::expect_error(object = normal(par))
})

test_that("input is not logical vector", {
  #character
  par = c(T,F,T,T,T)
  testthat::expect_error(object = normal(par))
})

test_that("input single number", {
  #character
  par = 1
  testthat::expect_error(object = normal(par))
})
