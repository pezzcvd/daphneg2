test_that("par is not numeric", {
  par2 = 123
  testthat::expect_error(qq_plot(par2))
})

test_that("par is not logical", {
  par2 = T
  testthat::expect_error(qq_plot(par2))
})

test_that("par is longer vector", {
  par2 = c("CO_Spring", "CO_Summer")
  testthat::expect_error(qq_plot(par2))
})
