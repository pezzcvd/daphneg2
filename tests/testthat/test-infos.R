test_that("infos has the correct input", {
  # Correct env paramenter
  out = "CO_Spring"
  testthat::expect_vector(object = infos(out))

  # Correct phn paramenter
  out = "4W_100"
  testthat::expect_vector(object = infos(out))

  # Parameter is a number
  out = 1
  testthat::expect_error(object = infos(out))

  # Parameter is a boolean
  out = T
  testthat::expect_error(object = infos(out))

  # Longer string vector
  out = c("test1par", "test2par")
  testthat::expect_error(object = infos(out))

  # Parameter not in core dataset
  out = "ciao_bella"
  testthat::expect_error(object = infos(out))
})

test_that("infos retrieves correctly environmental parameters", {
  ## Output
  out = infos("CO_Spring")
  checkmate::expect_data_frame(out)
})
