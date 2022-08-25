# test function organic matter decline according to Janssen (1986)

require(testthat)

  # default inputs
  # A_SOM_LOI = c(2,5)
  # A_C_OF = NA
  # iage = 10
  # duration = 10

# test for two soils with only SOM input
test_that("estimation omb_janssen", {
  expect_equal(
    omb_janssen(A_SOM_LOI = c(2,5)),
    expected = c(0.37,0.92),
    tolerance = 0.01)
})

# test for two soils with both SOM and SOC input (and SOC is preferred above SOM)
test_that("estimation omb_janssen", {
  expect_equal(
    omb_janssen(A_SOM_LOI = c(2,5),
                A_C_OF = c(8,14)),
    expected = c(0.29,0.51),
    tolerance = 0.01)
})

# test for two soils with both SOM and SOC input (and SOC is preferred above SOM) and adapted duration and age
test_that("estimation omb_janssen", {
  expect_equal(
    omb_janssen(A_SOM_LOI = c(2,5),
                A_C_OF = c(8,14),
                iage = 23,
                duration = 20),
    expected = c(0.09,0.16),
    tolerance = 0.01)
})
