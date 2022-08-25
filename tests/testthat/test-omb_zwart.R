# test function organic matter decline according to Zwart et al. (2013)

require(testthat)

  # default inputs
  A_SOM_LOI = c(2,5)
  A_C_OF = c(NA,NA)
  A_CLAY_MI = 7.5

# test for two soils with only SOM input
test_that("estimation omb_zwart", {
  expect_equal(
    omb_zwart(A_SOM_LOI = c(2,5)),
    expected = c(0.23,0.33),
    tolerance = 0.01)
})

# test for two soils with both SOM and SOC input (and SOM is preferred above SOC)
test_that("estimation omb_zwart", {
  expect_equal(
    omb_zwart(A_SOM_LOI = c(2,4.5),
                A_C_OF = c(8,4.5)),
    expected = c(0.23,0.32),
    tolerance = 0.01)
})

# test for two soils with both SOM and SOC input (and SOC is preferred above SOM) and adapted duration and age
test_that("estimation omb_zwart", {
  expect_equal(
    omb_zwart(A_SOM_LOI = c(2,5),
                A_C_OF = c(8,14),
                A_CLAY_MI = c(25,1.5)),
    expected = c(0.26,0.33),
    tolerance = 0.01)
})
