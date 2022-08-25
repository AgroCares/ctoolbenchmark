# test function organic matter decline according to Vermeulen & Hendriks (1996)

require(testthat)

  # default inputs
  A_SOM_LOI = c(2,5)
  A_C_OF = c(NA,NA)
  A_N_RT = c(1740,3957)
  A_CLAY_MI = 7.5

# test for two soils with only SOM input
test_that("estimation omb_vermeulen", {
  expect_equal(
    omb_vermeulen(A_SOM_LOI = c(2,5),A_N_RT = c(1100,3240)),
    expected = c(0.13,0.335),
    tolerance = 0.01)
})

# test for two soils with both SOM and SOC input (and SOM is preferred above SOC)
test_that("estimation omb_vermeulen", {
  expect_equal(
    omb_vermeulen(A_SOM_LOI = c(2,4.5),
                  A_C_OF = c(8,4.5),
                  A_N_RT = c(1700,3542)),
    expected = c(0.11,0.066),
    tolerance = 0.01)
})

# test for two soils with both SOM and SOC input (and SOC is preferred above SOM) and adapted duration and age
test_that("estimation omb_vermeulen", {
  expect_equal(
    omb_vermeulen(A_SOM_LOI = c(2,5),
                  A_C_OF = c(8,14),
                  A_N_RT = c(1700,3542),
                  duration = 25),
    expected = c(0.10,0.177),
    tolerance = 0.01)
})
