# test bulk density functions
require(testthat)

test_that("estimation est_bulkdensity", {
  expect_equal(
    est_bulkdensity(A_SOM_LOI = c(2,5),
                    A_CLAY_MI = c(10,15)),
    expected = c(1343.75,1176.612),
    tolerance = 0.01)
})
