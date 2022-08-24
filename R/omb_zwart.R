#' Calculate the mean decomposition rate of Soil Organic Matter
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @import data.table
#'
#' @references Zwart et al. (2013) De organische stof balans met de te verwachten stikstoflevering per teeltrotatie. Available at: https://edepot.wur.nl/272649.
#'
#' @export
omb_zwart <- function(A_SOM_LOI = NA, A_C_OF = NA,A_CLAY_MI = 5) {

  # add visual bindings
  A_DENSITY = NULL

  # Check inputs
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100)

  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_CLAY_MI = A_CLAY_MI,
                   value = NA_real_)

  # estimate SOM (%) from carbon content (g/kg) if only SOM is given
  # assuming that 50% of SOM consists of carbon (Pribyl, 2010)
  dt[is.na(A_SOM_LOI), A_SOM_LOI := A_C_OF * 2 * 0.1]

  # estimate density of the soil
  dt[,A_DENSITY := est_bulkdensity(A_SOM_LOI = A_SOM_LOI, A_CLAY_MI = A_CLAY_MI)]

  # carbon decomposition (kg EOS per year)
  dt[A_SOM_LOI > 23, value := 3145.513]
  dt[A_SOM_LOI <= 23, value := A_SOM_LOI * 0.1 * 0.3 * (-0.37 * log(A_SOM_LOI) + 1.8398) * (10 ^ 6) * 0.0487 * A_SOM_LOI ^ -0.453 * 0.57]

  # convert carbon decomposition to % EOS change
  dt[, value := value * 100 / (0.3 * 100 * 100 * A_DENSITY)]

  # estimate the carbon decomposition (g C/ kg soil)
  dt[, value := value * 0.5 * 10]

  # return value for C concentration (g C / kg soil)
  value <- dt[, value]

  # return value
  return(value)

}
