#' Calculate the mean decomposition rate of Soil Organic Matter
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_N_RT (numeric) The total nitrogen content of the soil (mg / kg).
#' @param duration (numeric) The duration over which an an annual averaged decomposition rate is estimated (years). Default value is 10 years.
#'
#' @import data.table
#'
#' @references Vermeulen & Hendriks (1996).Bepaling van afbraaksnelheden van organische stof in laagveen: ademhalingsmetingen aan ongestoorde veenmonsters in het laboratorium. DLO-Staring centrum, Wageningen, Rapport 288, 99 pp.
#'
#' @export
omb_vermeulen <- function(A_SOM_LOI = NA_real_, A_C_OF = NA_real_, A_N_RT = NA_real_, duration = 10) {

  # add visual bindings
  A_CN_FR = NULL

  # extend A_C_OF when input is missing
  if(length(A_C_OF)==1 & is.na(A_C_OF[1])){A_C_OF <- rep(A_C_OF,length(A_SOM_LOI))}

  # Check inputs
  arg.length <- max(length(A_SOM_LOI), length(A_C_OF), length(A_N_RT))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(duration, lower = 1, upper = 100)

  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT * 0.001,
                   value = NA_real_)

  # estimate carbon content (g/kg) from SOM (%) if only SOM is given
  # assuming that 50% of SOM consists of carbon (Pribyl, 2010)
  dt[is.na(A_C_OF), A_C_OF := A_SOM_LOI * 0.5 * 10]

  # estimate the CN ratio, and set the applicability range (see Vermeulen & Hendriks)
  dt[, A_CN_FR := A_C_OF / A_N_RT]
  dt[, A_CN_FR := pmin(pmax(A_CN_FR,10),55)]

  # estimate the averaged annual carbon decomposition (g C kg-1 year-1), based on duration of (default) 10 years
  dt[, value := A_C_OF * (1 - exp(-1 * (0.016 - 0.00021 * A_C_OF / A_N_RT) * duration)) / duration]

  # return value for C decomposition (g C / kg soil)
  value <- dt[, value]

  # return value
  return(value)

}
