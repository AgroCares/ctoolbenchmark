#' Calculate the mean decomposition rate of Soil Organic Matter
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param iage (numeric) The initial age of the soil organic matter (years). Default value is 17 years.
#' @param duration (numeric) The duration over which an an annual averaged decomposition rate is estimated (years). Default value is 10 years.
#'
#' @import data.table
#'
#' @references Janssen (1984) A simple method for calculating decomposition and accumulation of 'young' soil organic matter. Plant & Soil 76, 297-304.
#'
#' @export
omb_janssen <- function(A_SOM_LOI = NA_real_, A_C_OF = NA_real_, iage = 10, duration = 10) {

  # add visual bindings
  temp = cor_temp = NULL

  # extend A_C_OF when input is missing
  if(length(A_C_OF)==1 & is.na(A_C_OF[1])){A_C_OF <- rep(A_C_OF,length(A_SOM_LOI))}

  # Check inputs
  arg.length <- max(length(A_SOM_LOI), length(A_C_OF))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100,len = arg.length)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000, len = arg.length)
  checkmate::assert_numeric(iage, lower = 1, upper = 100)
  checkmate::assert_numeric(duration, lower = 1, upper = 100)

  # Collect data into a table
  dt <- data.table(A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   value = NA_real_)

  # estimate carbon content (g/kg) from SOM (%) if only SOM is given
  # assuming that 50% of SOM consists of carbon (Pribyl, 2010)
  dt[is.na(A_C_OF), A_C_OF := A_SOM_LOI * 0.5 * 10]

  # set the anual temperature
  dt[, temp := 12]

  # add the temperature correction function
  dt[, cor_temp := ifelse(temp<=-1,0,ifelse(temp<=9,0.1*(temp+1),ifelse(temp<=27,2^((temp-9)/9),4)))]

  # estimate the carbon decomposition in 10 years
  dt[, value := A_C_OF * (1-exp(4.7*(((iage+cor_temp*duration)^-0.6)-(iage^-0.6))))/duration]

  # return value for C decomposition (g C / kg soil)
  value <- dt[, value]

  # return value
  return(value)

}
