#' Calculate the mean decomposition rate of Soil Organic Matter
#'
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_C_OF (numeric) The carbon content of the soil (g / kg).
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil)
#' @param A_PH_CC (numeric) The pH-CaCl2 of the soil
#' @param duration (numeric) The duration over which an an annual averaged decomposition rate is estimated (years). Default value is 10 years.
#'
#' @import data.table
#'
#' @references Eurofins Agro (2013). Formules om OS-balans te berekenen voor NLV. Extension made by Hanegraaf & Bussink (2007) Schatting afbraaksnelheid organische stof. Vertaling van empirisch onderzoek naar nieuwe vuistregels - update 2007. NMI report B112.08, 15 pp.
#'
#' @export
omb_eurofins <- function(B_SOILTYPE_AGR, A_SOM_LOI = NA, A_C_OF = NA, A_N_RT = NA, A_PH_CC = NA, A_N_PMN = NA, duration = 10) {

  # add visual bindings
  cosfr = iagefr = temp = cor_temp = iage = NULL

  # Check inputs
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100)
  checkmate::assert_numeric(A_C_OF, lower = 0, upper = 1000)
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000)
  checkmate::assert_numeric(A_N_PMN, lower = 0, upper = 1000)
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 10)
  checkmate::assert_subset(B_SOILTYPE_AGR,
                           choices = c("duinzand","dekzand","zeeklei","rivierklei","maasklei","dalgrond","moerige_klei", "veen","loess"),
                           empty.ok = FALSE)
  checkmate::assert_numeric(duration, lower = 1, upper = 100)

  # Collect data into a table
  dt <- data.table(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_C_OF = A_C_OF,
                   A_N_RT = A_N_RT * 0.001,
                   A_N_PMN = A_N_PMN,
                   A_PH_CC = A_PH_CC,
                   value = NA_real_)

  # set C-OS fraction dependent on soil type
  dt[B_SOILTYPE_AGR =='loess',cosfr := 0.44]
  dt[B_SOILTYPE_AGR %in% c("moerige_klei", "veen"),cosfr := 0.50]
  dt[B_SOILTYPE_AGR %in% c("rivierklei", "maasklei"),cosfr := 0.41]
  dt[B_SOILTYPE_AGR =='zeeklei',cosfr := 0.46]
  dt[is.na(cosfr), cosfr := 0.58]

  # add soil dependent correctionfactor for the initial age
  dt[!is.na(A_N_RT) & !is.na(A_N_PMN) & !is.na(A_PH_CC), iagefr := 3 * (-0.2366 * A_C_OF + 0.8999 * A_C_OF^0.8 - 0.01496 * A_N_PMN - 0.1722 * A_N_RT + 0.0936 * A_PH_CC)/17]
  dt[is.na(iagefr),iagefr := 1]

  # set initial age
  dt[,iage := fifelse(B_SOILTYPE_AGR == 'duinzand',12.5,17) * iagefr]

  # set the anual temperature
  dt[, temp := 14]

  # add the temperature correction function
  dt[, cor_temp := ifelse(temp<=-1,0,ifelse(temp<=9,0.1*(temp+1),ifelse(temp<=27,2^((temp-9)/9),4)))]

  # estimate carbon content (g/kg) from SOM (%) if only SOM is given
  dt[is.na(A_C_OF), A_C_OF := A_SOM_LOI * cosfr * 10]

  # estimate the carbon decomposition in 10 years
  dt[, value := A_C_OF * (1-exp(4.7*(((iage+cor_temp*duration)^-0.6)-(iage^-0.6))))/duration]

  # return value for C concentration (g C / kg soil)
  value <- dt[, value]

  # return value
  return(value)

}
