% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/omb_janssen.R
\name{omb_janssen}
\alias{omb_janssen}
\title{Calculate the mean decomposition rate of Soil Organic Matter}
\usage{
omb_janssen(A_SOM_LOI = NA_real_, A_C_OF = NA_real_, iage = 10, duration = 10)
}
\arguments{
\item{A_SOM_LOI}{(numeric) The percentage organic matter in the soil (\%).}

\item{A_C_OF}{(numeric) The carbon content of the soil (g / kg).}

\item{iage}{(numeric) The initial age of the soil organic matter (years). Default value is 17 years.}

\item{duration}{(numeric) The duration over which an an annual averaged decomposition rate is estimated (years). Default value is 10 years.}
}
\description{
Calculate the mean decomposition rate of Soil Organic Matter
}
\references{
Janssen (1984) A simple method for calculating decomposition and accumulation of 'young' soil organic matter. Plant & Soil 76, 297-304.
}
