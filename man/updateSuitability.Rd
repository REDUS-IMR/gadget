% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{updateSuitability}
\alias{updateSuitability}
\title{Update a fleet suitability parameters for a certain stock}
\usage{
updateSuitability(fleetName, stockName, len, value)
}
\arguments{
\item{fleetName}{a string of the fleet name}

\item{stockName}{a string of the stock name}

\item{len}{the stock length target}

\item{value}{the target suitability value}
}
\value{
a vector of numeric error codes (0 is success, otherwise is error)
}
\description{
This function updates a fleet suitability parameters for a given stock.
Please note that this function will only update the gadget internal pre-computed
suitability values and not the user defined suitability function and its parameters
found in the model file
}
