% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{runStep}
\alias{runStep}
\title{Forward simulation for a step and collect stats for all the stocks and fleets}
\usage{
runStep(stockAfterStep = FALSE)
}
\arguments{
\item{stockAfterStep}{Tells the function to collect stock information after the step if TRUE, Default: FALSE}
}
\value{
list of fleets and stocks statistics (in matrix format) for the current step run
}
\description{
Forward simulation for a year and collect the stats. For the stocks, the output consists of
stock number (stk), spawning stock biomass (ssb), recruitment number (rec), and predatory information
which are categorized by preys (stock names). Please note that the stock number can be collected in either
before or after the step by using the stockAfterStep parameter. For the fleets, the output consists of
only the predatory information which are categorized by preys (stock names).
}
\examples{
\dontrun{
if(interactive()){
 exPath <- loadExample()
 initGadget(exPath, "refinputfile")
 stat <- runStep(stockAfterStep = TRUE)
 print(stat)
 stat <- runStep(stockAfterStep = FALSE)
 print(stat)
 endGadget()
 }
}
}
