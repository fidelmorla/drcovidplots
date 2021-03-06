% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g_incidence.R
\name{g_incidence}
\alias{g_incidence}
\title{Incidence of COVID19}
\usage{
g_incidence(saveplot = FALSE, savepng = FALSE)
}
\arguments{
\item{saveplot}{Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.}

\item{savepng}{Logical. Should save a png version of the plot? Default \code{FALSE}.}
}
\value{
Incidence chart and save a
copy in png format to the computer at the address defined in \code{setwd()}.
}
\description{
This function graphs the incidence of COVID19 in DR.
}
\examples{
g_incidence()
g_incidence(saveplot = TRUE, savepng = FALSE)
}
