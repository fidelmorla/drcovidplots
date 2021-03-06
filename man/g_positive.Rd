% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g_positive.R
\name{g_positive}
\alias{g_positive}
\title{COVID19 positivity}
\usage{
g_positive(saveplot = FALSE, savepng = FALSE)
}
\arguments{
\item{saveplot}{Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.}

\item{savepng}{Logical. Should save a png version of the plot? Default \code{FALSE}.}
}
\value{
Graph of the percentage of positives with respect to the total number of tests and saves a
copy in png format to the computer at the address defined in \code{setwd()}.
}
\description{
This function graphs the percentage of positives with respect to the total number of tests.
}
\examples{
g_positive()
g_positive(saveplot = FALSE, savepng = TRUE)
}
