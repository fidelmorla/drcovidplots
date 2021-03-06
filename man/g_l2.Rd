% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g_l2.R
\name{g_l2}
\alias{g_l2}
\title{Case fatality rate (L2) from Ghani \emph{et al.} (2005) of COVID19}
\usage{
g_l2(saveplot = FALSE, savepng = FALSE)
}
\arguments{
\item{saveplot}{Logical. Should save the ggplot objet to the \code{.GlobalEnv}? Default \code{FALSE}.}

\item{savepng}{Logical. Should save a png version of the plot? Default \code{FALSE}.}
}
\value{
Graphic of L2 of COVID19 and save a
copy in png format to the computer at the address defined in \code{setwd()}.
}
\description{
This function graphs the L2 of the COVID19 in RD. L2 = 100 * Deaths / (Deaths + Recovered),
as recommended by Ghani \emph{et al.} (2005) for the Dominican Republic
}
\examples{
g_l2()
g_l2(saveplot = TRUE, savepng = FALSE)
}
\references{
A. C. Ghani, C. A. Donnelly, D. R. Cox, J. T. Griffin, C. Fraser, T. H. Lam, L. M. Ho,
W. S. Chan, R. M. Anderson, A. J. Hedley, G. M. Leung, Methods for Estimating the Case Fatality
Ratio for a Novel, Emerging Infectious Disease, American Journal of Epidemiology, Volume 162,
Issue 5, 1 September 2005, Pages 479–486, \url{https://doi.org/10.1093/aje/kwi230}
}
