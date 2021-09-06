\name{TESD.expected}
\alias{TESD.expected}
\title{ Trinomial Effect Size Display (TESD) }
\description{ Calculate and plot expected TESD for any correlation, given either normal, uniform or skewed (beta) distributions for the variables involved. }
\usage{ TESD.expected(r, distribution, n, plot = TRUE) }
\arguments{
  \item{r}{ The correlation for which TESD is calculated }
  \item{distribution}{ Distributions of the variables to be simulated ("normal", "uniform", "skewed") }
  \item{n}{ Sample size for the simulation }
  \item{plot}{ Should the TESD plot be drawn? }
 }
\value{ Expected TESD, given the correlation and distribution. }
\references{ Mõttus, R. (2021)}
\seealso{ TESD() }
\author{ René Mõttus }
\examples{ TESD.expected(r = .3, distribution = "normal") }
