\name{BESD.expected}
\alias{BESD.expected}
\title{ Binomial Effect Size Display (BESD) }
\description{ Calculate and plot expected BESD for any correlation, given either normal, uniform or skewed (beta) distributions for the variables involved. }
\usage{ BESD.expected(r, distribution, n, plot = TRUE) }
\arguments{
  \item{r}{ The correlation for which BESD is calculated }
  \item{distribution}{ Distributions of the variables to be simulated ("normal", "uniform", "skewed") }
  \item{n}{ Sample size for the simulation }
  \item{plot}{ Should the BESD plot be drawn? }
 }
\value{ Expected BESD, given the correlation and distribution. }
\seealso{ BESD(), TESD(), TESD.expected() }
\author{ René Mõttus }
\examples{ BESD.expected(r = .3, distribution = "normal") }
