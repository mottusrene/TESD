\name{BESD}
\alias{BESD}
\title{Binomial Effect Size Display (BESD) for correlations}
\description{

Calculate and plot pairwise probabilities for low and high values for continuously distributed variables x and y. This is procedure is called Binomial Effect Size Display (BESD). BESD allows heuristically estimating the extents to which correlations apply to individual observations (e.g., the probability that a high value in one variable, x, is matched with a high value in another variable, y). For both variable, low values correspond to the lower half of the distribution and high values to the higher half of the distribution. You may also plot the expected BESD alongside BESD calculated from the supplied data. Missing values are not permitted!

For more precise and meaningful effect size display, it may be advisable to use the Trinomial Effect Size Display (TESD), however. TESD categorises observations into low, medium and high values and calculates their pairwie probabilities such as, for example, the propbability that high values in one variable are matched with high values in another. Use TESD() function for this.

}
\usage{
BESD(x, y, plot = TRUE, Xlab = "X", Ylab = "Y", plot.expected.BESD = FALSE)
}
\arguments{
  \item{x}{ First continuously distributed variable }
  \item{y}{ Second continuously distributed variable }
  \item{plot}{ Whether a scatterplot with a BESD overlay should be printed }
  \item{Xlab}{ Label for the x variable }
  \item{Ylab}{ Label for the y variable }
  \item{plot.expected.BESD}{
Should expected BESD, given the correlation between x and y and assuming normal distribution for them, be printed (denoted with E)?}
}
\details{
When using this function, make sure that both variables can be meaningfully grouped into roughly equal low and high groups (the univariate and multivariate, cross-tabulated, proportions of high and low values for both variables will be printed, so you can see whether this assumption holds). Variables with limited values (e.g., Likert scale scores or those with only certain values possible) will lead to unequal group sizes. If the groups are not of equal size, the BESD calculated from the supplied data will not match the expected BESD (assuming normal distributions of continuously distributed variables). The function will warn you. In this case, it may be better to interpret the expected BESD (also printed), given the correlation between the supplied variables and assuming that they are normally distributed. This is always a safe option.
}
\value{
\item{univariate.x.proportions}{Proportions for the two groups in variable x}
\item{univariate.y.proportions}{Proportions for the two groups in variable y}
\item{cross.tabulations}{Cross-tabulation of x and y groups}
\item{BESD.table}{Cross-tabulation of the groups of variables x and y}
\item{multivariate.x.proportions}{
Sums of the proportions in the groups along variable x. If the sums are very different from 1, be careful! The variable may not be suitable for BESD, and you may want to interpret the expected BESD instead (always a safe option).}
}
\author{ René Mõttus }
\seealso{ BESD.expected(), BESD(), TESD(), TESD.expected() }
\examples{
Conscientiousness = rnorm(10000)
Performance = .3 * Conscientiousness + sqrt(1 - .3^2) + rnorm(length(Conscientiousness))
cor(Conscientiousness, Performance)
BESD(Conscientiousness, Performance)
}

