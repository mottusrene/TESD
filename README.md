# TESD

Calculate and plot pairwise probabilities for low, medium and high values for continuously distributed variables x and y. This is procedure is called Trinomial Effect Size Display (TESD). TESD allows heuristically estimating the extents to which correlations apply to individual observations (e.g., the probability that a high value in one variable, x, is matched with a high value in another variable, y). For both variable, low values correspond to the lowest third of the distribution, high values to the highest third of the distribution, and medium values contribute to the middle third of the distribution.  You can also calculate and plot expected TESD for any correlation, given either normal, uniform or skewed (beta) distributions for the variables involved.

To install the package in R:

> devtools::install_github("https://github.com/mottusrene/TESD")