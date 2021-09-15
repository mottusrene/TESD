BESD.expected <- function (r, distribution = c("normal", "uniform", "skewed"),
                           n = 10^3, plot=TRUE)
{
  cut2 <- function(x) cut(x, quantile(x, c(0, 1/2, 1)),
                          c("Low", "High"), include.lowest = TRUE)
  if (distribution == "normal") {
    x <- rnorm(n)
    y <- r * x + sqrt(1 - r^2) * rnorm(n)
  }
  if (distribution == "uniform") {
    x <- runif(n, -1, 1)
    y <- r * x + sqrt(1 - r^2) * runif(n, -1, 1)
  }
  if (distribution == "skewed") {
    x <- rbeta(n, 5, 1)
    y <- r * x + sqrt(1 - r^2) * rbeta(n, 5, 1)
  }
  tabs <- data.frame(y = cut2(y), x = cut2(x))
  crosstabs <- apply(prop.table(table(tabs)), 2, function(x) x/sum(x))
  crosstabs <- crosstabs[c("High", "Low"), c("Low", "High")]

  percents <- matrix(paste(round((crosstabs * 100), 1), "%",
                           sep = ""), ncol = 2)


  if(plot) {
    color = as.factor(apply(tabs, 1, paste, collapse = ""))
    plot(x, y, cex = 0.5, , xlab = "X", ylab = "Y", col = c("olivedrab1", "orange", "green", "red", "darkgreen", "brown")[color])

    lines(rep(median(x), 2), c(min(y) - abs(min(y)), max(y) + abs(max(y))), col = "grey50")
    lines(rep(median(x), 2), c(min(y) - abs(min(y)), max(y) + abs(max(y))), col = "grey50")

    lines(c(min(x) - abs(min(x)), max(x) + abs(max(x))), rep(median(y), 2), col = "grey50")
    lines(c(min(x) - abs(min(x)), max(x) + abs(max(x))), rep(median(y), 2), col = "grey50")

    text((median(x) + min(x))/2, (median(y) + max(y))/2, percents[1,1], cex = 1)

    text((median(x) + min(x))/2, (median(y) + min(y))/2, percents[2, 1], cex = 1)

    text((median(x) + max(x))/2, (median(y) + max(y))/2, percents[2, 1], cex = 1)
    text((median(x) + max(x))/2, (median(y) + min(y))/2, percents[2,2], cex = 1)

    xstand = (x-min(x))/(max(x)-min(x))
    ystand = (y-min(y))/(max(y)-min(y))

    mtext(paste("Proportions of Y if X is:"), side=3, line=3, adj=0)

    mtext('low', side=3, line=1, adj=median(xstand)/2)
    mtext('high', side=3, line=1, adj=(median(xstand)+1)/2)

    mtext('low', side=4, line=1, adj=median(ystand)/2)
    mtext('high', side=4, line=1, adj=(median(ystand)+1)/2)

    mtext(paste("Correlation =", round(cor(x,y),2)), side = 1, line=4, adj=0)

  }

  round(crosstabs,4)

}



