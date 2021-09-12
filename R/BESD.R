BESD <- function(x,y, plot=TRUE, Xlab="X",Ylab="Y", plot.expected.BESD=FALSE){

  cut2 <- function(x) cut(x, quantile(x, c(0, 1/2, 1)), c("Low", "High"), include.lowest = TRUE)

  tabs <- data.frame(y = cut2(y), x = cut2(x))

  crosstabs <- apply(prop.table(table(tabs)),2,function(x) x/sum(x))

  crosstabs <- crosstabs[c("High","Low"),c("Low","High")]

  percents <- matrix(paste(round((crosstabs * 100),1),"%", sep=""), ncol=2)


  ## Expected BESD for the correlation if the variables were normally distributed

  tx <- rnorm(10^6)
  ty <- cor(x,y)* tx + sqrt(1- cor(x,y)^2)*rnorm(length(tx))
  ttabs <- data.frame(y = cut2(ty), x = cut2(tx))
  tcrosstabs <- apply(prop.table(table(ttabs)),2,function(x) x/sum(x))
  tcrosstabs <- tcrosstabs[c("High","Low"),c("Low","High")]
  tpercents <- matrix(paste(round((tcrosstabs * 100),1),"%", sep=""), ncol=2)

  if(plot.expected.BESD)
    percents <- matrix(paste(percents, tpercents, sep = "\nE: "), ncol=2)

  if(plot){

    color = as.factor(apply(tabs, 1, paste, collapse = ""))
    plot(x, y, cex = 0.5, , xlab = Xlab, ylab = Ylab, col = c("olivedrab1", "orange", "green", "red", "darkgreen", "brown")[color])

    lines(rep(median(x), 2), c(min(y) - abs(min(y)), max(y) + abs(max(y))), col = "grey50")
    lines(rep(median(x), 2), c(min(y) - abs(min(y)), max(y) + abs(max(y))), col = "grey50")

    lines(c(min(x) - abs(min(x)), max(x) + abs(max(x))), rep(median(y), 2), col = "grey50")
    lines(c(min(x) - abs(min(x)), max(x) + abs(max(x))), rep(median(y), 2), col = "grey50")

    text((median(x) + min(x))/2, (median(y) + max(y))/2, percents[1,1], cex = 1)

    text((median(x) + min(x))/2, (median(y) + min(y))/2, percents[2, 1], cex = 1)

    text((median(x) + max(x))/2, (median(y) + max(y))/2, percents[2, 1], cex = 1)
    text((median(x) + max(x))/2, (median(y) + min(y))/2, percents[2,2], cex = 1)

    xstand = (x-min(x))/(max(x)-min(x))
    mtext(paste("Proportions of" , Ylab, "if", Xlab, "is:"), side=3, line=3, adj=0)
    mtext('low', side=3, line=1, adj=median(xstand)/2)
    mtext('high', side=3, line=1, adj=(median(xstand)+1)/2)

  }

  warning(c("\n\n",
            paste("Interpret BESD from supplied data as:
          Proportions of", Ylab, "if", Xlab," is either low or high, respectively"), "\n"))

  if(min(rowSums(crosstabs)) < .95 | max(rowSums(crosstabs)) > 1.05)
    warning("\n\nBut BESD groups have unexpected sizes (see univariate and multivariate proportions), so be careful!
            These variables may not be suitable for interpreting BESD from supplied data.
            This is probably beause the variables have a limited range of values, which makes group sizes unequal.
            Interpret the estimated BESD instead (assuming variables should be normally distributed). It is printed.
            You can also switch it on by plot.expected.BESD = TRUE.\n")

  list(
    BESD.table.from.supplied.data = round(crosstabs,4),
    expected.BESD = round(tcrosstabs,4),
    univariate.x.proportions = round(prop.table(table(tabs$x)),4),
    univariate.y.propoprtions = round(prop.table(table(tabs$y)),4),
    cross.tabulations = table(tabs),
    multivariate.y.proportions = round(rowSums(crosstabs),4)
  )

}
