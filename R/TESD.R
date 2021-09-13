TESD <- function(x,y, plot=TRUE, Xlab="X",Ylab="Y", plot.expected.TESD=FALSE){

  cut3 <- function(x) cut(x,quantile(x,c(0,1/3,2/3,1)),c("Low","Medium","High"), include.lowest=TRUE)

  tabs <- data.frame(y = cut3(y), x = cut3(x))

  crosstabs <- apply(prop.table(table(tabs)),2,function(x) x/sum(x))

  crosstabs <- crosstabs[c("High","Medium","Low"),c("Low","Medium","High")]

  percents <- matrix(paste(round((crosstabs * 100),1),"%", sep=""), ncol=3)


  ## Expected TESD for the correlation if the variables were normally distributed

  tx <- rnorm(10^6)
  ty <- cor(x,y)* tx + sqrt(1- cor(x,y)^2)*rnorm(length(tx))
  ttabs <- data.frame(y = cut3(ty), x = cut3(tx))
  tcrosstabs <- apply(prop.table(table(ttabs)),2,function(x) x/sum(x))
  tcrosstabs <- tcrosstabs[c("High","Medium","Low"),c("Low","Medium","High")]
  tpercents <- matrix(paste(round((tcrosstabs * 100),1),"%", sep=""), ncol=3)

  if(plot.expected.TESD)
    percents <- matrix(paste(percents, tpercents, sep = "\nE: "), ncol=3)
  if(plot){
    color <- as.factor(apply(tabs, 1, paste, collapse=""))

    plot(x,y, cex=.5, ,
         xlab=Xlab, ylab=Ylab,
         col = c("olivedrab1","orange", "lightblue", "green", "red", "dodgerblue2", "darkgreen","brown","midnightblue")[color] )

    xq <- quantile(x, c(1/3, 2/3))
    yq <- quantile(y, c(1/3, 2/3))

    lines(rep(xq[1],2),c(min(y) - abs(min(y)),max(y) + abs(max(y))), col="grey50")
    lines(rep(xq[2],2),c(min(y) - abs(min(y)),max(y) + abs(max(y))), col="grey50")

    lines(c(min(x) - abs(min(x)),max(x) + abs(max(x))), rep(yq[1],2), col="grey50")
    lines(c(min(x) - abs(min(x)),max(x) + abs(max(x))), rep(yq[2],2), col="grey50")

    text((xq[1] + min(x))/2, (yq[2] + max(y))/2, percents[1,1], cex=1)
    text((xq[1] + min(x))/2, mean(y), percents[2,1], cex=1)
    text((xq[1] + min(x))/2, (yq[1] + min(y))/2, percents[3,1], cex=1)

    text(median(x), (yq[2] + max(y))/2, percents[1,2], cex=1)
    text(median(x), mean(y), percents[2,2], cex=1)
    text(median(x), (yq[1] + min(y))/2, percents[3,2], cex=1)

    text((xq[2] + max(x))/2, (yq[2] + max(y))/2, percents[1,3], cex=1)
    text((xq[2] + max(x))/2, mean(y), percents[2,3], cex=1)
    text((xq[2] + max(x))/2, (yq[1] + min(y))/2, percents[3,3], cex=1)

    xstand = (x-min(x))/(max(x)-min(x))
    xq1 <- quantile(xstand, c(1/3, 2/3))
    ystand = (y-min(y))/(max(y)-min(y))
    yq1 <- quantile(ystand, c(1/3, 2/3))
    
    mtext(paste("Proportions of" , Ylab, "if", Xlab, "is:"), side=3, line=3, adj=0)
    
    mtext('low', side=3, line=1, adj=(xq1[1])/2)
    mtext('medium', side=3, line=1, adj=median(xstand))
    mtext('high', side=3, line=1, adj=(xq1[2] + 1)/2 )
    
    mtext('low', side=4, line=1, adj=(yq1[1])/2)
    mtext('medium', side=4, line=1, adj=median(ystand))
    mtext('high', side=4, line=1, adj=(yq1[2] + 1)/2 )

  }

  if(min(rowSums(crosstabs)) < .95 | max(rowSums(crosstabs)) > 1.05)
    warning("\n\nBut TESD groups have unexpected sizes (see univariate and multivariate proportions), so be careful!
            These variables may not be suitable for interpreting TESD from supplied data.
            This is probably beause the variables have a limited range of values, which makes group sizes unequal.
            Interpret the estimated TESD instead (assuming variables should be normally distributed). It is printed.
            You can also switch it on by plot.expected.TESD = TRUE.\n")

  list(
    TESD.table.from.supplied.data = round(crosstabs,4),
    expected.TESD = round(tcrosstabs,4),
    univariate.x.proportions = round(prop.table(table(tabs$x)),4),
    univariate.y.propoprtions = round(prop.table(table(tabs$y)),4),
    cross.tabulations = table(tabs),
    multivariate.y.proportions = round(rowSums(crosstabs),4)
  )
}
