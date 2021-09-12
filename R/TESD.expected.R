TESD.expected <- function(r, distribution = c("normal", "uniform", "skewed"), n = 10^3, plot=TRUE){

  cut3 <- function(x) cut(x,quantile(x,c(0,1/3,2/3,1)),c("Low","Medium","High"), include.lowest=TRUE)

  if(distribution == "normal"){
    x <- rnorm(n)
    y <- r * x + sqrt(1 - r^2)*rnorm(n)
  }

  if(distribution == "uniform"){
    x <- runif(n, -1, 1)
    y <- r * x + sqrt(1 - r^2)*runif(n,-1,1)
  }

  if(distribution == "skewed"){
    x <- rbeta(n, 5, 1)
    y <- r * x + sqrt(1 - r^2)*rbeta(n, 5, 1)
  }

  tabs <- data.frame(y = cut3(y), x = cut3(x))
  crosstabs <- apply(prop.table(table(tabs)),2,function(x) x/sum(x))
  crosstabs <- crosstabs[c("High","Medium","Low"),c("Low","Medium","High")]
  percents <- matrix(paste(round((crosstabs * 100),1),"%", sep=""), ncol=3)
  if(plot){
    color = as.factor(apply(tabs, 1, paste, collapse=""))

    plot(x,y, cex=.5, ,
         xlab="X", ylab="Y",
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
    mtext("Proportions of Y if X is:", side=3, line=3, adj=0)
    mtext('low', side=3, line=1, adj=(xq1[1])/2)
    mtext('medium', side=3, line=1, adj=median(xstand))
    mtext('high', side=3, line=1, adj=(xq1[2] + 1)/2 )

  }

  round(crosstabs,4)

}
