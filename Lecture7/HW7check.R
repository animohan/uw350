library(MASS)

auto = read.csv("Auto.csv", header = TRUE, stringsAsFactors = FALSE)
numcols = c("price","bore","stroke","horsepower","peak.rpm")
auto[,numcols] = lapply(auto[,numcols], as.numeric)
auto = data.frame(auto)
dim(auto)
auto = subset(auto, select = -c(symboling,normalized.losses))
dim(auto)
auto[complete.cases(auto),]

#Run linear regression
lm.auto = lm(log(price) ~., data = auto)
summary(lm.auto)
plot(lm.auto)


lm.auto.aic = stepAIC(lm.auto, direction = "both")
lm.auto.aic$anova
summary(lm.auto.aic)
plot(lm.auto.aic)


#SVD Regression
auto = read.csv("Auto.csv", header = TRUE, stringsAsFactors = TRUE)
numcols = c("price","bore","stroke","horsepower","peak.rpm")
auto[,numcols] = lapply(auto[,numcols], as.numeric)
auto = data.frame(auto)
dim(auto)
auto = subset(auto, select = -c(symboling,normalized.losses))
dim(auto)
auto[complete.cases(auto),]
#cols = c("make","fuel.type","aspiration","num.of.doors","body.style","drive.wheels","engine.location", "engine.type","num.of.cylinders", "fuel.system")
cols = c("wheel.base","length","width","height","curb.weight","engine.size","bore","stroke","compression.ratio","horsepower","peak.rpm","city.mpg","highway.mpg","price")
auto.scale = auto
auto.scale[,cols] = lapply(auto[,cols],scale)
auto.modmat = model.matrix(price~.-1, data = auto.scale)

M = auto.modmat[,-1]
M2 = t(M)%*%M

head(M2)
mSVD = svd(M2)
plot(diag(mSVD$d))

inv.diag = rep(0,64)
inv.diag[1:20] = 1/mSVD$d[1:20]
mD = diag(inv.diag)

#Inverse (t(M)*M) = V Diag U
mInV = mSVD$v %*% mD %*% t(mSVD$u)

b = auto.scale$price
#x = Inv(MTM)*t(M)*b
x = mInV %*% t(M) %*% b

auto.cp = auto
auto.cp$score =  M %*% x + mean(auto.cp$price)
auto.cp$resids =  auto.cp$score - auto.cp$price


require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)

plot.svd.reg <- function(df, k = 4){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
    geom_point(aes(score, resids), size = 2) + 
    stat_smooth(aes(score, resids)) +
    ggtitle('Residuals vs. fitted values')
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle('Histogram of residuals')
  
  qqnorm(df$resids)
  
  grid.arrange(p1, p2, ncol = 2)
  
  df$std.resids = sqrt((df$resids - mean(df$resids))^2)  
  
  p3 = ggplot(df) + 
    geom_point(aes(score, std.resids), size = 2) + 
    stat_smooth(aes(score, std.resids)) +
    ggtitle('Standardized residuals vs. fitted values')
  print(p3) 
  
  n = nrow(df)
  Ybar = mean(df$childHeight)
  SST <- sum((df$childHeight - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}

plot.svd.reg(auto.cp)
