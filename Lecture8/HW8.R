ca = read.csv("CADairyProduction.csv", stringsAsFactors = FALSE)
ca = na.omit(ca)
summary(ca)
str(ca)
milk.ts = ts(ca[,5], start = 1995, frequency = 12)

plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}

ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  # if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}

milk.decomp = ts.decomp(milk.ts, Mult = TRUE, is.df = FALSE, span = 0.5)
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(milk.decomp[, 3], is.df = FALSE)

#Arima model
ts.model = function(ts, col = 'remainder', order = c(0,0,1)){
  mod = arima(ts, order = order, include.mean = FALSE)
  print(mod)
  mod
}

milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(1,1,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(2,1,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(1,2,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(1,1,2))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(1,3,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(1,4,1))


require(forecast)
fit.elect = auto.arima(milk.ts, max.p=3, max.q=3,
                       max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=0, start.q=0, start.P=0, start.Q=0)
summary(fit.elect)

milk.forecast = forecast(fit.elect, h=12)
summary(milk.forecast)
plot(milk.forecast, xlab = "Year", ylab = "Milk Production")


#ICE CREAM
ice.ts = ts(ca[,4], start = 1995, frequency = 12)

ice.decomp = ts.decomp(ice.ts, Mult = TRUE, is.df = FALSE, span = 0.5)
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(ice.decomp[, 3], is.df = FALSE)

#Arima model

ice.arima = ts.model(ice.decomp[, 3], col = 'ARIMA model for Ice-Cream Order', order = c(1,1,1))
ice.arima = ts.model(ice.decomp[, 3], col = 'ARIMA model for Ice-Cream Order', order = c(2,1,1))
ice.arima = ts.model(ice.decomp[, 3], col = 'ARIMA model for Ice-Cream Order', order = c(1,2,1))
ice.arima = ts.model(ice.decomp[, 3], col = 'ARIMA model for Ice-Cream Order', order = c(1,1,2))
ice.arima = ts.model(ice.decomp[, 3], col = 'ARIMA model for Ice-Cream Order', order = c(1,3,1))
ice.arima = ts.model(ice.decomp[, 3], col = 'ARIMA model for Ice-Cream Order', order = c(1,4,1))


require(forecast)
fit.elect = auto.arima(ice.ts, max.p=3, max.q=3,
                       max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=0, start.q=0, start.P=0, start.Q=0)
summary(fit.elect)

ice.forecast = forecast(fit.elect, h=12)
summary(ice.forecast)
plot(ice.forecast, xlab = "Year", ylab = "Ice-Cream Production")
