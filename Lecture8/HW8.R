ca = read.csv("CADairyProduction.csv", stringsAsFactors = FALSE)
ca = na.omit(ca)
summary(ca)
str(ca)
milk.ts = ts(ca[,5], start = 1995, frequency = 12)

ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  # if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}

milk.decomp = ts.decomp(milk.ts, Mult = TRUE, is.df = FALSE, span = 0.25)
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(milk.decomp[, 3], is.df = FALSE)

#Arima model
ts.model = function(ts, col = 'remainder', order = c(0,0,1)){
  mod = arima(ts, order = order, include.mean = FALSE)
  print(mod)
  mod
}
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(2,1,2))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(0,1,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(1,0,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(1,1,0))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(2,1,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(2,2,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(2,3,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(2,5,1))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(2,5,0))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(1,5,0))
milk.arima = ts.model(milk.decomp[, 3], col = 'ARIMA model for Milk Order', order = c(0,5,0))
