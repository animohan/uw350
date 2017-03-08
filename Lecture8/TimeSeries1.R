vec = sin((1:365)/30)  # A vector of values
class(vec) # Vector is an atomic class in R

## Create a ts class object from the vector
## by adding time series attributes
vec.ts = ts(vec, start = 1990/01/01, freq = 365)
attributes(vec.ts) # Note the time series attributes
require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
plot(vec.ts) # Note the x-axis is the time attribute


options(repr.pmales.extlot.width=8, repr.plot.height=4)
ts.white = function(n, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  ts(rnorm(n, mean = mean, sd = sd), start = start, freq = 12)
}
white = ts.white(180)
attributes(white)
plot(white)

dist.ts = function(df, col = 'residual', bins = 40){
  par(mfrow = c(1,2))
  temp = as.vector(df)
  breaks = seq(min(temp), max(temp), length.out = (bins + 1))
  hist(temp, breaks = breaks, main = paste('Distribution of ', col), xlab = col)
  qqnorm(temp, main = paste('Normal Q-Q plot of ', col))
  par(mfrow = c(1,1))
}
dist.ts(white, col = 'white noise')


options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}
plot.acf(white, col = 'white noise', is.df = F)


ts.white.sin = function(n, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  sin.vals = 1 * sin(1:n/freq)
  ts((rnorm(n, mean = mean, sd = sd) + sin.vals), start = start, freq = 12)
}
white.sin = ts.white.sin(180)
attributes(white.sin)
plot(white.sin)
plot(sin(1:180/12))


## Investigate the time series properties of random walk
options(repr.pmales.extlot.width=8, repr.plot.height=4)
ran.walk = function(n, freq = 12, start = 1990, sd = 1.0, mean = 0.0){
  norms = rnorm(n, mean = mean, sd = sd)
  ts(cumsum(norms), start = start, freq = 12)
}
ranWalk = ran.walk(180)
plot(ranWalk, main = 'Random walk time series')

options(repr.pmales.extlot.width=8, repr.plot.height=4)
dist.ts(ranWalk, col = 'random walk')
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(ranWalk, col = 'random walk', is.df = F)


## ---- Investigate time series properties of 
## trend + white noise
options(repr.pmales.extlot.width=8, repr.plot.height=4)
ts.trend = function(n, slope = 0.01, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  temp = seq(0, slope * n, length.out = n) + 
    rnorm(n, mean = mean, sd = sd)
  ts(temp, start = start, freq = 12)
}
trend = ts.trend(180, slope = 0.05)
plot(trend, main = 'Trend + white noise time series')
dist.ts(trend, col = 'trend + white noise')
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(trend, col = 'trend + white noise', is.df = F)


## --- Investigate time series properties of 
## trend + white noise + seasonal
ts.season = function(n, slope = 0.01, mean = 0.0, sd = 1.0, start = 1990, freq = 12){
  temp = seq(0, slope * n, length.out = n) + 
    rnorm(n, mean = mean, sd = sd) +
    2 * sin(0:(n -1) * pi / freq) +
    cos(0:(n -1) * pi / freq)
  ts(temp, start = start, freq = 12)
}
season = ts.season(180, slope = 0.00)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
plot(season, main = 'White noise + seasonal time series')

options(repr.pmales.extlot.width=8, repr.plot.height=4)
dist.ts(season, col = 'seasonal + white noise')
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(season, col = 'seasonal + white noise', is.df = F)


## Decomposition of the time series into components
ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  # if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}
season.trend = ts.season(180, slope = 0.05)      
temp = ts.decomp(season.trend, is.df = FALSE, Mult = FALSE, span = .4)
