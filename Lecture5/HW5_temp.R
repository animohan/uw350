library(HistData)
library(resample)
library(simpleboot)


auto = read.csv("Auto.csv", header = TRUE, stringsAsFactors = FALSE)
numcols = c("price","bore","stroke","horsepower","peak.rpm")
auto[, numcols] <- lapply(auto[, numcols], as.numeric)
auto = auto[complete.cases(auto),]

#Compare the difference of the bootstrap resampled mean of the log price of autos grouped by 1) aspiration and 2) fuel type. 
#Use both numerical and graphical methods for your comparison. Are these means different within a 95% confidence interval? 
#How do your conclusions compare to the results you obtained using the t-test last week?

# Multi-Histogram Plotting function
plot.hist = function(a, b, cols = c("Population A","Population B"), nbins = 80, p = 0.05 ){
  dat = c(a,b)
  maxs = max(dat, na.rm = TRUE)
  mins = min(dat, na.rm = TRUE)
  breaks = seq(maxs, mins, length.out = (nbins+1))
  par(mfrow = c(2,1))
  hist(a, breaks = breaks, main = paste("Histogram of ", cols[1]), xlab  = cols[1])
  abline(v = mean(a), lwd = 4, col = "red")
  abline(v = quantile(a, probs = p/2), lty = 3, col = "red", lwd = 3)
  abline(v = quantile(a, probs = (1-p/2)), lty = 3, col = "red", lwd = 3)
  
  hist(b, breaks = breaks, main = paste("Histogram of ", cols[2]),  xlab = cols[2])
  abline(v = mean(b), lwd = 4, col = "red")
  abline(v = quantile(b, probs = p/2), lty = 3, col = "red", lwd = 3)
  abline(v = quantile(b, probs = (1-p/2)), lty = 3, col = "red", lwd = 3)
}

#Comparing  Log(Price) v.s Aspiration
auto.turbo.log.price = (auto[auto$aspiration=="turbo",]$price)
auto.std.log.price = (auto[auto$aspiration == "std",]$price)
plot.hist(auto.turbo.log.price, auto.std.log.price, c(" Auto Price: Aspiration = Turbo", "Auto Price: Aspiration = Std"))

#T-test
t.test(auto.turbo.log.price, auto.std.log.price, alternative = "two.sided")

#Bootstrap
mean.boot.auto.turbo.price = one.boot(auto.turbo.log.price, mean, R = 100000)
mean.boot.auto.std.price = one.boot(auto.std.log.price, mean, R = 100000)
plot.hist(mean.boot.auto.turbo.price$t, mean.boot.auto.std.price$t, c(" Bootstrapping Auto Price: Aspiration = Turbo", " Bootstrapping Auto Price: Aspiration = Std "))

print(paste0("Mean value of Auto Price with Aspiration = Turbo given by Bootstrap: $",mean.boot.auto.gas.price$t0))
print(paste0("Mean value of Auto Price with Aspiration = Std given by Bootstrap: $",mean.boot.auto.diesel.price$t0))

#Comparing  Log(Price) vs. Fuel Type
auto.gas.log.price = (auto[auto$fuel.type=="gas",]$price)
auto.diesel.log.price = (auto[auto$fuel.type == "diesel",]$price)
plot.hist(auto.gas.log.price, auto.diesel.log.price, c(" Auto Price: Fuel Type = Gas", "Auto Price: Fuel Type = Diesel "))

#T-test
t.test(auto.gas.log.price, auto.diesel.log.price, alternative = "two.sided")

#Bootstrap
mean.boot.auto.gas.price = one.boot(auto.gas.log.price, mean, R = 100000)
mean.boot.auto.diesel.price = one.boot(auto.diesel.log.price, mean, R = 100000)
plot.hist(mean.boot.auto.gas.price$t, mean.boot.auto.diesel.price$t, c(" Bootstrapping Auto Price: Fuel Type = Gas", " Bootstrapping Auto Price: Fuel Type = Diesel "))

print(paste0("Mean value of Auto Price with fuel = Gas given by Bootstrap",mean.boot.auto.gas.price$t0))
print(paste0("Mean value of Auto Price with fuel = Diesel given by Bootstrap",mean.boot.auto.diesel.price$t0))




#Compare the differences of the bootstrap resampled mean of the log price of the autos grouped by body style. 
#You will need to do this pair wise; e.g. between each possible pairing of body styles. Use both numerical and graphical methods 
#for your comparison. Which pairs of means are different within a 95% confidence interval? How do your conclusions compare to the 
#results you obtained from the ANOVA and Tukey’s HSD analysis you performed last week?

