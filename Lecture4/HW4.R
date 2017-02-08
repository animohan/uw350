# Compare and test Normality the distributions of price and log price – Use both a graphical method and a formal test.

# Test significance of price (log price) stratified by a) fuel type, b) aspiration, and c) rear vs. front wheel drive. Use both graphical methods and the formal test.



auto = read.csv("Automobile price data _Raw_.csv", header = TRUE, stringsAsFactors = FALSE)
numcols = c("price","bore","stroke","horsepower","peak.rpm")
auto[, numcols] <- lapply(auto[, numcols], as.numeric)
auto = auto[complete.cases(auto),]

# Normality of Price (In Automobile):
auto.price = auto$price
auto.log.price = log(auto$price)
qqnorm(auto.price, main = "Q-Q Plot of Auto - Price")
qqnorm(auto.log.price, main = "Q-Q Plot of Auto - Log(Price)")

# KS Test for normality
auto.log.price.scaled = scale(auto.log.price, center = TRUE, scale = TRUE)
auto.price.scaled = scale(auto.price, center = TRUE, scale = TRUE)
summary(auto.log.price.scaled)
summary(auto.price.scaled)

plot(ecdf(auto.log.price.scaled), col = "blue", main = "CDF of Auto- Price", xlab = "Auto Price", ylab = "Cumulative Density")
lines(ecdf(norm1), col = "red")

#K-S Statistics
norm1 = rnorm(200,mean=0,sd=1)
x_seq = seq(-3,4,len=200)

norm.cdf = sapply(x_seq, function(x){
  sum(norm1<x)/length(norm1)
})

log.price.cdf = sapply(x_seq, function(x){
  sum(auto.log.price.scaled<x)/length(auto.log.price.scaled)
})

price.cdf = sapply(x_seq, function(x){
  sum(auto.price.scaled<x)/length(auto.price.scaled)
})

k_s_stat = max(abs(norm.cdf - log.price.cdf))
k_s_stat
# where does it occur?
k_index = which.max(abs(norm.cdf - log.price.cdf))
k_s_x = x_seq[k_index]
plot(x_seq,norm.cdf, col='blue', pch=16, main ='CDFs of standardized samples', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,log.price.cdf,col='red', pch=16)
lines(c(k_s_x,k_s_x), c(norm.cdf[k_index],log.price.cdf[k_index]),
      col='black', lwd=8)
print(paste0("Comparing Normal CDF and Log Price CDF: K-S=", k_s_stat))


k_s_stat = max(abs(norm.cdf - price.cdf))
k_s_stat
# where does it occur?
k_index = which.max(abs(norm.cdf - price.cdf))
k_s_x = x_seq[k_index]
plot(x_seq, norm.cdf, col='blue', pch=16, main ='CDFs of standardized samples', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,price.cdf,col='red', pch=16)
lines(c(k_s_x,k_s_x), c(norm.cdf[k_index],price.cdf[k_index]),
      col='black', lwd=8)
print(paste0("Comparing Normal CDF and Auto Price CDF: K-S=", k_s_stat))


# Question 2

plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 20){
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  par(mfrow = c(2, 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = cols[1])
  abline(v = mean(a), lwd = 4, col = 'red')
  hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = cols[2])
  abline(v = mean(b), lwd = 4, col = 'red')
  par(mfrow = c(1, 1))
}

auto.gas.price = auto.df[auto$fuel.type=="gas",]$price
auto.diesel.price = auto.df[auto$fuel.type == "diesel",]$price
plot.t(auto.gas.price, auto.diesel.price, cols = c("Auto Price: Fuel Type = Gas","Auto Price: Fuel Type = Diesel"))
t.test(auto.gas.price, auto.diesel.price)

auto.std.price = auto.df[auto$aspiration=="std",]$price
auto.turbo.price = auto.df[auto$aspiration == "turbo",]$price
plot.t(auto.std.price, auto.turbo.price, cols = c("Auto Price: Aspiration = Std","Auto Price: Aspiration = Turbo"))
t.test(auto.std.price, auto.turbo.price)

auto.rwd.price = auto.df[auto$drive.wheels=="rwd",]$price
auto.fwd.price = auto.df[auto$drive.wheels == "fwd",]$price
plot.t(auto.rwd.price, auto.fwd.price, cols = c("Auto Price: Drive Wheels = Rear","Auto Price: Drive Wheels = Front"))
t.test(auto.rwd.price, auto.fwd.price)


#Q3
# Apply ANOVA to the auto price data to compare the price (or log price if closer to a Normal distribution) of autos stratified by number of doors, and body style – two sets of tests.
# Graphically explore the differences between the price conditioned by the categories of each variable – Hint, make sure you have enough data for each category.
# Use standard ANOVA and Tukey ANOVA to test the differences of these groups.

auto = read.csv("Automobile price data _Raw_.csv", header = TRUE, stringsAsFactors = FALSE, na.string = "?")
numcols = c("price","bore","stroke","horsepower","peak.rpm")
auto[, numcols] <- lapply(auto[, numcols], as.numeric)
auto = auto[complete.cases(auto),]

#auto.door.sub = data.frame(auto$price, as.factor(auto$num.of.doors))
#colnames(auto.door.sub)= c("price","doors")
boxplot(price~num.of.doors, data = auto)
auto.door.aov = aov(price ~ as.factor(num.of.doors), data = auto)
summary(auto.door.aov)
print(auto.door.aov)
tukey.door.anova = TukeyHSD(auto.door.aov)
tukey.door.anova
plot(tukey.door.anova)


boxplot(price~body.style, data = auto)
auto.body.aov = aov(price ~ as.factor(body.style), data = auto)
summary(auto.body.aov)
print(auto.body.aov)
tukey.body.anova = TukeyHSD(auto.body.aov)
tukey.body.anova
plot(tukey.body.anova)



auto.twoDoor.price = log(auto[auto$num.of.doors=="two",]$price)
auto.fourDoor.price = log(auto[auto$num.of.doors == "four",]$price)

auto.convertible.price = log(auto[auto$body.style =="convertible",]$price)
auto.hardtop.price = log(auto[auto$body.style =="hardtop",]$price)
auto.hatchback.price = log(auto[auto$body.style =="hatchback",]$price)
auto.sedan.price = log(auto[auto$body.style =="sedan",]$price)
auto.wagon.price = log(auto[auto$body.style =="wagon",]$price)


plot.t(auto.twoDoor.price, auto.fourDoor.price, cols = c("Auto Price: Num of Doors = 2","Auto Price: Num of Doors = 4"))
t.test(auto.twoDoor.price, auto.fourDoor.price)


#DELETE
#{
# Use Student-t test:
    norm1 = rnorm(200, mean = 0, sd = 1)
    norm2 = rnorm(200, mean = 0, sd = 1)
    t.test(norm1, auto.price, alternative = "two.sided")
    t.test(norm1, auto.log.price, alternative = "two.sided")
    
    auto.price.scaled = scale(auto.price, center = TRUE, scale = TRUE)
    auto.log.price.scaled = scale(auto.log.price, center = TRUE, scale = TRUE)
    
    t.test(norm1, auto.log.price.scaled, alternative = "two.sided")
    t.test(norm1, auto.price.scaled, alternative = "two.sided")
#}