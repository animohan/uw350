#Compute the probability that the driver of a car is texting at a specific intersection.
# Nationally the cumulative probability that a driver is texting is:
# P = 0.5, at x = 0.1
# P = 0.75 at x = 0.3

#  Given these data
#   Compute the Beta prior, and report the coefficients
library(LearnBayes)

#Setting the Beta Prior
# Nationally the *cumulative probability* that a driver is texting is:
# P = 0.5, at x = 0.1
# P = 0.75 at x = 0.3 -> Cumulative Probability hence p=0.25 at x = 0.3

beta.prior = beta.select(list(p = 0.5, x = 0.1), list(p = 0.75, x = 0.3))
beta.prior

  
#You observe cars at a location three times and note the number of texting drivers:
#1. 2 texting out of 20 drivers
#2. 4 texting out of 20 drivers
#3. 1 texting out of 20 drivers
#   Plot the prior, likelihood and posterior three times as you update your belief based on collecting more data
require(repr)
options(repr.plot.width = 6, repr.plot.height = 5)
triplot(beta.prior, c(2,18))
triplot(beta.prior, c(6,34))
triplot(beta.prior, c(7,53))

#   Simulate the final posterior distribution and do the following:
#   Plot the posterior with the 90% HDI shown
#   Report the upper and lower limits of the 90% HDI

#Posterior Distributions
options(repr.plot.width = 8, repr.plot.height = 5)
final.posterior = beta.prior + c(7,53)
post.sample = rbeta(10000, final.posterior[1], final.posterior[2])
par(mfrow = c(1,1))
quants = quantile(post.sample, c(0.05, 0.95))
breaks = seq(min(post.sample), max(post.sample), length.out = 25)

hist(post.sample, breaks = breaks, main = "Distribution of samples with 90% HDI", xlab = "Probability of Crash", ylab ="# of Instances")
abline(v = quants[1], lty = 3, col = "red", lwd = 3)
abline(v = quants[2], lty = 3, col = "red", lwd = 3)
#predplot(final.posterior,60,7 )

#90% HDI on Prior Distribution
prior.sample = rbeta(10000, beta.prior[1], beta.prior[2])
par(mfrow = c(1,1))
quants = quantile(prior.sample, c(0.05, 0.95))
breaks = seq(min(prior.sample), max(prior.sample), length.out = 25)

hist(prior.sample, breaks = breaks, main = "Distribution of Prior samples with 90% HDI", xlab = "Probability of Crash", ylab ="# of Instances")
abline(v = quants[1], lty = 3, col = "red", lwd = 3)
abline(v = quants[2], lty = 3, col = "red", lwd = 3)

#   Of the next hundred drivers what are the number of texting drivers in the 90% HDI?
n = 100
s = 0:n
pred.probs = pbetap(final.posterior, n, s)
plot(s, pred.probs, type = "h",
     main = "Probability distribution of # of accidents in next 100 drivers", xlab = "# of Accidents")
discint(cbind(s, pred.probs),0.90)
interval = discint(cbind(s, pred.probs),0.90)$set
abline(v = interval[1], lty = 3, col = "red", lwd = 3)
abline(v = interval[length(interval)], lty = 3, col = "red", lwd = 3)
mean(pred.probs)

#Are the drivers in this area better or worse that the national figures indicate?
n = 100
s = 0:n
pred.probs.national = pbetap(beta.prior, n, s)
plot(s, pred.probs.national, type = "h",
     main = "Probability distribution of # of accidents in 100 National drivers", xlab = "# of Accidents")
discint(cbind(s, pred.probs.national),0.90)
interval = discint(cbind(s, pred.probs.national),0.90)$set
abline(v = interval[1], lty = 3, col = "red", lwd = 3)
abline(v = interval[length(interval)], lty = 3, col = "red", lwd = 3)
mean(pred.probs.national)
