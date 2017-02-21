#You are asked to compute the probability that the driver of a car is texting at a specific intersection.
# Nationally the cumulative probability that a driver is texting is:
# P = 0.5, at x = 0.1
# P = 0.75 at x = 0.3

library(LearnBayes)
beta.prior = beta.select(list(p = 0.5, x = 0.1), list(p = 0.75, x = 0.28))
beta.prior
  
#You observe cars at a location three times and note the number of texting drivers:
#  1.2 texting out of 20 drivers
#  2.4 texting out of 20 drivers
#  3.1 texting out of 20 drivers
  
#  Given these data
#   Compute the Beta prior, and report the coefficients
#   Plot the prior, likelihood and posterior three times as you update your belief based on collecting more data
#   Simulate the final posterior distribution and do the following:
#   Plot the posterior with the 90% HDI shown
#   Report the upper and lower limits of the 90% HDI
#   Of the next hundred drivers what are the number of texting drivers in the 90% HDI?
#   Are the drivers in this area better or worse that the national figures indicate?

require(repr)
options(repr.plot.width = 6, repr.plot.height = 5)
triplot(beta.prior, c(20,2))
triplot(beta.prior, c(40,6))
triplot(beta.prior, c(60,7))

#Posterior Distributions
options(repr.plot.width = 8, repr.plot.height = 5)
final.posterior = beta.prior + c(60,7)
post.sample = rbeta(10000, final.posterior[1], final.posterior[2])
par(mfrow = c(1,2))
quants = quantile(post.sample, c(0.05, 0.95))
breaks = seq(min(post.sample), max(post.sample), length.out = 25)

hist(post.sample, breaks = breaks, main = "Distribution of samples with 90% HDI", xlab = "Sample Value", ylab ="Density")
abline(v = quants[1], lty = 3, col = "red", lwd = 3)
abline(v = quants[2], lty = 3, col = "red", lwd = 3)
