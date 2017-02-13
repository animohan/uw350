.99 * 0.0003125 /(.99 * 0.0003125 + .01 * (1 - 0.0003125))

eyeHair = data.frame(Black = c(0.11, 0.03, 0.03, 0.01), 
                     Brunette = c(0.2, 0.14, 0.09, 0.05),
                     Red = c(0.04, 0.03, 0.02, 0.02),
                     Blond = c(0.01, 0.16, 0.02, 0.03))
row.names(eyeHair) = c('Brown', 'Blue', 'Hazel', 'Green')
eyeHair

## Compute the marginal distribution of hair color and eye color
eyeHair = rbind(eyeHair, apply(eyeHair, 2, sum))
eyeHair$Marginal_eye = apply(eyeHair, 1, sum)
row.names(eyeHair) = c('Brown', 'Blue', 'Hazel', 'Green', 'Marginal_hair')
eyeHair

# Probability of HairColor/Blue Eye
eyeHair['Blue',]/eyeHair['Blue','Marginal_eye']


alpha = c(0.5,1,2,3,4)
beta = alpha
x = seq(0.001,0.999,length=100)

par(mfrow = c(5,5), mar=c(2,1,2,1)) # mar = c(bottom, left, top, right)
sapply(alpha, function(a){
  sapply(beta, function(b){
    plot_title = paste("(a,b)=(",a,",",b,")")
    plot(x,dbeta(x,a,b),xlab="",ylab="",
         main=plot_title, type="l", lwd=2)
  })
})

# Set plot options back to normal
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))

library(LearnBayes)
## I think the chance of rain is 0.2 with
## with a probability at the 75% point of 0.28
## Compute my Beta prior
beta.par <- beta.select(list(p=0.5, x=0.2), list(p=0.75, x=.28))
beta.par ## The parameters of my Beta distribution

require(repr)
options(repr.plot.width=6, repr.plot.height=5)
triplot(beta.par, c(0, 0))

beta.par + c(6, 4)
triplot(beta.par, c(6, 4))

beta.par + c(11, 9)
triplot(beta.par, c(11, 9))

beta.par + c(25, 15)
triplot(beta.par, c(25, 15))

# Simulate from the posterior and 
## compute confidence intervals
options(repr.plot.width=8, repr.plot.height=5)
beta.post.par <- beta.par + c(6 + 6 + 12 + 12, 4 + 4 + 8 + 8)
post.sample <- rbeta(10000, beta.post.par[1], beta.post.par[2])
par(mfrow = c(1,2))
quants = quantile(post.sample, c(0.05, 0.95))
breaks = seq(min(post.sample), max(post.sample), length.out = 41)
hist(post.sample, breaks = breaks, 
     main = 'Distribution of samples \n with 90% HDI',
     xlab = 'Sample value',
     ylab = 'Density')
abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
qqnorm(post.sample)
par(mfrow = c(1,1))
quants

## Check on the model
predplot(beta.post.par, 25, 15)

## What is the probability of observing 0-8 successes in the
## next 60 trials?
n <- 60
s <- 0:n
pred.probs <- pbetap(beta.post.par, n, s)
plot(s, pred.probs, type="h", 
     main = 'Probability distribution of success in trail',
     xlab = 'Successes')
discint(cbind(s, pred.probs), 0.90)


##--- Discrete priors ------ 
#
# Create a uniform prior
p <- seq(0, 1, by = 0.01)
prior <- 1 / 101 + 0 * p

## With 20 successes and 12 failures find posterior
library(LearnBayes)
tries = c(20, 12)
post <- pdisc(p, prior, tries)
HCI = discint(cbind(p, post), 0.90) # 90 percent HCI

## Plot the prior and posterior
par(mfrow = c(2,1))
plot(p, prior, type="h",main="Posterior Distribution")
Main = paste("Posterior Distribution with N =",
             as.character(sum(tries)), 'Z =',
             as.character(tries[1]),
             'mu = ',
             as.character(tries[1]/sum(tries)))
plot(p, post, type="h",main=Main)
abline(v = HCI$set[1], lty = 3, col = 'red', lwd = 3)
abline(v = HCI$set[length(HCI$set)], , lty = 3, col = 'red', lwd = 3)
abline(v = tries[1]/sum(tries), col = 'red', lwd = 3)
par(mfrow = c(1,1))

## Print the 90 percent HCI 
HCI

## Make a prediction for the distribution of the 
## next 20 observations
n <- 20
s <- 0:20
pred.probs <- pdiscp(p, post, n, s)
plot(s, pred.probs, type="h",
     main="Predictive Distribution")
