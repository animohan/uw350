x = c(rnorm(1000),rnorm(1000,mean=3,sd=0.5))
plot(density(x)) # Definitely not normal



# generate 500 samples
x_samples = lapply(1:500, function(i) sample(x, size=50, replace=FALSE))
x_means = sapply(x_samples, mean)
breaks = seq(min(x_means), max(x_means), length.out = 40)
hist(unlist(x_means), breaks = breaks)
qqnorm(unlist(x_means)) # Yay normality!

pop_mean_estimate = mean(unlist(x_means))
pop_mean_estimate
pop_mean_sd = sd(unlist(x_means))
pop_mean_sd

actual_mean = mean(x)
actual_mean

alpha = 0.95
half_width = qnorm((1+alpha)/2, mean=pop_mean_estimate, sd = pop_mean_sd) - pop_mean_estimate
print(paste("The half width is ", round(half_width, 3)))

ci_low = pop_mean_estimate - half_width
ci_high = pop_mean_estimate + half_width

print(paste('The actual mean is',round(actual_mean,3)))
print(paste('The',alpha,'level CI is (',round(ci_low,3),',',round(ci_high,3),').'))

u = runif(10000)
plot(density(u)) # Definitely not normal


u_samples = lapply(1:500, function(i) sample(u, size=50, replace=TRUE))
u_means = sapply(u_samples, mean)
breaks = seq(min(u_means), max(u_means), length.out = 40)
hist(unlist(u_means), breaks = breaks)
qqnorm(unlist(u_means)) # Yay normality!
