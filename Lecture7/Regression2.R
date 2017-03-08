#Preparing the Data
#Step 1:
  # Subset the data to just male adult children
  # Computer two new features, the heights of the parents squared
  # Zscore scale the features

require(HistData)
require(dplyr)
males = GaltonFamilies[GaltonFamilies$gender == 'male',]
males.ext = males[,c('mother', 'father','childHeight')]
males.ext = mutate(males.ext, mother.sqr = mother^2, father.sqr = father^2)
males.ext[,c('mother','father','mother.sqr','father.sqr')] = lapply(males.ext[,c('mother',"father","mother.sqr","father.sqr")], scale)
str(males.ext)
head(males.ext)


#Computing a model with all the features
lm.males = lm(childHeight ~., data = males.ext)
summary(lm.males)
plot(lm.males)

#Using stepwise regression:
library(MASS)
lm.step = stepAIC(lm.males, direction = "both")
lm.step$anova
summary(lm.step)
plot(lm.step)


#Categorical variables;
Galton.scaled = GaltonFamilies[, c('mother', 'father', 'childHeight', 'gender')]
Galton.scaled = mutate(Galton.scaled, 
                       mother.sqr = mother^2, father.sqr = father^2)
Galton.scaled[, c('mother', 'father', 'mother.sqr', 'father.sqr')] = 
  lapply(Galton.scaled[, c('mother', 'father', 'mother.sqr', 'father.sqr')], 
         scale)
str(Galton.scaled)
mod.mat = model.matrix(childHeight ~ ., data = Galton.scaled)

lm.males = lm(childHeight ~ ., data = males.ext)
summary(lm.males)
plot(lm.males)

library(MASS)
lm.step = stepAIC(lm.males, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model
plot(lm.step)

lm.interaction = lm(childHeight ~ mother*father, data = males.ext)
summary(lm.interaction)
plot(lm.interaction)

M = as.matrix(males.ext[, c('mother', 'father', 'mother.sqr', 'father.sqr')])
head(M)
MTM = t(M) %*% M
MTM



#______________________________ Towarde end:
plot.svd.reg2 <- function(df, k = 4){
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

require(glmnet)
b = as.matrix(males.ext$childHeight)
mod.ridge.lasso = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 0.5)
plot(mod.ridge.lasso, xvar = 'lambda', label = TRUE)
plot(mod.ridge.lasso, xvar = 'dev', label = TRUE)


males.ext$score = predict(mod.ridge.lasso, newx = M)[, 15]
males.ext$resids = males.ext$score - males.ext$childHeight



plot.svd.reg2(males.ext)

