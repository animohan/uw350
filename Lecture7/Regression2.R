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
