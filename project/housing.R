library(ggplot2)
library(reshape2)
library(MASS)
house = read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
house[complete.cases(house),]
names(house)
dim(house)
str(house)

todrop = c("Fence", "MiscFeature","PoolQC","GarageType","GarageQual","GarageCond", "GarageFinish","GarageYrBlt",
           "FireplaceQu", "BsmtFinType1", "BsmtFinType2", "BsmtExposure","BsmtQual","BsmtCond","Alley","LotFrontage")
house[,todrop] = NULL
house = na.omit(house)

house.int = house[,c("MSSubClass","LotArea","OverallQual","OverallCond",
                      "MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF",
                     "X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath",
                     "BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr",
                     "TotRmsAbvGrd","Fireplaces","GarageCars","GarageArea",
                     "WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","PoolArea",
                     "MiscVal","MoSold","YrSold","SalePrice")]

summary(house.int)

cov(house.int)
cor(house.int)

#[1] "Id"            "MSSubClass"    "MSZoning"      "LotFrontage"   "LotArea"       "Street"        "Alley"        
#[8] "LotShape"      "LandContour"   "Utilities"     "LotConfig"     "LandSlope"     "Neighborhood"  "Condition1"   
#[15] "Condition2"    "BldgType"      "HouseStyle"    "OverallQual"   "OverallCond"   "YearBuilt"     "YearRemodAdd" 
#[22] "RoofStyle"     "RoofMatl"      "Exterior1st"   "Exterior2nd"   "MasVnrType"    "MasVnrArea"    "ExterQual"    
#[29] "ExterCond"     "Foundation"    "BsmtQual"      "BsmtCond"      "BsmtExposure"  "BsmtFinType1"  "BsmtFinSF1"   
#[36] "BsmtFinType2"  "BsmtFinSF2"    "BsmtUnfSF"     "TotalBsmtSF"   "Heating"       "HeatingQC"     "CentralAir"   
#[43] "Electrical"    "X1stFlrSF"     "X2ndFlrSF"     "LowQualFinSF"  "GrLivArea"     "BsmtFullBath"  "BsmtHalfBath" 
#[50] "FullBath"      "HalfBath"      "BedroomAbvGr"  "KitchenAbvGr"  "KitchenQual"   "TotRmsAbvGrd"  "Functional"   
#[57] "Fireplaces"    "FireplaceQu"   "GarageType"    "GarageYrBlt"   "GarageFinish"  "GarageCars"    "GarageArea"   
#[64] "GarageQual"    "GarageCond"    "PavedDrive"    "WoodDeckSF"    "OpenPorchSF"   "EnclosedPorch" "X3SsnPorch"   
#[71] "ScreenPorch"   "PoolArea"      "PoolQC"        "Fence"         "MiscFeature"   "MiscVal"       "MoSold"       
#3[78] "YrSold"        "SaleType"      "SaleCondition" "SalePrice" 

#todrop = c("Fence", "MiscFeature","PoolQC","GarageType","GarageQual","GarageCond", "GarageFinish","GarageYrBlt",
#           "FireplaceQu", "BsmtFinType1", "BsmtFinType2", "BsmtExposure","BsmtQual","BsmtCond","Alley","LotFrontage")
#hist(house$LotFrontage)
#hist(house$GarageYrBlt)

hist(house$MSSubClass)
hist(house$LotArea)
hist(house$OverallQual)
hist(house$OverallCond)
hist(house$MasVnrArea)
hist(house$BsmtFinSF1)
hist(house$BsmtFinSF2)
hist(house$BsmtUnfSF)
hist(house$TotalBsmtSF)
hist(house$X1stFlrSF + house$X2ndFlrSF)
hist(house$X2ndFlrSF)
hist(house$LowQualFinSF)
hist(house$GrLivArea)
hist(house$BsmtFullBath)
hist(house$BsmtHalfBath)
hist(house$FullBath)
hist(house$HalfBath)
hist(house$BedroomAbvGr)
hist(house$KitchenAbvGr)
hist(house$TotRmsAbvGrd)
hist(house$Fireplaces)

hist(house$GarageCars)
hist(house$GarageArea)
hist(house$WoodDeckSF)
hist(house$OpenPorchSF)
hist(house$EnclosedPorch)
hist(house$X3SsnPorch)
hist(house$PoolArea)
hist(house$MiscVal)
hist(house$MoSold)
hist(house$YrSold)
hist(house$SalePrice)
hist(house$LotArea)


ggplot(house, aes(SalePrice)) + geom_histogram(binwidth = 10000) + xlab("SalePrice") + ylab("Counts") + ggtitle(" Histogram of Sale Prices")
ggplot(house, aes(OverallQual)) + geom_histogram() + xlab("Quality Ratings") + ylab("Counts") + ggtitle(" Histogram of Quality Rating of Houses")
ggplot(house, aes(X1stFlrSF+X2ndFlrSF)) + geom_histogram(binwidth = 100) + xlab("Total Housing Area Square Feet") + ylab("Counts") + ggtitle("Histogram of House Area")
ggplot(house, aes(GarageCars)) + geom_histogram() + xlab("Garage Capacity") + ylab("Counts") + ggtitle("Histogram of Garage Capacity")
ggplot(house, aes(FullBath)) + geom_histogram() + xlab("# of Full Bath") + ylab("Counts") + ggtitle("Histogram of Full Baths")



#Exploratory Data Analyis

#Removed ones:
##todrop = c("Fence", "MiscFeature","PoolQC","GarageType","GarageQual","GarageCond", "GarageFinish","GarageYrBlt",
#           "FireplaceQu", "BsmtFinType1", "BsmtFinType2", "BsmtExposure","BsmtQual","BsmtCond","Alley","LotFrontage")
#ggplot(house, aes(y = SalePrice, x = LotArea)) + geom_point() + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
#ggplot(house, aes(y = SalePrice, x = Alley)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = BsmtQual)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = BsmtCond)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = BsmtExposure)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = BsmtFinType1)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = BsmtFinType2)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = FireplaceQu)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = GarageType)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = GarageYrBlt)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = GarageFinish)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = GarageQual)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = GarageCond)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = PoolQC)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = Fence)) + geom_point()
#ggplot(house, aes(y = SalePrice, x = MiscFeature)) + geom_point()




ggplot(house, aes(y = SalePrice, x = MSSubClass)) + geom_point()
ggplot(house, aes(y = SalePrice, x = MSZoning)) + geom_point()
ggplot(house, aes(y = SalePrice, x = LotFrontage)) + geom_point()
ggplot(house, aes(y = log(SalePrice), x = log(LotArea))) + geom_point() + xlab("Log of Lot Area") + ylab("Log of Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = Street)) + geom_point()
ggplot(house, aes(y = SalePrice, x = LotShape)) + geom_point()
ggplot(house, aes(y = SalePrice, x = LandContour)) + geom_point()
ggplot(house, aes(y = SalePrice, x = Utilities)) + geom_point()
ggplot(house, aes(y = SalePrice, x = LotConfig)) + geom_point()
ggplot(house, aes(y = SalePrice, x = LandSlope)) + geom_point()
ggplot(house, aes(y = SalePrice, x = Neighborhood)) + geom_point()
ggplot(house, aes(y = SalePrice, x = Condition1)) + geom_point()
ggplot(house, aes(y = SalePrice, x = Condition2)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BldgType)) + geom_point()
ggplot(house, aes(y = SalePrice, x = HouseStyle)) + geom_point()
ggplot(house, aes(y = SalePrice, x = OverallQual)) + geom_point() #Good indicator for price
ggplot(house, aes(y = SalePrice, x = OverallCond)) + geom_point()

ggplot(house, aes(y = SalePrice, x = YearBuilt)) + geom_point() # try Year built and house type

ggplot(house, aes(y = SalePrice, x = YearRemodAdd)) + geom_point()
ggplot(house, aes(y = SalePrice, x = RoofStyle)) + geom_point()
ggplot(house, aes(y = SalePrice, x = RoofMatl)) + geom_point()
ggplot(house, aes(y = SalePrice, x = Exterior1st)) + geom_point()
ggplot(house, aes(y = SalePrice, x = Exterior2nd)) + geom_point()
ggplot(house, aes(y = SalePrice, x = MasVnrType)) + geom_point()
ggplot(house, aes(y = SalePrice, x = MasVnrArea)) + geom_point()
ggplot(house, aes(y = SalePrice, x = ExterQual)) + geom_point()
ggplot(house, aes(y = SalePrice, x = ExterCond)) + geom_point()
ggplot(house, aes(y = SalePrice, x = Foundation)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BsmtFinSF1)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BsmtFinSF2)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BsmtUnfSF)) + geom_point()
ggplot(house, aes(y = SalePrice, x = TotalBsmtSF)) + geom_point() #Bsmt is a good indicator of price

ggplot(house, aes(y = SalePrice, x = BsmtFullBath)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BsmtHalfBath)) + geom_point()

ggplot(house, aes(y = SalePrice, x = Heating)) + geom_point()
ggplot(house, aes(y = SalePrice, x = HeatingQC)) + geom_point()
ggplot(house, aes(y = SalePrice, x = CentralAir)) + geom_point() #Impacts price

ggplot(house, aes(y = SalePrice, x = Electrical)) + geom_point() #Impacts price

ggplot(house, aes(y = SalePrice, x = X1stFlrSF)) + geom_point() #Good Indicator
ggplot(house, aes(y = SalePrice, x = X2ndFlrSF)) + geom_point()
ggplot(house, aes(y = SalePrice, x = GrLivArea)) + geom_point() #Good inicator
ggplot(house, aes(y = SalePrice, x = FullBath)) + geom_point() #Indicator
ggplot(house, aes(y = SalePrice, x = HalfBath)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BedroomAbvGr)) + geom_point()
ggplot(house, aes(y = SalePrice, x = KitchenAbvGr)) + geom_point()
ggplot(house, aes(y = SalePrice, x = KitchenQual)) + geom_point()
ggplot(house, aes(y = SalePrice, x = TotRmsAbvGrd)) + geom_point() #INdicator
ggplot(house, aes(y = SalePrice, x = Functional)) + geom_point()

ggplot(house, aes(y = SalePrice, x = Fireplaces)) + geom_point()


ggplot(house, aes(y = SalePrice, x = GarageCars)) + geom_point() #indicator
ggplot(house, aes(y = SalePrice, x = GarageArea)) + geom_point() #Indicator

ggplot(house, aes(y = SalePrice, x = PavedDrive)) + geom_point() #I

ggplot(house, aes(y = SalePrice, x = WoodDeckSF)) + geom_point()
ggplot(house, aes(y = SalePrice, x = OpenPorchSF)) + geom_point()
ggplot(house, aes(y = SalePrice, x = EnclosedPorch)) + geom_point()
ggplot(house, aes(y = SalePrice, x = X3SsnPorch)) + geom_point()
ggplot(house, aes(y = SalePrice, x = ScreenPorch)) + geom_point()

ggplot(house, aes(y = SalePrice, x = PoolArea)) + geom_point()
ggplot(house, aes(y = SalePrice, x = MiscVal)) + geom_point()

ggplot(house, aes(y = SalePrice, x = MoSold)) + geom_point()
ggplot(house, aes(y = SalePrice, x = YrSold)) + geom_point()

ggplot(house, aes(y = SalePrice, x = SaleType)) + geom_point()
ggplot(house, aes(y = SalePrice, x = SaleCondition)) + geom_point()

ggplot(house, aes(MSSubClass)) + geom_bar() 

n = names(house)
n = c("Id","MSSubClass","MSZoning","LotFrontage","LotArea")
for(i in seq_along(n)){
  print(ggplot(house, aes_string(x = n[i])) + geom_bar())
}

# All cateogrical variables
ggplot(house, aes(MSZoning)) + geom_bar()
ggplot(house, aes(Street)) + geom_bar()
ggplot(house, aes(Alley)) + geom_bar()
ggplot(house, aes(LotShape)) + geom_bar()
ggplot(house, aes(LandContour)) + geom_bar()
ggplot(house, aes(Utilities)) + geom_bar()
ggplot(house, aes(LotConfig)) + geom_bar()
ggplot(house, aes(LandSlope)) + geom_bar()
ggplot(house, aes(Neighborhood)) + geom_bar()
ggplot(house, aes(Condition1)) + geom_bar()
ggplot(house, aes(Condition2)) + geom_bar()
ggplot(house, aes(BldgType)) + geom_bar()
ggplot(house, aes(HouseStyle)) + geom_bar()
ggplot(house, aes(RoofStyle)) + geom_bar()
ggplot(house, aes(RoofMatl)) + geom_bar()
ggplot(house, aes(Exterior1st)) + geom_bar()
ggplot(house, aes(Exterior2nd)) + geom_bar()
ggplot(house, aes(MasVnrType)) + geom_bar()
ggplot(house, aes(ExterQual)) + geom_bar()
ggplot(house, aes(ExterCond)) + geom_bar()
ggplot(house, aes(BsmtQual)) + geom_bar()
ggplot(house, aes(BsmtCond)) + geom_bar()
ggplot(house, aes(BsmtExposure)) + geom_bar()
ggplot(house, aes(BsmtFinType1)) + geom_bar()
ggplot(house, aes(BsmtFinType2)) + geom_bar()
ggplot(house, aes(Heating)) + geom_bar()
ggplot(house, aes(HeatingQC)) + geom_bar()
ggplot(house, aes(CentralAir)) + geom_bar()
ggplot(house, aes(Electrical)) + geom_bar()
ggplot(house, aes(KitchenQual)) + geom_bar()
ggplot(house, aes(Functional)) + geom_bar()
ggplot(house, aes(FireplaceQu)) + geom_bar()
ggplot(house, aes(GarageType)) + geom_bar()
ggplot(house, aes(GarageFinish)) + geom_bar()
ggplot(house, aes(GarageQual)) + geom_bar()
ggplot(house, aes(GarageCond)) + geom_bar()
ggplot(house, aes(PavedDrive)) + geom_bar()
ggplot(house, aes(PoolQC)) + geom_bar()
ggplot(house, aes(Fence)) + geom_bar()
ggplot(house, aes(MiscFeature)) + geom_bar()
ggplot(house, aes(SaleType)) + geom_bar()
ggplot(house, aes(SaleCondition)) + geom_bar()


#Testing the price levels
#Histogram:
ggplot(house, aes(SalePrice)) + geom_histogram(binwidth = 10000)



#Boxplots
ggplot(house, aes(x = factor(CentralAir), y = SalePrice)) + geom_boxplot() #Significant
ggplot(house, aes(x = factor(MSZoning), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(Street), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(Alley), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(LotShape), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(LandContour), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(Utilities), y = SalePrice)) + geom_boxplot() #useless
ggplot(house, aes(x = factor(LotConfig), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(LandSlope), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(Neighborhood), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(Condition1), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(Condition2), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(BldgType), y = SalePrice)) + geom_boxplot() #Worth look
ggplot(house, aes(x = factor(HouseStyle), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(RoofStyle), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(RoofMatl), y = SalePrice)) + geom_boxplot() #worth a look
ggplot(house, aes(x = factor(Exterior1st), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(Exterior2nd), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(MasVnrType), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(ExterQual), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(ExterCond), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(BsmtQual), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(BsmtCond), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(BsmtExposure), y = SalePrice)) + geom_boxplot() #Worth look
ggplot(house, aes(x = factor(BsmtFinType1), y = SalePrice)) + geom_boxplot() 
ggplot(house, aes(x = factor(BsmtFinType2), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(Heating), y = SalePrice)) + geom_boxplot() #worth look
ggplot(house, aes(x = factor(HeatingQC), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(CentralAir), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(Electrical), y = SalePrice)) + geom_boxplot() #loook
ggplot(house, aes(x = factor(KitchenQual), y = SalePrice)) + geom_boxplot() #look
ggplot(house, aes(x = factor(Functional), y = SalePrice)) + geom_boxplot() # worth look
ggplot(house, aes(x = factor(FireplaceQu), y = SalePrice)) + geom_boxplot() #worth look
ggplot(house, aes(x = factor(GarageType), y = SalePrice)) + geom_boxplot() #worth look
ggplot(house, aes(x = factor(GarageFinish), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(GarageQual), y = SalePrice)) + geom_boxplot() #look
ggplot(house, aes(x = factor(GarageCond), y = SalePrice)) + geom_boxplot() #worth look
ggplot(house, aes(x = factor(PavedDrive), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(PoolQC), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(Fence), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(MiscFeature), y = SalePrice)) + geom_boxplot()
ggplot(house, aes(x = factor(SaleType), y = SalePrice)) + geom_boxplot() #Look
ggplot(house, aes(x = factor(SaleCondition), y = SalePrice)) + geom_boxplot() #Look

#Violin Plots:
ggplot(house, aes(x = factor(ExterCond), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Look
ggplot(house, aes(x = factor(BsmtQual), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Look
ggplot(house, aes(x = factor(BsmtCond), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Look
ggplot(house, aes(x = factor(BsmtExposure), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Worth look
ggplot(house, aes(x = factor(Heating), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #worth look
ggplot(house, aes(x = factor(CentralAir), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Useless
ggplot(house, aes(x = factor(Electrical), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #loook
ggplot(house, aes(x = factor(KitchenQual), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #look
ggplot(house, aes(x = factor(Functional), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) # worth look
ggplot(house, aes(x = factor(FireplaceQu), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #worth look
ggplot(house, aes(x = factor(GarageType), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #worth look
ggplot(house, aes(x = factor(GarageFinish), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Look
ggplot(house, aes(x = factor(GarageQual), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #look
ggplot(house, aes(x = factor(GarageCond), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #worth look
ggplot(house, aes(x = factor(PoolQC), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Look
ggplot(house, aes(x = factor(MiscFeature), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75))
ggplot(house, aes(x = factor(SaleType), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Look
ggplot(house, aes(x = factor(SaleCondition), y = SalePrice)) + geom_violin(trim = T, draw_quantiles = c(0.25,0.5,0.75)) #Look



#Hexbins:
ggplot(house, aes(y = SalePrice, x = LotArea)) + stat_binhex(bins = 10)
ggplot(house, aes(y = SalePrice, x = OverallQual)) + stat_binhex(bins = 10) #Good indicator for price
ggplot(house, aes(y = SalePrice, x = TotalBsmtSF)) + stat_binhex(bins = 10) #Bsmt is a good indicator of price
ggplot(house, aes(y = SalePrice, x = CentralAir)) + stat_binhex(bins = 10) #Impacts price
ggplot(house, aes(y = SalePrice, x = X1stFlrSF)) + stat_binhex(bins = 10) #Good Indicator
ggplot(house, aes(y = SalePrice, x = GrLivArea)) + stat_binhex(bins = 10) #Good inicator
ggplot(house, aes(y = SalePrice, x = FullBath)) + stat_binhex(bins = 10) #Indicator

#Multiple Dimensions:
ggplot(house, aes(y = SalePrice, x = log(LotArea))) + geom_point(aes(color= factor(MSZoning))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = log(LotArea))) + geom_point(aes(color= factor(LandContour))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price") #outliers
ggplot(house, aes(y = SalePrice, x = log(LotArea))) + geom_point(aes(color= factor(BldgType))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")

ggplot(house, aes(y = SalePrice, x = (X1stFlrSF))) + geom_point(aes(color= factor(MSZoning))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (X1stFlrSF))) + geom_point(aes(color= factor(HouseStyle))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (X1stFlrSF))) + geom_point(aes(color= factor(BldgType))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (X1stFlrSF))) + geom_point(aes(color= factor(YearBuilt))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (X1stFlrSF))) + geom_point(aes(color= factor(FullBath))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (X1stFlrSF))) + geom_point(aes(color= factor(HalfBath))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (X1stFlrSF))) + geom_point(aes(color= factor(GarageCars))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (X1stFlrSF))) + geom_point(aes(color= factor(Foundation))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")



ggplot(house, aes(y = SalePrice, x = (GrLivArea))) + geom_point(aes(color= factor(MSZoning))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (GrLivArea))) + geom_point(aes(color= factor(HouseStyle))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (GrLivArea))) + geom_point(aes(color= factor(BldgType))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (GrLivArea))) + geom_point(aes(color= factor(YearBuilt))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (GrLivArea))) + geom_point(aes(color= factor(FullBath))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (GrLivArea))) + geom_point(aes(color= factor(HalfBath))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (GrLivArea))) + geom_point(aes(color= factor(GarageCars))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = (GrLivArea))) + geom_point(aes(color= factor(Foundation))) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")


ggplot(house, aes(y = SalePrice, x = log(LotArea))) + geom_point(aes(color= factor(BldgType), size = X1stFlrSF+X2ndFlrSF), alpha = 0.7) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = log(LotArea))) + geom_point(aes(color= factor(Foundation), size = X1stFlrSF+X2ndFlrSF), alpha = 0.7) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = log(LotArea))) + geom_point(aes(color= factor(BldgType), size = PoolArea), alpha = 0.9) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")


ggplot(house, aes(y = SalePrice, x = log(LotArea))) + geom_point(aes(color= factor(BldgType), size = X1stFlrSF+X2ndFlrSF, shape = factor(FullBath)), alpha = 0.9) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = log(LotArea))) + geom_point(aes(color= factor(BldgType), size = PoolArea), alpha = 0.9) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")

ggplot(house, aes(y = SalePrice, x = OverallQual)) + geom_point(aes(color= factor(BldgType), size = LotArea), alpha = 0.9) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = OverallQual)) + geom_point(aes(color= factor(HouseStyle), size = LotArea), alpha = 0.9) + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")


#More plots
options(repr.plot.width=8, repr.plot.height=8)
require(car)
scatterplotMatrix(~LotArea + YearBuilt  + TotalBsmtSF + X1stFlrSF + GrLivArea + SalePrice, data = house)

#Plots
library(ellipse)
R = cor(house[, c('LotArea','OverallQual',"FullBath","GarageCars", 'YearBuilt', 'TotalBsmtSF', 'X1stFlrSF', 'GrLivArea', 'SalePrice')], method = 'pearson')
print(R)
plotcorr(R, col = colorRampPalette(c("firebrick3", "white", "navy"))(10))

options(repr.plot.width=16, repr.plot.height=16)
R = cor(house[, c("MSSubClass","LotArea","OverallQual","OverallCond",
                  "TotalBsmtSF","X1stFlrSF","GrLivArea","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr",
                  "TotRmsAbvGrd","Fireplaces","GarageCars","GarageArea",
                  "WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","PoolArea",
                  "SalePrice","LotArea")], method = 'pearson')




print(R)
plotcorr(R, col = colorRampPalette(c("firebrick3", "white", "navy"))(10))
library(corrplot)
corrplot(R, method="circle", type='lower')

#Regression:
house$logSalePrice = log(house$SalePrice)

lm.house = lm(logSalePrice ~.-SalePrice,data = house)
summary(lm.house)
plot(lm.house)

lm.house.aic = stepAIC(lm.house, direction = "both")
lm.house.aic$anova


house.svd = house
house.svd$SalePrice = NULL
cols = names(house.int)
cols = cols[cols!="SalePrice"]
cols = c(cols, "logSalePrice")
house.scale = house.svd
house.scale[,cols] = lapply(house.svd[,cols], function(x) { scale(x, center = TRUE, scale = TRUE)})
house.modmat = model.matrix(logSalePrice~., data = house.scale)


plot.svd.reg <- function(df, k){
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
  Ybar = mean(df$logSalePrice)
  SST <- sum((df$logSalePrice - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  RMSE = sqrt(SSR/(n-2))
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}



M = house.modmat[,-1]
M2 = t(M)%*%M
head(M2)

mSVD = svd(M2)
plot(log(mSVD$d))

inv.diag = rep(0, length(mSVD$d))
inv.diag[1:190] = 1/mSVD$d[1:190]
mD = diag(inv.diag)

#Inverse (t(M)*M) = V Diag U
mInV = mSVD$v %*% mD %*% t(mSVD$u)
b = house.scale$logSalePrice
#x = Inv(MTM)*t(M)*b
x = mInV %*% t(M) %*% b

x = ginv(M) %*% b
house.cp = house
house.cp$score =  (M %*% x)*sd(house.cp$logSalePrice) + mean(house.cp$logSalePrice)
house.cp$resids =  house.cp$score - house.cp$logSalePrice

RSS = sum((house.cp$logSalePrice - (house.cp$score))^2)
RSS
TSS = sum((house.cp$logSalePrice - mean(house.cp$logSalePrice))^2)
TSS
require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
plot.svd.reg(house.cp, length(names(house))-1)

#Elastic Net

#Plotting function for elastitc net
plot.elastic.reg <- function(df, k){
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
  Ybar = mean(df$logSalePrice)
  SST <- sum((df$logSalePrice - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  RMSE = sqrt(SSR/(n-2))
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(RMSE), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}


library(glmnet)
#M = ModelMatrix
#auto.cp$price = log price
#Specifying the dependent variable and the features
M = house.modmat[,-1]
b = house.cp$logSalePrice

#Runnnign ridge + Lasso
mod.ridge.lasso = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 0.5)

#Storing the predictions in the Auto data frame
house.cp$score = predict(mod.ridge.lasso, newx = M)[, 15]
house.cp$resids = house.cp$score - house.cp$logSalePrice

#Plotting the decay of variables based on lambda value
plot(mod.ridge.lasso, xvar = 'lambda', label = TRUE)
plot(mod.ridge.lasso, xvar = 'dev', label = TRUE)

plot.elastic.reg(house.cp, length(names(house))-1)

