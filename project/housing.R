library(ggplot2)
library(reshape2)
house = read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
house[complete.cases(house),]
names(house)
dim(house)
str(house)

house.int = house[,c("MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond",
                      "MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF",
                     "X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath",
                     "BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr",
                     "TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars","GarageArea",
                     "WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","PoolArea",
                     "MiscVal","MoSold","YrSold","SalePrice","LotArea")]

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

hist(house$MSSubClass)
hist(house$LotFrontage)
hist(house$LotArea)
hist(house$OverallQual)
hist(house$OverallCond)
hist(house$MasVnrArea)
hist(house$BsmtFinSF1)
hist(house$BsmtFinSF2)
hist(house$BsmtUnfSF)
hist(house$TotalBsmtSF)
hist(house$X1stFlrSF)
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
hist(house$GarageYrBlt)
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
#Exploratory Data Analyis

ggplot(house, aes(y = SalePrice, x = MSSubClass)) + geom_point()
ggplot(house, aes(y = SalePrice, x = MSZoning)) + geom_point()
ggplot(house, aes(y = SalePrice, x = LotFrontage)) + geom_point()
ggplot(house, aes(y = SalePrice, x = LotArea)) + geom_point() + xlab("Lot Area") + ylab("Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = log(SalePrice), x = log(LotArea))) + geom_point() + xlab("Log of Lot Area") + ylab("Log of Sale Price") + ggtitle("Relationship between Lot Area and Sale Price")
ggplot(house, aes(y = SalePrice, x = Street)) + geom_point()
ggplot(house, aes(y = SalePrice, x = Alley)) + geom_point()
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
ggplot(house, aes(y = SalePrice, x = BsmtQual)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BsmtCond)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BsmtExposure)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BsmtFinType1)) + geom_point()
ggplot(house, aes(y = SalePrice, x = BsmtFinType2)) + geom_point()
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
ggplot(house, aes(y = SalePrice, x = FireplaceQu)) + geom_point()

ggplot(house, aes(y = SalePrice, x = GarageType)) + geom_point()
ggplot(house, aes(y = SalePrice, x = GarageYrBlt)) + geom_point()
ggplot(house, aes(y = SalePrice, x = GarageFinish)) + geom_point()
gplot(house, aes(y = SalePrice, x = GarageCars)) + geom_point() #indicator
ggplot(house, aes(y = SalePrice, x = GarageArea)) + geom_point() #Indicator
ggplot(house, aes(y = SalePrice, x = GarageQual)) + geom_point()
ggplot(house, aes(y = SalePrice, x = GarageCond)) + geom_point()

ggplot(house, aes(y = SalePrice, x = PavedDrive)) + geom_point() #I

ggplot(house, aes(y = SalePrice, x = WoodDeckSF)) + geom_point()
ggplot(house, aes(y = SalePrice, x = OpenPorchSF)) + geom_point()
ggplot(house, aes(y = SalePrice, x = EnclosedPorch)) + geom_point()
ggplot(house, aes(y = SalePrice, x = X3SsnPorch)) + geom_point()
ggplot(house, aes(y = SalePrice, x = ScreenPorch)) + geom_point()

ggplot(house, aes(y = SalePrice, x = PoolArea)) + geom_point()
ggplot(house, aes(y = SalePrice, x = PoolQC)) + geom_point()

ggplot(house, aes(y = SalePrice, x = Fence)) + geom_point()
ggplot(house, aes(y = SalePrice, x = MiscFeature)) + geom_point()

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



