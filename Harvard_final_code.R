#We will need some standard libraries
library(tidyverse)
library(caret)
library(maptree)

#Read in the data
train<-read.csv("train.csv")
test<-read.csv("test.csv")

set.seed(2)

#Create a function to calculate RMSE(performance evaluation)
RMSE<-function(predicted_price,actual_price){
  sqrt(mean((predicted_price-actual_price)^2))}


#Impute Missing Values with commonly occuring values
train%>%ggplot(aes(x=MasVnrArea,y=SalePrice))+geom_point()+scale_y_continuous(labels = scales::comma)+ggtitle("Masonry Veneer Area Distribution")+theme(plot.title = element_text(size = 12, face = "bold",hjust=0.5))
#0 is very common
train$MasVnrArea[is.na(train$MasVnrArea)]<-0

train%>%group_by(SaleType)%>%summarize(count=n())
train$SaleType[is.na(train$SaleType)]<-"WD"

train%>%group_by(Functional)%>%summarize(count=n())
train$Functional[is.na(train$Functional)]<-"Typ"

train%>%group_by(Exterior1st)%>%summarize(count=n())
train$Exterior1st[is.na(train$Exterior1st)]<-"VinylSd"

train%>%group_by(MSZoning)%>%summarize(count=n())
train$MSZoning[is.na(train$MSZoning)]<-"RL"

#Some values are missing because conditions are likely Not Applicable, i.e. There is no Garage etc.
#I believe this may be the case because other conditions like GarageQual etc. are also missing
#Note there may be some missing values in test rather than train and vice versa, the code is just copy pasted for both sets
train$GarageArea[is.na(train$GarageArea)]<-0
train$GarageCars[is.na(train$GarageCars)]<-0
train[is.na(train$TotalBsmtSF),c("TotalBsmtSF","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF")]<-0
train[is.na(train$BsmtFullBath),c("BsmtFullBath","BsmtHalfBath")]<-0

#Explore all variables
train<-train%>%mutate(LSP=log(SalePrice))

train%>%ggplot(aes(y=LSP,x=MSZoning))+geom_boxplot()
#MSZoning is a useful variable

#We will create new variables, each corresponding to one category of MSZoning
#Since we need to do this many times, we will do it automatically with dynamic names

MSZ_list<-unique(train$MSZoning)
#First we will generate the list of all categories
for(MSZ in MSZ_list){
  varname<-paste(MSZ,"MSZ_detect")
  #Then we create a new variable name for each category
  train<-train%>%mutate(!!varname:=MSZoning==MSZ)
  #Finally we will use a dynamic variable name to create a new variable for each category
}               

train%>%ggplot(aes(y=LSP,x=LotFrontage))+geom_point()
#Its useful but it has many missing values. Can we impute these values?
train%>%filter(LotArea<38000)%>%ggplot(aes(y=LotFrontage,x=LotArea))+geom_point(aes(color=LotShape))+geom_smooth(method="loess")
train%>%filter(LotArea<38000)%>%ggplot(aes(y=LotFrontage,x=LotArea))+geom_point()+geom_smooth(method="loess")+facet_wrap(~LotConfig)
#CulDSac and FR3 does have a slightly different distribution but I am not using it because most of the points(Corner or Inside) follow the same curve and I want to avoid complexity 

train_temp<-train
fit_LotAreaVsFrontage<-loess(LotFrontage~LotArea,data=train_temp)
train_missing<-train[is.na(train$LotFrontage),]
train_missing_predict<-predict(fit_LotAreaVsFrontage,train_missing)
train[is.na(train$LotFrontage),"LotFrontage"]<-train_missing_predict

train%>%ggplot(aes(y=LSP,x=LotArea))+geom_point()
train%>%filter(LotArea<50000)%>%ggplot(aes(y=LSP,x=LotArea))+geom_point()+geom_smooth(method="loess")
#LotArea Does correlate with LSP

train%>%ggplot(aes(y=LSP,x=LotShape))+geom_boxplot()
#LotShape is only mildly correlated to SalePrice
#LotShape is reduced to Reg/Irreg
train<-train%>%mutate(BLotShape=(LotShape=="Reg"))
train%>%ggplot(aes(y=LSP,x=BLotShape))+geom_boxplot()

train%>%ggplot(aes(y=LSP,x=LotConfig))+geom_boxplot()
#We can say there are two categories , CulDSac+Fr3 and Others

#LotConfig is reduced to single 1/0
train<-train%>%mutate(BLotConfig=(LotConfig=="CulDSac"|LotConfig=="FR3"))
train%>%ggplot(aes(y=LSP,x=BLotConfig))+geom_boxplot()

train%>%ggplot(aes(y=LSP,x=Street))+geom_boxplot()
#Useful, turn into binary
train<-train%>%mutate(BStreet=str_detect(Street,"Grvl"))

train%>%ggplot(aes(y=LSP,x=Alley))+geom_boxplot()
#NA is actually no alley access
train[,"Alley"]<-as.character(train$Alley)
train[is.na(train$Alley),"Alley"]<-as.character("NA")
Alley_list<-unique(train$Alley)
for(Alley_loop in Alley_list){
  varname<-paste(Alley_loop,"Alley_detect")
  train<-train%>%mutate(!!varname:=Alley==Alley_loop)
}  

train%>%ggplot(aes(y=LSP,x=LandContour))+geom_boxplot()
LC_list<-unique(train$LandContour)
for(LC_loop in LC_list){
  varname<-paste(LC_loop,"LC_detect")
  train<-train%>%mutate(!!varname:=LandContour==LC_loop)
} 

train%>%ggplot(aes(y=LSP,x=Utilities))+geom_boxplot()
sum((train$Utilities=="NoSeWa"))
#Only One NoSeWa point, drop this column

train%>%ggplot(aes(y=LSP,x=LandSlope))+geom_boxplot()
LS_list<-unique(train$LandSlope)
for(LS_loop in LS_list){
  varname<-paste(LS_loop,"LS_detect")
  train<-train%>%mutate(!!varname:=LandSlope==LS_loop)
} 

train%>%ggplot(aes(y=LSP,x=Condition1))+geom_boxplot()
Cond1_list<-unique(train$Condition1)
for(Cond1_loop in Cond1_list){
  varname<-paste(Cond1_loop,"Cond1_detect")
  train<-train%>%mutate(!!varname:=Condition1==Cond1_loop)
} 

train%>%ggplot(aes(y=LSP,x=Condition2))+geom_boxplot()

Cond2_list<-unique(train$Condition2)
for(Cond2_loop in Cond2_list){
  varname<-paste(Cond2_loop,"Cond2_detect")
  train<-train%>%mutate(!!varname:=Condition2==Cond2_loop)
} 

train%>%ggplot(aes(y=LSP,x=Neighborhood))+geom_boxplot()
Nbor_list<-unique(train$Neighborhood)
for(Nbor_loop in Nbor_list){
  varname<-paste(Nbor_loop,"Nbor_detect")
  train<-train%>%mutate(!!varname:=Condition2==Cond2_loop)
}

train%>%ggplot(aes(y=LSP,x=BldgType))+geom_boxplot()
Bldg_list<-unique(train$BldgType)
for(Bldg_loop in Bldg_list){
  varname<-paste(Bldg_loop,"Bldg_detect")
  train<-train%>%mutate(!!varname:=BldgType==Bldg_loop)
}

train%>%ggplot(aes(y=LSP,x=HouseStyle))+geom_boxplot()
House_list<-unique(train$HouseStyle)
for(House_loop in House_list){
  varname<-paste(House_loop,"House_detect")
  train<-train%>%mutate(!!varname:=HouseStyle==House_loop)
}

train%>%ggplot(aes(y=LSP,x=RoofStyle))+geom_boxplot()
#RoofStyle does not differentiate, drop

train%>%ggplot(aes(y=LSP,x=RoofMatl))+geom_boxplot()
#Not a balanced and useful distribution, drop

train%>%ggplot(aes(y=LSP,x=Exterior1st))+geom_boxplot()
Ext1_list<-unique(train$Exterior1st)
for(Ext1_loop in Ext1_list){
  varname<-paste(Ext1_loop,"Ext1_detect")
  train<-train%>%mutate(!!varname:=Exterior1st==Ext1_loop)
}


train%>%ggplot(aes(y=Exterior1st,x=Exterior1st))+geom_point()
#Exterior 2nd is same as Exterior First, drop

train%>%ggplot(aes(y=LSP,x=MasVnrType))+geom_boxplot()
train$MasVnrType<-as.character(train$MasVnrType)
train[is.na(train$MasVnrType),"MasVnrType"]<-"NA"
MVT_list<-unique(train$MasVnrType)

for(MVT_loop in MVT_list){
  varname<-paste(MVT_loop,"MVT_detect")
  train<-train%>%mutate(!!varname:=MasVnrType==MVT_loop)
}

train%>%ggplot(aes(y=LSP,x=MasVnrArea))+geom_point()
#Not very indicative, can drop

train%>%ggplot(aes(y=LSP,x=Foundation))+geom_boxplot()
Fnd_list<-unique(train$Foundation)
for(Fnd_loop in Fnd_list){
  varname<-paste(Fnd_loop,"Fnd_detect")
  train<-train%>%mutate(!!varname:=Foundation==Fnd_loop)
}

train%>%ggplot(aes(y=LSP,x=Heating))+geom_boxplot()
Heat_list<-unique(train$Heat)
for(Heat_loop in Heat_list){
  varname<-paste(Heat_loop,"Heat_detect")
  train<-train%>%mutate(!!varname:=Heating==Heat_loop)
}

train%>%ggplot(aes(y=LSP,x=Electrical))+geom_boxplot()
train[is.na(train$Electrical),"Electrical"]<-"SBrkr"
Elct_list<-unique(train$Electrical)
for(Elct_loop in Elct_list){
  varname<-paste(Elct_loop,"Elct_detect")
  train<-train%>%mutate(!!varname:=Electrical==Elct_loop)
}

train%>%ggplot(aes(y=LSP,x=Fireplaces))+geom_point()
#Can Include

train%>%ggplot(aes(y=LSP,x=Functional))+geom_boxplot()
#Major Deduction2 are major differentiator  
train<-train%>%mutate(BFunctional=Functional=="Maj2")

train%>%ggplot(aes(y=LSP,x=FireplaceQu))+geom_boxplot()
train$FireplaceQu<-as.character(train$FireplaceQu)
train[is.na(train$FireplaceQu),"FireplaceQu"]<-"NA"
FplaceQ_list<-unique(train$FireplaceQu)
for(FplaceQ_loop in FplaceQ_list){
  varname<-paste(FplaceQ_loop,"FplaceQ_detect")
  train<-train%>%mutate(!!varname:=FireplaceQu==FplaceQ_loop)
}

train%>%ggplot(aes(y=LSP,x=GarageType))+geom_boxplot()
train$GarageType<-as.character(train$GarageType)
train[is.na(train$GarageType),"GarageType"]<-"NA"
GarageT_list<-unique(train$GarageType)

for(GarageT_loop in GarageT_list){
  varname<-paste(GarageT_loop,"GarageT_detect")
  train<-train%>%mutate(!!varname:=GarageType==GarageT_loop)
}

train%>%ggplot(aes(y=LSP,x=SaleType))+geom_boxplot()
SaleT_list<-unique(train$SaleType)
for(SaleT_loop in SaleT_list){
  varname<-paste(SaleT_loop,"SaleT_detect")
  train<-train%>%mutate(!!varname:=SaleType==SaleT_loop)
}

train%>%ggplot(aes(y=LSP,x=SaleCondition))+geom_boxplot()
SaleC_list<-unique(train$SaleCondition)
for(SaleC_loop in SaleC_list){
  varname<-paste(SaleC_loop,"SaleC_detect")
  train<-train%>%mutate(!!varname:=SaleCondition==SaleC_loop)
}

train%>%ggplot(aes(y=LSP,x=CentralAir))+geom_boxplot()
train<-train%>%mutate(BCentralAir=CentralAir=="Y")

train%>%ggplot(aes(y=LSP,x=MoSold))+geom_point()
#not that useful drop

###Ordinal Data
temp1<-sapply(as.character(train$ExterQual), switch, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
train$ExterQual<-temp1

temp1<-sapply(as.character(train$ExterCond), switch, "Po"=0, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
train$ExterCond<-temp1

#BsmtQual Mild Correlation
temp1<-sapply(as.character(train$BsmtQual), switch, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
train$BsmtQual<-temp1

train%>%ggplot(aes(y=LSP,x=GarageQual))+geom_boxplot()
train[is.na(train$GarageQual),"GarageQual"]<-"Po"
train%>%ggplot(aes(y=LSP,x=GarageQual))+geom_boxplot()
temp1<-sapply(as.character(train$GarageQual), switch, "Po"=0, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
train$GarageQual<-temp1

#BsmtCond poorly Correlated, can drop
temp1<-sapply(as.character(train$BsmtCond), switch, "Po"=0, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
train$BsmtCond<-temp1

temp1<-sapply(as.character(train$HeatingQC), switch, "Po"=0, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
train$HeatingQC<-temp1

#KitchenQual Mild Correlation
temp1<-sapply(as.character(train$KitchenQual), switch, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
train$KitchenQual<-temp1


#Drop the outlier
train%>%ggplot(aes(x=GrLivArea,y=SalePrice))+geom_point()
train<-train%>%filter(GrLivArea<4000)
train%>%ggplot(aes(x=GrLivArea,y=LSP))+geom_point()

View(train[1:10,c(4,5,18:21,27,35,37:39,44,45,47:55,57,62,63,82:216)])
train2<-train[,c(4,5,18:21,27,35,37:39,44,45,47:55,57,62,63,82:216)]
#Select Columns for Machine Learning

ind<-createDataPartition(train$SalePrice,times=1,p=0.75,list=FALSE)
train2_sub<-train2[ind,]
validation2<-train2[-ind,]
#Partition Data for Quick Testing

############################
##Below Code Repeats Imputation and Encoding For Test Data Set ##
test$MasVnrArea[is.na(test$MasVnrArea)]<-0
test%>%group_by(SaleType)%>%summarize(count=n())
test$SaleType[is.na(test$SaleType)]<-"WD"
test%>%group_by(Functional)%>%summarize(count=n())
test$Functional[is.na(test$Functional)]<-"Typ"
test%>%group_by(Exterior1st)%>%summarize(count=n())
test$Exterior1st[is.na(test$Exterior1st)]<-"VinylSd"
test%>%group_by(MSZoning)%>%summarize(count=n())
test$MSZoning[is.na(test$MSZoning)]<-"RL"
test$GarageArea[is.na(test$GarageArea)]<-0
test$GarageCars[is.na(test$GarageCars)]<-0
test[is.na(test$TotalBsmtSF),c("TotalBsmtSF","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF")]<-0
test[is.na(test$BsmtFullBath),c("BsmtFullBath","BsmtHalfBath")]<-0


for(MSZ in MSZ_list){
  varname<-paste(MSZ,"MSZ_detect")
  test<-test%>%mutate(!!varname:=MSZoning==MSZ)
}               

test_missing<-test[is.na(test$LotFrontage),]
test_missing_predict<-predict(fit_LotAreaVsFrontage,test_missing)
test[is.na(test$LotFrontage),"LotFrontage"]<-test_missing_predict


test<-test%>%mutate(BLotShape=(LotShape=="Reg"))
test<-test%>%mutate(BLotConfig=(LotConfig=="CulDSac"|LotConfig=="FR3"))
#Useful, turn into binary
test<-test%>%mutate(BStreet=str_detect(Street,"Grvl"))


#NA is actually no alley access
test[,"Alley"]<-as.character(test$Alley)
test[is.na(test$Alley),"Alley"]<-as.character("NA")

for(Alley_loop in Alley_list){
  varname<-paste(Alley_loop,"Alley_detect")
  test<-test%>%mutate(!!varname:=Alley==Alley_loop)
}  

for(LC_loop in LC_list){
  varname<-paste(LC_loop,"LC_detect")
  test<-test%>%mutate(!!varname:=LandContour==LC_loop)
} 



for(LS_loop in LS_list){
  varname<-paste(LS_loop,"LS_detect")
  test<-test%>%mutate(!!varname:=LandSlope==LS_loop)
} 

for(Cond1_loop in Cond1_list){
  varname<-paste(Cond1_loop,"Cond1_detect")
  test<-test%>%mutate(!!varname:=Condition1==Cond1_loop)
} 

for(Cond2_loop in Cond2_list){
  varname<-paste(Cond2_loop,"Cond2_detect")
  test<-test%>%mutate(!!varname:=Condition2==Cond2_loop)
} 

for(Nbor_loop in Nbor_list){
  varname<-paste(Nbor_loop,"Nbor_detect")
  test<-test%>%mutate(!!varname:=Condition2==Cond2_loop)
}


for(Bldg_loop in Bldg_list){
  varname<-paste(Bldg_loop,"Bldg_detect")
  test<-test%>%mutate(!!varname:=BldgType==Bldg_loop)
}

for(House_loop in House_list){
  varname<-paste(House_loop,"House_detect")
  test<-test%>%mutate(!!varname:=HouseStyle==House_loop)
}

for(Ext1_loop in Ext1_list){
  varname<-paste(Ext1_loop,"Ext1_detect")
  test<-test%>%mutate(!!varname:=Exterior1st==Ext1_loop)
}
test$MasVnrType<-as.character(test$MasVnrType)
test[is.na(test$MasVnrType),"MasVnrType"]<-"NA"
for(MVT_loop in MVT_list){
  varname<-paste(MVT_loop,"MVT_detect")
  test<-test%>%mutate(!!varname:=MasVnrType==MVT_loop)
}

for(Fnd_loop in Fnd_list){
  varname<-paste(Fnd_loop,"Fnd_detect")
  test<-test%>%mutate(!!varname:=Foundation==Fnd_loop)
}

for(Heat_loop in Heat_list){
  varname<-paste(Heat_loop,"Heat_detect")
  test<-test%>%mutate(!!varname:=Heating==Heat_loop)
}

test[is.na(test$Electrical),"Electrical"]<-"SBrkr"
for(Elct_loop in Elct_list){
  varname<-paste(Elct_loop,"Elct_detect")
  test<-test%>%mutate(!!varname:=Electrical==Elct_loop)
}

#Major Deduction2 are major differentiator  
test<-test%>%mutate(BFunctional=Functional=="Maj2")

test$FireplaceQu<-as.character(test$FireplaceQu)
test[is.na(test$FireplaceQu),"FireplaceQu"]<-"NA"
for(FplaceQ_loop in FplaceQ_list){
  varname<-paste(FplaceQ_loop,"FplaceQ_detect")
  test<-test%>%mutate(!!varname:=FireplaceQu==FplaceQ_loop)
}
test$GarageType<-as.character(test$GarageType)
test[is.na(test$GarageType),"GarageType"]<-"NA"
for(GarageT_loop in GarageT_list){
  varname<-paste(GarageT_loop,"GarageT_detect")
  test<-test%>%mutate(!!varname:=GarageType==GarageT_loop)
}

for(SaleT_loop in SaleT_list){
  varname<-paste(SaleT_loop,"SaleT_detect")
  test<-test%>%mutate(!!varname:=SaleType==SaleT_loop)
}

for(SaleC_loop in SaleC_list){
  varname<-paste(SaleC_loop,"SaleC_detect")
  test<-test%>%mutate(!!varname:=SaleCondition==SaleC_loop)
}

test<-test%>%mutate(BCentralAir=CentralAir=="Y")

temp1<-sapply(as.character(test$ExterQual), switch, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
test$ExterQual<-temp1

temp1<-sapply(as.character(test$ExterCond), switch, "Po"=0, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
test$ExterCond<-temp1


temp1<-sapply(as.character(test$BsmtQual), switch, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
test$BsmtQual<-temp1

test[is.na(test$GarageQual),"GarageQual"]<-"Po"

temp1<-sapply(as.character(test$GarageQual), switch, "Po"=0, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
test$GarageQual<-temp1


temp1<-sapply(as.character(test$BsmtCond), switch, "Po"=0, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
test$BsmtCond<-temp1

temp1<-sapply(as.character(test$HeatingQC), switch, "Po"=0, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
test$HeatingQC<-temp1


test$KitchenQual<-as.character(test$KitchenQual)
test$KitchenQual[is.na(test$KitchenQual)]<-"TA"
test$KitchenQual<-as.factor(test$KitchenQual)
temp1<-sapply(as.character(test$KitchenQual), switch, "Fa" = 1, "TA" = 2, "Gd" = 3, "Ex" = 4, 
              USE.NAMES = F)
test$KitchenQual<-temp1

View(test[1:10,c(4,5,18:21,27,35,37:39,44,45,47:55,57,62,63,81:214)])
test2<-test[c(4,5,18:21,27,35,37:39,44,45,47:55,57,62,63,81:214)]

##################################
##Data Analysis and Machine Leanring Below##

#Check Variable Importance
tree_fit<-rpart(LSP~.,data=train2_sub)
varImp(tree_fit)

#I tried several models, only final ones are given below

#Gradient Boosting Machine

#First Fit Model on training subset 
gbm_fit<-train(LSP~.,method="gbm",data=train2_sub)
gbm_predict<-predict(gbm_fit,validation2)
#gbm_train<-predict(gbm_all_fit2,train2_sub)
#The above dataset can be used to train "stacking"
#Stacking was not used since simple average always seems to beat stack(overfit?)

RMSE(gbm_predict,validation2$LSP)
#Performance is Acceptable

#Now Train on Full Data and Predict Test
gbm_full_fit<-train(LSP~.,method="gbm",data=train2)
gbm_full_predict<-predict(gbm_full_fit,test2)

#Linear Model
lm_fit<-train(LSP~.,method="lm",data=train2_sub)
lm_predict<-predict(lm_fit,validation2)
#lm_train<-predict(lm_fit,train2_sub)
RMSE(lm_predict,validation2$LSP)

lm_full_fit<-train(LSP~.,method="lm",data=train2)
lm_full_predict<-predict(lm_full_fit,test2)

#Random Forest
rf_fit<-train(LSP~.,method="rf",data=train2_sub)
rf_predict<-predict(rf_fit,validation2)
#rf_train<-predict(rf_fit,train2_sub)
RMSE(rf_predict,validation2$LSP)

rf_full_fit<-train(LSP~.,method="rf",data=train2)
rf_full_predict<-predict(rf_full_fit,test2)

#Center and Scale Numeric Data for Neural Network
train2_scale_temp<-scale(train2[,1:25],center=TRUE,scale=TRUE)
train2_scale<-train2
train2_scale[,1:25]<-train2_scale_temp

#Remove Columns with Near Zero Variance(for Neural Network)
nzv<-nearZeroVar(train2_scale,saveMetrics = FALSE)
train2_filt<-train2_scale[,-nzv]
train2_sub_filt<-train2_filt[ind,]
validation2_filt<-train2_filt[-ind,]

#Repeat for Test Data
test2_scale_temp<-scale(test2[,1:25],center=TRUE,scale=TRUE)
test2_scale<-test2
test2_scale[,1:25]<-test2_scale_temp
nm = intersect(names(train2_filt), names(test2));
test2_filt<-test2_scale[,nm]

#Bayesian Regularized Neural Network
brnn_fit<-train(LSP~.,method="brnn",data=train2_sub_filt)
brnn_predict<-predict(brnn_fit,validation2_filt)
#brnn_train<-predict(brnn_fit,train2_filt)
RMSE(brnn_predict,validation2$LSP)

brnn_full_fit<-train(LSP~.,method="brnn",data=train2_filt)
brnn_full_predict<-predict(brnn_full_fit,test2_filt)

#Calculate Average Prediction
avg_predict<-(gbm_predict+lm_predict+rf_predict+brnn_predict)/4
RMSE(avg_predict,validation2$LSP)

avg_full_predict<-(gbm_full_predict+lm_full_predict+rf_full_predict+brnn_full_predict)/4

output<-data.frame(Id=test$Id)%>%mutate(SalePrice=exp(avg_full_predict))
write.csv(output,"output_harvard.csv",row.names=FALSE)
save.image("HarvardX_Workspace_Final_GG.RData")
