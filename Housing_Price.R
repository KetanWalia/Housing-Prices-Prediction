### AUTHOR: KETAN WALIA

#LOADING THE PACKAGES

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(mice)
library(gridExtra)
library(e1071)
library(dplyr)
library(VIM)
library(Amelia)
library(scales)
library(corrplot)
library("PerformanceAnalytics")
library(caret)
library(MASS)
library(randomForest)
library(glmnet)


#LOADING THE DATA
train=read.csv("../input/train.csv")
dim(data)

#Checking the data
head(train)
str(train)
summary(train)

# Getting the names of continous variables & categorical Variables
numeric_var<- names(train)[which(sapply(train,is.numeric))]

cat_var <- names(train)[which(sapply(train, is.factor))]

# Checking for missing values 
colSums(sapply(train[,cat_var], is.na))

colSums(sapply(train[,numeric_var], is.na))


#The categorical variables with the largest number of missing values are: Alley, FirePlaceQu, PoolQC, Fence, and MiscFeature.
#There are 259 values for the LotFrontage, 8 missing values for MasVnrArea and 81 missing values for GarageYrBlt.

#Further looking at the distribution of missing values attribute wise using 'mice' & 'VIM' package

md.pattern(train)

aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, 
                  gap=3, ylab=c("Histogram of missing data","Pattern"))

# Generating missing value heatmap for complete look at missing values both row wise and column wise

missmap(train[,colSums(is.na(train)) > 0])

# Let's gain some insight on the number of houses that were remodeled

sum(train[,'YearRemodAdd'] != train[,'YearBuilt'])

cat('Percentage of houses remodeled',sum(train[,'YearRemodAdd'] != train[,'YearBuilt'])/ dim(train)[1])

train %>% select(YearBuilt, YearRemodAdd) %>% 
  mutate(Remodeled = as.integer(YearBuilt != YearRemodAdd)) %>%
  ggplot(aes(x= factor(x = Remodeled, labels = c( 'No','Yes')))) + geom_bar() + xlab('Remodeled')
+ theme_light()

# Looking at the distribution of response variable

library(scales)
ggplot(train, aes(x=SalePrice)) + geom_histogram(col = 'white') + theme_light() +scale_x_continuous(labels = comma)

# SPlitting the data into w.r.t continous variable and categorical variable
train_cat <- train[, cat_var]
train_cont <- train[,numeric_var]

# Writing functions to generate plots for exploratory analysis

#1) Function for plotting histograms
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- qplot(data=data,geom='histogram')
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

#2) Function for plotting graphs on grid
doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

#3) Function for plotting density plots
plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

# Bar plots for categorical variables
doPlots(train_cat, fun = plotHist, ii = 1:4, ncol = 2)

doPlots(train_cat, fun = plotHist, ii  = 4:8, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 8:12, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 13:18, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 18:22, ncol = 2)

#Note:
#The houses that have sever landslope are located in the Clear Creek and Timberland.
#The houses with moderate landslope are present in more neighborhood. 
#The Clear Creek and the Crawford neighborhoods seem to have high slopes.

train %>% select(LandSlope, Neighborhood, SalePrice) %>% filter(LandSlope == c('Sev', 'Mod'))%>% arrange(Neighborhood) %>% group_by(Neighborhood, LandSlope) %>% 
  summarize(Count = n()) %>% ggplot(aes(Neighborhood, Count)) 
+ geom_bar(aes(fill = LandSlope), position = 'dodge', stat = 'identity') +
  theme_light() +theme(axis.text.x = element_text(angle = 90, hjust =1))

# Looking at the average value of SalePrice by Landslope and Neighborhood
train %>% select(LandSlope, Neighborhood, SalePrice) %>% filter(LandSlope == c('Sev', 'Mod')) %>% 
  arrange(Neighborhood) %>% group_by(Neighborhood, LandSlope) %>% summarize(Count = mean(SalePrice)) %>% ggplot(aes(Neighborhood, Count)) + geom_bar(aes(fill = LandSlope), position = 'dodge', stat = 'identity') 
+ theme_light() +theme(axis.text.x = element_text(angle = 90, hjust =1))

#Plotting a boxplot between the neighboorhoods and sale price shows that BrookSide and South & West of Iowa State University have cheap houses
# Also we can observe some outliers especially for the neighbourhood Northridge and Northridge Heights
train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice))
+ geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')

# Density Plots for Numeric Variables

doPlots(train_cont, fun = plotDen, ii = 2:6, ncol = 2)

doPlots(train_cont, fun = plotDen, ii = 7:12, ncol = 2)

doPlots(train_cont, fun = plotDen, ii = 13:17, ncol = 2)

#The histograms below show that majority of the houses have 2 full baths, 0 half baths, and have an average of 3 bedrooms.
doPlots(train_cont, fun = plotHist, ii = 18:23, ncol = 2)

# Evaluating the correlation between variables
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = correlations, col=col,symm = TRUE)


################### DATA CLEANING ##################################################

# Loading the test data

testdata<-read.csv("../input/test.csv")

#Combining training & test data
data_raw<-rbind(train[,-ncol(train)],testdata)
dim(data_raw)

#### Missing Value Handling

# We see that for a number of variables there are missing values. However, we realize that for most of
# the variables the values are missing because the values are simply not applicable to those 
# variables. We can see a lot of NAs here, but most them could be because of NA being used in the dataset when a particular feature is not there in a given property. 
var_NAtoWithout<-c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")
## impute NA to without
without<-function(data,var){
  levels(data[,var]) <- c(levels(data[,var]), "without")
  data[,var][is.na(data[,var])] <- "without"
  return(data[,var])
}
data_combined<-data_raw
for (i in 1:length(var_NAtoWithout)){
  data_combined[,var_NAtoWithout[i]]<-without(data_raw,var_NAtoWithout[i]) 
}

#Compare the missing pattern before imputation
missmap(data_raw[-1], col=c('Red', 'Blue'), y.cex=0.2, x.cex=0.8,main = "Before Imputation")


#Imputing remaining missing values with "CART" using MICE package
data_imp<-mice(data_combined, m=1, method='cart', maxit=50, printFlag=FALSE)
data_imputed<-complete(data_imp, action = 1, include = FALSE)

# Plotting missing values after imputation
missmap(data_combined[-1], col=c('Red', 'Green'), y.cex=0.2, x.cex=0.8,main = "After Imputation")

####SPlitting the data back to train set and test set
final_train<-data_combined[1:nrow(train),]
final_train2<-cbind(final_train,SalePrice=train$SalePrice)
final_train2[[1]]<-NULL
final_train_use<-final_train2

final_test<-data_combined[c(1461:2919),]
final_test[[1]]<- NULL
final_test_use<-final_test



## We see that a lot varibles which are factor variables does have implicit ranking scheme to each level
## Leveraging this aspect we could convert these factor variables into numeric variables
final_train_use$Alley<- factor(final_train_use$Alley,levels=c("without","Grvl","Pave"), labels=c(0,1,2))

final_train_use$BsmtCond<- factor(final_train_use$BsmtCond,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_train_use$BsmtFinType1<- factor(final_train_use$BsmtFinType1,levels=c("without","Unf", "LwQ", "Rec", "BLQ","ALQ", "GLQ" ), labels=c(0,1,2,3,4,5,6))

final_train_use$BsmtFinType2<- factor(final_train_use$BsmtFinType2,levels=c("without","Unf", "LwQ", "Rec", "BLQ","ALQ", "GLQ" ), labels=c(0,1,2,3,4,5,6))

final_train_use$BsmtQual<- factor(final_train_use$BsmtQual,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_train_use$ExterCond<- factor(final_train_use$ExterCond,levels=c("Po", "Fa", "TA", "Gd", "Ex"), labels=c(1,2,3,4,5))

final_train_use$ExterQual<- factor(final_train_use$ExterQual,levels=c("Po", "Fa", "TA", "Gd", "Ex"), labels=c(1,2,3,4,5))

final_train_use$FireplaceQu<- factor(final_train_use$FireplaceQu,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_train_use$Functional<- factor(final_train_use$Functional,levels=c("Sal","Sev", "Maj2", "Maj1" , "Mod","Min2", "Min1", "Typ"), labels=c(1,2,3,4,5,6,7,8))

final_train_use$GarageCond<- factor(final_train_use$GarageCond,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_train_use$GarageQual<- factor(final_train_use$GarageQual,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_train_use$HeatingQC<- factor(final_train_use$HeatingQC,levels=c("Po", "Fa", "TA", "Gd", "Ex"), labels=c(1,2,3,4,5))

final_train_use$KitchenQual<- factor(final_train_use$KitchenQual,levels=c("Po", "Fa", "TA", "Gd", "Ex"), labels=c(1,2,3,4,5))

final_train_use$LandSlope<- factor(final_train_use$LandSlope,levels=c("Sev", "Mod", "Gtl"), labels=c(1,2,3))

final_train_use$LotShape<- factor(final_train_use$LotShape,levels=c("IR3", "IR2", "IR1","Reg"), labels=c(1,2,3,4))

final_train_use$PavedDrive<- factor(final_train_use$PavedDrive,levels=c("N", "P", "Y"), labels=c(0,1,2))

final_train_use$PoolQC<- factor(final_train_use$PoolQC,levels=c("without", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4))

final_train_use$Street<- factor(final_train_use$Street,levels=c("Grvl","Pave"), labels=c(1,2))

final_train_use$Utilities<- factor(final_train_use$Utilities,levels=c("ELO","NoSeWa","NoSewr","AllPub"), labels=c(1,2,3,4))

# Now converting the data type of above varibales from factor to numric
b<-c('Alley','BsmtCond','BsmtFinType1','BsmtFinType2','BsmtQual','ExterCond','ExterQual','FireplaceQu',
     'Functional','GarageCond','GarageQual','HeatingQC','KitchenQual','LandSlope','LotShape','PavedDrive',
     'PoolQC','Street','Utilities')

s<-which(names(final_train_use) %in% b==TRUE)
for (i in s){
  final_train_use[[i]]<-as.numeric(final_train_use[[i]])
}

sapply(final_train_use,class)

# Similarly logic is applied to the test data
final_test_use$Alley<- factor(final_test_use$Alley,levels=c("without","Grvl","Pave"), labels=c(0,1,2))

final_test_use$BsmtCond<- factor(final_test_use$BsmtCond,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_test_use$BsmtFinType1<- factor(final_test_use$BsmtFinType1,levels=c("without","Unf", "LwQ", "Rec", "BLQ","ALQ", "GLQ" ), labels=c(0,1,2,3,4,5,6))

final_test_use$BsmtFinType2<- factor(final_test_use$BsmtFinType2,levels=c("without","Unf", "LwQ", "Rec", "BLQ","ALQ", "GLQ" ), labels=c(0,1,2,3,4,5,6))

final_test_use$BsmtQual<- factor(final_test_use$BsmtQual,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_test_use$ExterCond<- factor(final_test_use$ExterCond,levels=c("Po", "Fa", "TA", "Gd", "Ex"), labels=c(1,2,3,4,5))

final_test_use$ExterQual<- factor(final_test_use$ExterQual,levels=c("Po", "Fa", "TA", "Gd", "Ex"), labels=c(1,2,3,4,5))

final_test_use$FireplaceQu<- factor(final_test_use$FireplaceQu,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_test_use$Functional<- factor(final_test_use$Functional,levels=c("Sal","Sev", "Maj2", "Maj1" , "Mod","Min2", "Min1", "Typ"), labels=c(1,2,3,4,5,6,7,8))

final_test_use$GarageCond<- factor(final_test_use$GarageCond,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_test_use$GarageQual<- factor(final_test_use$GarageQual,levels=c("without","Po", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4,5))

final_test_use$HeatingQC<- factor(final_test_use$HeatingQC,levels=c("Po", "Fa", "TA", "Gd", "Ex"), labels=c(1,2,3,4,5))

final_test_use$KitchenQual<- factor(final_test_use$KitchenQual,levels=c("Po", "Fa", "TA", "Gd", "Ex"), labels=c(1,2,3,4,5))

final_test_use$LandSlope<- factor(final_test_use$LandSlope,levels=c("Sev", "Mod", "Gtl"), labels=c(1,2,3))

final_test_use$LotShape<- factor(final_test_use$LotShape,levels=c("IR3", "IR2", "IR1","Reg"), labels=c(1,2,3,4))

final_test_use$PavedDrive<- factor(final_test_use$PavedDrive,levels=c("N", "P", "Y"), labels=c(0,1,2))

final_test_use$PoolQC<- factor(final_test_use$PoolQC,levels=c("without", "Fa", "TA", "Gd", "Ex"), labels=c(0,1,2,3,4))

final_test_use$Street<- factor(final_test_use$Street,levels=c("Grvl","Pave"), labels=c(1,2))

final_test_use$Utilities<- factor(final_test_use$Utilities,levels=c("ELO","NoSeWa","NoSewr","AllPub"), labels=c(1,2,3,4))

z<-which(names(final_test_use) %in% b==TRUE)
for (i in z){
  final_test_use[[i]]<-as.numeric(final_test_use[[i]])
}


## Converting factor variables to dummy variables using CARET package

final_train_use1<-final_train_use

train_Dummy <- dummyVars("~.",data=final_train_use1, fullRank=F)

final_train_use1 <- as.data.frame(predict(train_Dummy,final_train_use1))

dim(final_train_use1)


final_test_use1<-final_test_use

test_Dummy <- dummyVars("~.",data=final_test_use1, fullRank=F)

final_test_use1 <- as.data.frame(predict(test_Dummy,final_test_use1))


######################  Modelling ############################################

# 1) Building Multiple Linear Regression Model
reg1<- lm(SalePrice~., data = final_train_use1)

summary(reg1)

#R Square is not bad, but many variables do not pass the Hypothesis Testing, so the model is not perfect. 
#Potential overfitting will occur if someone insist on using it. Therefore, the variable selection 
#process should be involved in model construction.

# Evaluating the plots of MLR model

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(reg1)

# WE can see that the standardized residual distribution does tend to follow normal distribution but there are
# outliers which are influence points. These outliers have there effect on the distribution of errors
# not being exactly normal.
# Also we see there are certain points having high cooks distance. There is no apparent pattern evident 
# in the plot between Residuals & fitted values as such the violation of the homoskedasticity is not evident.
# Also the form of the model looks linear in nature which is also evident from the p-value pertaining to the 
# F-test

# Generating prediction for the test data
Prediction_test<- predict(reg1, newdata= final_test_use1)

## 2) STEP AIC Model

## As we saw in the MLR model a number of variables came out to be statistically insignificant i.e not significantly
# contributing towards explaining the variance of response variable. Feature selection is an appropriate way
# to build model using the variables which are significant

# We start with the full model as obtained by MLR in the previous step and then conduct stepwise AIC in 
#both directions
reg2<- stepAIC(reg1,direction="both")
summary(reg2)

#Note that the R2 values fr step AIC has  decreassed marginalli and so does the adj R2 value. However, 
# a large proprtion of the regressor variables sleted are significant which was not the case with MLR

Prediction_test<- predict(reg2, newdata= final_test_use1)

## 3) RANDOM FOREST

# Tuning the parameter of Random Forest. One of the most critical parameter is choosing the subset of variables
# to build forest of trees independently. This is done use cross validation with step factor chosen as 2 and
# improvement in error rate to be atleast 0.01.
#
bestmtry<-tuneRF(final_train_use[1:79],final_train_use[[80]],stepFactor=2,improve=0.01,trace=T,plot=T,doBest=T)
# The bestmtry obtained from tuning is 26
# Building Random Forest Model
modelRandom<-randomForest(SalePrice~.,data=final_train_use,mtry=26,ntree=500)

# % Var explained: 88.49

varImpPlot(modelRandom)

importance(modelRandom)

plot(modelRandom)

## Applying model to the test data
Prediction_rf <- predict(modelRandom, newdata= final_test_use)


#4) LASSO Regression

# Tuning the parameter for the model using cross validation and grid search
CV <- cv.glmnet(x=as.matrix(final_train_use1[1:233]),y=as.numeric(final_train_use1[[234]]),
                family="gaussian",type.measure="mse",
                alpha=1,nlambda=100)

plot(CV)

## Fitting the model based on parameters obtained from the previous step
fit <- glmnet(x=as.matrix(final_train_use1[1:233]),y=as.numeric(final_train_use1[[234]]),
              family="gaussian",
              alpha=1,lambda=CV$lambda.1se)

fit$beta[,1]
# We see that beta coeff. for a large number of variables is set to zero. The deviance explained is 0.79 with 
# degree of freedom = 18
tpred<- predict(fit,as.matrix(final_test_use1))