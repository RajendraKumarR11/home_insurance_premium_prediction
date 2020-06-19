# Dividing the data into training and validation data.

home_insurance_cleaned.df <- read.csv("Home_Insurance_Cleaned6.csv")
set.seed(1)

ncol(home_insurance_cleaned.df)
nrow(home_insurance_cleaned.df)

# Getting the row number of 60% of data to fix for training data
index = floor(0.60 * nrow(home_insurance_cleaned.df))

# Fixing the training index
train.index <- sample(c(1:nrow(home_insurance_cleaned.df)), index) 

# Splitting into training data and validation data
home_insurance_cleaned_training.df <- home_insurance_cleaned.df[train.index,]
home_insurance_cleaned_validation.df <- home_insurance_cleaned.df[-train.index,]

#Create a scatterplot matrix of "Training Data" and select an initial set of predictor variables
library(car)
#Evaluate behavior between variables with scatterplotMatrix
scatterplotMatrix(home_insurance_cleaned_training.df, ellipse = FALSE,col=c("black"),
                  regLine = list(method=lm, lty=1, lwd=2, col="red"),
                  smooth=list(smoother=loessLine, spread=FALSE, lty.smooth=2, lwd.smooth=1, col.smooth="black"), 
                  main="Scatter Plot Matrix")


#Regression Models
#######################################################1. Model 1: Using all predictor variables################################3333
# use lm() to run a linear regression of Price on all 13 predictors

#Identifying correlation values

cor(home_insurance_cleaned_training.df)

model1 <- lm(LAST_ANN_PREM_GROSS ~ ., data = home_insurance_cleaned_training.df)
options(scipen = 999)
summary(model1)

####################################Not taken Begin#################
#Regression diagnosis Typical Approach
#Typical approach
par(mfrow=c(2,2))
plot(model1)
#-----a

par(mfrow=c(1,1))
#----------b
residplot(model1)
#-----------c
residualPlots(model1)


#Enhanced approach
library(car)
par(mfrow=c(1,1))
qqPlot(model1, id.method="identify", simulate=TRUE, main = "Q-Q Plot")
residplot<-function(fit, nbreaks=10){ 
  z<-rstudent(model)
  hist(z, breaks = nbreaks, freq = FALSE, 
       xlab = "Studentized Residual",
       main="Distribution of Errors") 
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean = mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y, 
        col="red", lwd=2, lty=2)
  legend("topright", 
         legend = c("Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue", "red"),cex=.7)
}
residplot(model1)
durbinWatsonTest(model1)
crPlots(model1)
ncvTest(model1)
spreadLevelPlot(model1)
library(gvlma)
summary(gvlma(model1))
sqrt(vif(model1))>2
##################################Not taken end######################


#Model 1 - Typical Approach
par(mfrow = c(2,2))
plot(model1)




###############################Model 2################################
##Considering addition of core Financial terms like SPEC_SUM_INSURED, SPEC_ITEM_PREM and UNSPEC_HRP_PREM and product of building specifications like BEDROOMS and YEARBUILT 
model2 <- lm(LAST_ANN_PREM_GROSS ~ SPEC_SUM_INSURED + UNSPEC_HRP_PREM + UNSPEC_HRP_PREM + (BEDROOMS * YEARBUILT), data = home_insurance_cleaned_training.df)
options(scipen = 999)
summary(model2)



########################Model 3#############################
##Considering only the attributes of the occuppant like OCC_STATUS_NUM, P1_SEX_NUM, P1_MAR_STATUS_NUM, AGE along with product of BEDROOMS and YEARBUILT  
model3 <- lm(LAST_ANN_PREM_GROSS ~ OCC_STATUS_NUM + P1_SEX_NUM + P1_MAR_STATUS_NUM + AGE + (BEDROOMS * YEARBUILT), data = home_insurance_cleaned_training.df)
options(scipen = 999)
summary(model3)



##########################Model 4###############################
##mix and match of buiding specs(SPEC_SUM_INSURED, UNSPEC_HRP_PREM) and occupant specs(OCC_STATUS_NUM,AGE ) along with product of BEDROOMS and YEARBUILT
model4 <- lm(LAST_ANN_PREM_GROSS ~ SPEC_SUM_INSURED + UNSPEC_HRP_PREM + OCC_STATUS_NUM +  AGE + (BEDROOMS * YEARBUILT), data = home_insurance_cleaned_training.df)
options(scipen = 999)
summary(model4)




############################Different steps for dimension reduction begin########
library(MASS)
#car.lm.step <- step(car.lm, direction = "backward")

#fit <- lm(LAST_ANN_PREM_GROSS~SPEC_SUM_INSURED,data=home_insurance_cleaned_training.df)
#step1 <- stepAIC(fit, direction="forward")
#summary(step1)
step.model <- step(model1, direction = "backward", 
                      trace = FALSE)
step.model2 <- stepAIC(model2, direction = "both", 
                      trace = FALSE)
summary(step.model)


#####################Different steps for dimension reduction end##########################

##########################Model 4###############################
##mix and match of buiding specs(SPEC_SUM_INSURED, UNSPEC_HRP_PREM) and occupant specs(OCC_STATUS_NUM,AGE ) along with product of BEDROOMS and YEARBUILT
model4 <- lm(LAST_ANN_PREM_GROSS ~ SPEC_SUM_INSURED + UNSPEC_HRP_PREM + OCC_STATUS_NUM +  AGE + (BEDROOMS * YEARBUILT), data = home_insurance_cleaned_training.df)
options(scipen = 999)
summary(model4)


##############Model 5 Quadratic ##########################
#Model 5
#Introducing  Coarse.Aggregate, Fine.Aggregate and establishing in quadratic form with 2 building specs and 2 content specs
model5 <- lm(LAST_ANN_PREM_GROSS ~ I(SPEC_SUM_INSURED^2)+AGE+I(UNSPEC_HRP_PREM^2)+
              +OCC_STATUS_NUM, data = home_insurance_cleaned_training.df)
summary(model5)




#############################Model 6 Polynomial regression
#involving ##############
#Model 6
model6 <- lm(LAST_ANN_PREM_GROSS~ poly(SPEC_SUM_INSURED,2)+ poly(UNSPEC_HRP_PREM,2) + poly(BEDROOMS,2) + AGE ,data = home_insurance_cleaned_training.df)
summary(model6)




####################3forward and Backward for all models
stepf.model1 <- step(model1, direction = "forward", 
                    trace = FALSE)
summary(stepf.model1 )
stepb.model1 <- step(model1, direction = "backward", 
                    trace = FALSE)
summary(stepb.model1)

stepf.model2 <- step(model2, direction = "forward", 
                    trace = FALSE)
summary(stepf.model2 )
stepb.model2 <- step(model2, direction = "backward", 
                    trace = FALSE)
summary(stepb.model2)


stepf.model3 <- step(model3, direction = "forward", 
                    trace = FALSE)
summary(stepf.model3)
stepb.model3 <- step(model3, direction = "backward", 
                    trace = FALSE)
summary(stepb.model3)


stepf.model4 <- step(model4, direction = "forward", 
                    trace = FALSE)
summary(stepf.model4)
stepb.model4 <- step(model4, direction = "backward", 
                    trace = FALSE)
summary(stepb.model4)

stepf.model5 <- step(model5, direction = "forward", 
                     trace = FALSE)
summary(stepf.model5)

stepb.model5 <- step(model5, direction = "backward", 
                    trace = FALSE)
summary(stepb.model5)


stepf.model6 <- step(model6, direction = "forward", 
                    trace = FALSE)
summary(stepf.model6)
stepb.model6 <- step(model6, direction = "backward", 
                    trace = FALSE)
summary(stepb.model6)



####Typical Approach model 1########################
#Model 1
par(mfrow = c(2,2))
plot(model1)

#Model 2
par(mfrow = c(2,2))
plot(model2)

#Model 3
par(mfrow = c(2,2))
plot(model3)

#Model 4
par(mfrow = c(2,2))
plot(model4)

#Model 5
par(mfrow = c(2,2))
plot(model5)

#Model 6
par(mfrow = c(2,2))
plot(model6)



########################Enhanced Approach################33
#######Normality Test######################33
library("ggplot2")
library("forecast")
library("car")
install.packages("carData")
#Model 1
qqPlot(model1,labels = row.names(home_insurance_cleaned_training.df),id.method ="identity",simulate =TRUE,main ="Q-Q Plot of Model 1")

#Model 2
qqPlot(model2,labels = row.names(home_insurance_cleaned_training.df),id.method ="identity",simulate =TRUE,main ="Q-Q Plot of Model 2")

#Model 3
qqPlot(model3,labels = row.names(home_insurance_cleaned_training.df),id.method ="identity",simulate =TRUE,main ="Q-Q Plot of Model 3")

#Model 4
qqPlot(model4,labels = row.names(home_insurance_cleaned_training.df),id.method ="identity",simulate =TRUE,main ="Q-Q Plot of Model 4")

#Model 5
qqPlot(model5,labels = row.names(home_insurance_cleaned_training.df),id.method ="identity",simulate =TRUE,main ="Q-Q Plot of Model 5")

#Model 6
qqPlot(model1,labels = row.names(home_insurance_cleaned_training.df),id.method ="identity",simulate =TRUE,main ="Q-Q Plot of Model 6")





#Residual Plot - Distribution of error
residplot <-function(fit,nbreak =10) {
  z <- rstudent(fit)
  hist(z,breaks =nbreak,freq =FALSE,
       main ="Distribution of Errors")
  rug(jitter(z),col="brown")
  curve(dnorm(x,mean = mean(z),sd = sd(z)),
        add =TRUE,col ="blue",lwd =2)
  lines(density(z)$x, density(z)$y, col ="red",lwd=2,lty=2)
}
residplot(model1)
residplot(model2)

residplot(model3)

residplot(model4)

residplot(model5)

residplot(model6)


# Independence of error
durbinWatsonTest(model1)
durbinWatsonTest(model2)
durbinWatsonTest(model3)
durbinWatsonTest(model4)
durbinWatsonTest(model5)
durbinWatsonTest(model6)



#Linearity Test:
crPlots(model1)
crPlots(model2)
crPlots(model3)
crPlots(model4)
crPlots(model5)
crPlots(model6)

#Homoscedasticity:
ncvTest(model1)
ncvTest(model2)
ncvTest(model3)
ncvTest(model4)
ncvTest(model5)
ncvTest(model6)


#Homoscedasticity - Spread level plot:

spreadLevelPlot(model1)
spreadLevelPlot(model2)
spreadLevelPlot(model3)
spreadLevelPlot(model4)
spreadLevelPlot(model5)
spreadLevelPlot(model6)

#Global validation and linear model assumption:
#  Code:
  install.packages("gvlma")
library(gvlma)
summary(gvlma(model1))
summary(gvlma(model2))
summary(gvlma(model3))
summary(gvlma(model4))
summary(gvlma(model5))
summary(gvlma(model6))


#Multicollinearlity:
vif(model1)
vif(model2)
vif(model3)
vif(model4)
vif(model5)
vif(model6)



#Identify unusual observations

#Outlier test executed on model 4
#Code:
 outlierTest(model4)
influencePlot(model4, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

#Select the best regression model 

AIC(model1, model2)
AIC(model1, model3)
AIC(model1, model4)
AIC(model1, model5)
AIC(model1, model6)
AIC(model2, model3)
AIC(model2, model4)
AIC(model2, model5)
AIC(model2, model6)
AIC(model3, model4)
AIC(model3, model5)
AIC(model3, model6)
AIC(model4, model5)
AIC(model4, model6)
AIC(model5, model6)

#AIC tests
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model4)
AIC(model5)
AIC(model6)


#ANOVA tests
ANOVA(model1)

#Identify unusual observations

#Outlier test executed on model 4
outlierTest(model1)
influencePlot(model1, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")


####################################Not considered Begin#################################
library(forecast)
fit.pred <- predict(model1, home_insurance_cleaned_training.df)
options(scipen=999, digits = 2)
residuals <- home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS - fit.pred
p<-data.frame("Predicted" = fit.pred, "Actual" = home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS,
              "Residual" = residuals)
options(scipen=999, digits = 3)
p
# use accuracy() to compute common accuracy measures.

#Training Data
accuracy(fit.pred, home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS)
#Validation Data
accuracy(fit.pred, home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS)
num<-sum(residuals^2)
den<-sum((home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS-mean(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS))^2)
Predicted.Rsq<-1-(num/den) 
Predicted.Rsq
####################################Not considered End#################################


#Fitting the model to validation data
library(forecast)

install.packages("'descr")
install.packages("corrplot")
library("corrplot")
library("gmodels")
options(max.print=1000000)

house_insurance.lm.pred <- predict(model1, home_insurance_cleaned_validation.df)
all.residuals <- home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS - house_insurance.lm.pred
#length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

comparison<-data.frame("Predicted" = house_insurance.lm.pred, "Actual" = home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS,
              "Residual" = all.residuals)
options(scipen=999, digits = 3)
head(comparison)



##Accuracy Measures
#Training Data
house_insurance_train.lm.pred <- predict(model1, home_insurance_cleaned_training.df)

accuracy(house_insurance_train.lm.pred, home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS)
num<-sum(all.residuals^2)
den_training<-sum((home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS-mean(home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS))^2)
Predicted_training.Rsq<-1-(num/den_training) 
Predicted_training.Rsq
#Validation Data
accuracy(house_insurance.lm.pred, home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS)
num<-sum(all.residuals^2)
den<-sum((home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS-mean(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS))^2)
Predicted.Rsq<-1-(num/den) 
Predicted.Rsq

# lift chart 
library(gains)
gain <- gains(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS[!is.na(house_insurance.lm.pred)], house_insurance.lm.pred[!is.na(house_insurance.lm.pred)])
options(scipen=999)
premium <- home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS[!is.na(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS)]

par(pty="s")
plot(c(0,gain$cume.pct.of.total*sum(premium)/1000000)~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Annual Premium Gross", main = "Lift Chart", type = "l", col = "blue")
#baseline
lines(c(0,sum(premium)/1000000)~c(0,dim(home_insurance_cleaned_validation.df)[1]), col = "gray", lty = 2)
#do it yourself
plot(cumsum(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS[order(house_insurance.lm.pred, decreasing=TRUE)]/1000000), 
     xlab = "# cases", ylab = "Cumulative Annual Premium Gross", main = "Lift Chart", type = "l", col = "blue")
lines(c(0,sum(price)/1000000)~c(0,dim(home_insurance_cleaned_validation.df)[1]), col = "gray", lty = 2)


#####ROC Curve########################
# Calculate sensitivity and false positive measures for logit model


fity_ypos <- model1$fitted[y == 1]
fity_yneg <- model1$fitted[y == 0]

sort_fity <- sort(model1$fitted.values)

sens <- 0
spec_c <- 0

for (i in length(sort_fity):1){
  sens <- c(sens, mean(fity_ypos >= sort_fity[i]))
  spec_c <- c(spec_c, mean(fity_yneg >= sort_fity[i]))
  
} 

# Calculate sensitivity and false positive measure for random forest model

#fity_ypos2 <- as.numeric(rf$pred[y == 1]) - 1
#fity_yneg2 <- as.numeric(rf$pred[y == 0]) - 1


#sort_fity2 <- as.numeric(sort(rf$pred)) - 1

#sens2 <- 0
#spec_c2 <- 0

#for (i in length(sort_fity2):1){
#  sens2 <- (c(sens2, mean(fity_ypos2 >= sort_fity2[i])))
#  spec_c2 <- (c(spec_c2, mean(fity_yneg2 >= sort_fity2[i])))
#} 

# plot ROC curves

plot(spec_c, sens, type = "l", 
     xlab = "false positive rate", ylab = "true positive rate", col = 'blue')
abline(col= "black")
lines(spec_c, sens, col='green')
legend("topleft", legend = c("Mutiple Linear Regression") , pch = 15, bty = 'n', col = c("blue"))

install.packages("caret")
install.packages("lattice")
install.packages("dplyr")
install.packages("regclass")
library("regclass")
library("lattice")
library("dplyr")
library("caret")
confusion_matrix(house_insurance.lm.pred)
CrossTable(x=head(home_insurance_cleaned_validation.df)$LAST_ANN_PREM_GROSS, y= head(house_insurance.lm.pred))
CrossTable(head(house_insurance.lm.pred), head(home_insurance_cleaned_validation.df)$LAST_ANN_PREM_GROSS)

#Correct Cross Table
CrossTable(x = head(home_insurance_cleaned_validation.df)$LAST_ANN_PREM_GROSS, y= head(house_insurance.lm.pred),dnn = c("Actual","Predicted"))
