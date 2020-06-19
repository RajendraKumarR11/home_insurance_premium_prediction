#Random Forest

#normalize
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
#standardize
standardize <- function(x){
  return ((x-mean(x))/sd(x))
}

set.seed(1234)

library(randomForest)



#Creation of model
home_insurance_rf<- randomForest( home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS ~ ., data = home_insurance_cleaned_training.df, ntree = 10, mtry = 4, nodesize =5, importance = TRUE)


#home_insurance_rf_2 <- randomForest( home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS ~ SPEC_SUM_INSURED + UNSPEC_HRP_PREM + UNSPEC_HRP_PREM + (BEDROOMS * YEARBUILT) ., data = home_insurance_cleaned_training.df, ntree = 10, mtry = 4, nodesize =5, importance = TRUE)
#options(scipen = 999)


#Fitting into validation data
home_insurance_rf.pred <- predict( home_insurance_rf, home_insurance_cleaned_validation.df)
all.residuals_rf <- home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS - home_insurance_rf.pred
#length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals_rf, breaks = 25, xlab = "Residuals", main = "")

#Comparison between nPredicted, Actual and Residual
comparison_rf<-data.frame("Predicted" = home_insurance_rf.pred, "Actual" = home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS,
                          "Residual" = all.residuals_rf)
options(scipen=999, digits = 3)
head(comparison_rf)

install.packages('forecast')
library("forecast")
##Accuracy Measures
#Training Data
house_insurance_train.rf.pred <- predict(home_insurance_rf, home_insurance_cleaned_training.df)

accuracy(house_insurance_train.rf.pred, home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS)
num_rf<-sum(all.residuals_rf^2)
den_train_rf<-sum((home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS-mean(home_insurance_cleaned_training.df$LAST_ANN_PREM_GROSS))^2)
Predicted_traning_rf.Rsq<-1-(num_rf/den_train_rf) 
Predicted_traning_rf.Rsq
#Validation Data
accuracy(home_insurance_rf.pred, home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS)
num_rf<-sum(all.residuals_rf^2)
den_rf<-sum((home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS-mean(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS))^2)
Predicted_rf.Rsq<-1-(num_rf/den_rf) 
Predicted_rf.Rsq




# lift chart 
library(gains)
gain_rf <- gains(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS[!is.na(home_insurance_rf.pred)], home_insurance_rf.pred[!is.na(home_insurance_rf.pred)])
options(scipen=999)
premium <- home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS[!is.na(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS)]

par(pty="s")
plot(c(0,gain_rf$cume.pct.of.total*sum(premium)/1000000)~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Annual Premium Gross", main = "Lift Chart", type = "l", col = "blue")
#baseline
lines(c(0,sum(premium)/1000000)~c(0,dim(home_insurance_cleaned_validation.df)[1]), col = "gray", lty = 2)
#do it yourself
plot(cumsum(home_insurance_cleaned_validation.df$LAST_ANN_PREM_GROSS[order(home_insurance_rf.pred, decreasing=TRUE)]/1000000), 
     xlab = "# cases", ylab = "Cumulative Annual Premium Gross", main = "Lift Chart", type = "l", col = "blue")
lines(c(0,sum(premium)/1000000)~c(0,dim(home_insurance_cleaned_validation.df)[1]), col = "gray", lty = 2)



#Cross Table:
CrossTable(head(home_insurance_rf.pred), head(home_insurance_cleaned_validation.df)$LAST_ANN_PREM_GROSS)

#Correct Cross Table
CrossTable(x = head(home_insurance_cleaned_validation.df)$LAST_ANN_PREM_GROSS, y= head(home_insurance_rf.pred),dnn = c("Actual","Predicted"))






