library(caret)

#Normalizing data in training set and validation set
home_insurance_cleaned_training.norm.df <- home_insurance_cleaned_training.df
home_insurance_cleaned_validation.norm.df <- home_insurance_cleaned_validation.df

norm.values<-preProcess(home_insurance_cleaned_training.df[,-13], method = c("center","scale"))
home_insurance_cleaned_training.norm.df[,-13]<-predict(norm.values, home_insurance_cleaned_training.df[,-13])
home_insurance_cleaned_validation.norm.df[,-13]<-predict(norm.values, home_insurance_cleaned_validation.df[,-13])

#train model with training set



#home_insurance_cleaned_training.df
#wbcd.knn.pred <- knn(wbcd.norm.train[-1], test = wbcd.norm.valid[-1], cl=wbcd.norm.train[,1], k=7)
home_insurance.knn <- knnreg(home_insurance_cleaned_training.norm.df[,1:12], home_insurance_cleaned_training.norm.df[,13], k = 1 )

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)
#make prediction for validation set
home_insurance_pred.knn <- predict(home_insurance.knn,home_insurance_cleaned_validation.norm.df[,1:12])
#check accuracy
accuracy(home_insurance_cleaned_validation.norm.df[,13], home_insurance_pred.knn)
#check correlation
cor(home_insurance_pred.knn, home_insurance_cleaned_validation.norm.df[,13])


#Testing with k values from 1 to 70, to pick out the best value
library(class)
library(readxl)
install.packages("fastDummies")
library(fastDummies)
library(caret)
install.packages("FNN")
library(FNN)

#Evaluating different values of k
accuracy.df<-data.frame(k=seq(1,70,1),accuracy=rep(0,70))

#accuracy.df<-data.frame(k=seq(1,6,1),accuracy=rep(0,70), accura)

#Perparng list of k values
k_list <- list(5,10,15,20,25,30)

# compute knn for different k on validation.
for(i in k_list) {
  knn_option <- knnreg(home_insurance_cleaned_training.norm.df[,1:12], home_insurance_cleaned_training.norm.df[,13], k = i)
  #home_insurance_pred.knn <- predict(home_insurance.knn,home_insurance_cleaned_validation.norm.df[,1:12])
  
  knn_predict_option <- predict(knn_option,home_insurance_cleaned_validation.norm.df[,1:12])
  #accuracy.df[i, 2] <- confusionMatrix(knn_option, home_insurance_cleaned_validation.norm.df[, 13])$overall[1]
  accuracy_knn.df[i,2] <- accuracy(home_insurance_cleaned_validation.norm.df[,13], knn_predict_option)
  
}
accuracy.df
#######################Individual evaluation of k values###############

# k = 5


#home_insurance_cleaned_training.df
#wbcd.knn.pred <- knn(wbcd.norm.train[-1], test = wbcd.norm.valid[-1], cl=wbcd.norm.train[,1], k=7)
home_insurance.knn_5 <- knnreg(home_insurance_cleaned_training.norm.df[,1:12], home_insurance_cleaned_training.norm.df[,13], k = 5 )

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)
#make prediction for validation set
home_insurance_pred.knn_5 <- predict(home_insurance.knn_5,home_insurance_cleaned_validation.norm.df[,1:12])
#check accuracy
accuracy(home_insurance_cleaned_validation.norm.df[,13], home_insurance_pred.knn_5)
#check correlation
cor(home_insurance_pred.knn_5, home_insurance_cleaned_validation.norm.df[,13])


# k = 10


#home_insurance_cleaned_training.df
#wbcd.knn.pred <- knn(wbcd.norm.train[-1], test = wbcd.norm.valid[-1], cl=wbcd.norm.train[,1], k=7)
home_insurance.knn_10 <- knnreg(home_insurance_cleaned_training.norm.df[,1:12], home_insurance_cleaned_training.norm.df[,13], k = 10 )

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)
#make prediction for validation set
home_insurance_pred.knn_10 <- predict(home_insurance.knn_10,home_insurance_cleaned_validation.norm.df[,1:12])
#check accuracy
accuracy(home_insurance_cleaned_validation.norm.df[,13], home_insurance_pred.knn_10)
#check correlation
cor(home_insurance_pred.knn_10, home_insurance_cleaned_validation.norm.df[,13])





# k = 15


#home_insurance_cleaned_training.df
#wbcd.knn.pred <- knn(wbcd.norm.train[-1], test = wbcd.norm.valid[-1], cl=wbcd.norm.train[,1], k=7)
home_insurance.knn_15 <- knnreg(home_insurance_cleaned_training.norm.df[,1:12], home_insurance_cleaned_training.norm.df[,13], k = 15 )

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)
#make prediction for validation set
home_insurance_pred.knn_15 <- predict(home_insurance.knn_15,home_insurance_cleaned_validation.norm.df[,1:12])
#check accuracy
accuracy(home_insurance_cleaned_validation.norm.df[,13], home_insurance_pred.knn_15)
#check correlation
cor(home_insurance_pred.knn_15, home_insurance_cleaned_validation.norm.df[,13])

# k = 20

home_insurance.knn_20 <- knnreg(home_insurance_cleaned_training.norm.df[,1:12], home_insurance_cleaned_training.norm.df[,13], k = 20 )

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)

#concrete.knn <- knnreg(concrete.train[,1:8], concrete.train[,9], k = 11)
#make prediction for validation set
home_insurance_pred.knn_20 <- predict(home_insurance.knn_20,home_insurance_cleaned_validation.norm.df[,1:12])
#check accuracy
accuracy(home_insurance_cleaned_validation.norm.df[,13], home_insurance_pred.knn_20)
#check correlation
cor(home_insurance_pred.knn_20, home_insurance_cleaned_validation.norm.df[,13])


# k =25
home_insurance.knn_25 <- knnreg(home_insurance_cleaned_training.norm.df[,1:12], home_insurance_cleaned_training.norm.df[,13], k = 25 )
home_insurance_pred.knn_25 <- predict(home_insurance.knn_25,home_insurance_cleaned_validation.norm.df[,1:12])
#check accuracy
accuracy(home_insurance_cleaned_validation.norm.df[,13], home_insurance_pred.knn_25)
#correlation
cor(home_insurance_pred.knn_25, home_insurance_cleaned_validation.norm.df[,13])


# k = 30

home_insurance.knn_30 <- knnreg(home_insurance_cleaned_training.norm.df[,1:12], home_insurance_cleaned_training.norm.df[,13], k = 30 )
home_insurance_pred.knn_30 <- predict(home_insurance.knn_30,home_insurance_cleaned_validation.norm.df[,1:12])
#check accuracy
accuracy(home_insurance_cleaned_validation.norm.df[,13], home_insurance_pred.knn_30)
cor(home_insurance_pred.knn_30, home_insurance_cleaned_validation.norm.df[,13])



# k = 25 had the minimum RMSE value of 61.16, so that k value is selected

# lift chart 
library(gains)
gain_knn_25 <- gains(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS[!is.na(home_insurance_pred.knn_25)], home_insurance_pred.knn_25[!is.na(home_insurance_pred.knn_25)])
options(scipen=999)
premium_knn_25 <- home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS[!is.na(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS)]

par(pty="s")
plot(c(0,gain_knn_25$cume.pct.of.total*sum(premium_knn_25)/1000000)~c(0,gain_knn_25$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Annual Premium Gross", main = "Lift Chart", type = "l", col = "blue")
#baseline
lines(c(0,sum(premium_knn_25)/1000000)~c(0,dim(home_insurance_cleaned_validation.norm.df)[1]), col = "gray", lty = 2)
#do it yourself
plot(cumsum(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS[order(home_insurance_pred.knn_25, decreasing=TRUE)]/1000000), 
     xlab = "# cases", ylab = "Cumulative Annual Premium Gross", main = "Lift Chart", type = "l", col = "blue")
lines(c(0,sum(premium_knn_25)/1000000)~c(0,dim(home_insurance_cleaned_validation.norm.df)[1]), col = "gray", lty = 2)

#Cross Table:
#CrossTable(head(home_insurance_pred.knn), head(home_insurance_cleaned_validation.norm.df)$LAST_ANN_PREM_GROSS)

#Cross Table Corrected Version
CrossTable(x = head(home_insurance_cleaned_validation.norm.df)$LAST_ANN_PREM_GROSS, y= head(home_insurance_pred.knn_25), dnn = c("Actual","Predicted"))

#CrossTable(x = head(home_insurance_pred.knn), y= head(home_insurance_cleaned_validation.norm.df)$LAST_ANN_PREM_GROSS, prop.r=FALSE, prop.c=FALSE,
#          prop.t=FALSE, prop.chisq=FALSE, dnn = c("Predicted","Actual"))


