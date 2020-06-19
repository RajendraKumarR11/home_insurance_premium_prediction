#
install.packages("neuralnet")
library(neuralnet)
#
n = names(home_insurance_cleaned_training.norm.df)
f<-as.formula(paste("LAST_ANN_PREM_GROSS~", paste(n[!n %in% "LAST_ANN_PREM_GROSS"], collapse = "+")))
#maxs    =   apply(home_insurance_cleaned.df,    MARGIN  =   2,  max)
#mins    =   apply(home_insurance_cleaned.df,    MARGIN  =   2,  min)
#home_insurance_cleaned_scaledData =     as.data.frame(scale(home_insurance_cleaned.df, center  =   mins,   scale   =   maxs    - mins))

#
home_insurance.ann <- neuralnet(f, data = home_insurance_cleaned_training.norm.df, hidden = c(10,10))
#home_insurance.ann.scaled   <- home_insurance.ann$net.result *(max(home_insurance_cleaned_scaledData$LAST_ANN_PREM_GROSS)-min(home_insurance_cleaned_scaledData$LAST_ANN_PREM_GROSS))+min(home_insurance_cleaned_scaledData$LAST_ANN_PREM_GROSS)

#
home_insurance.ann.pred <- compute(home_insurance.ann, home_insurance_cleaned_validation.norm.df[-13])
#
home_insurance.ann.pred.class <- ifelse(unlist(home_insurance.ann.pred$net.result) < 0.5, 0,1)

CrossTable(x = head(home_insurance_cleaned_training.norm.df)$LAST_ANN_PREM_GROSS, y= head(home_insurance.ann.pred.class), dnn = c("Actual","Predicted"))
#
#CrossTable(x = wbcd.norm.valid$diagnosis, y= wbcd.ann.pred.class, prop.r=FA

#CrossTable(x = head(home_insurance_cleaned_validation.norm.df)$LAST_ANN_PREM_GROSS, y= head(home_insurance_pred.knn_25), dnn = c("Actual","Predicted"))


# display weights
home_insurance.ann$weights

# display predictions
prediction(home_insurance.ann)

# plot network
plot(home_insurance.ann, rep="best")

home_insurance_ann_training=sample(row.names(home_insurance_cleaned.df), dim(home_insurance_cleaned.df)[1]*0.6)
home_insurance_ann_validation=setdiff(row.names(home_insurance_cleaned.df), training)

#Applying prediction to training data
home_insurance_training.prediction <- compute(home_insurance.ann, home_insurance_cleaned_training.norm.df[,-c(13)])
training.class <- apply(home_insurance_training.prediction$net.result,1,which.max)-1
confusionMatrix(training.class, home_insurance_cleaned.df[home_insurance_ann_training,]$LAST_ANN_PREM_GROSS)

#Applying prediction to validation data
home_insurance_validation.prediction <- compute(home_insurance.ann, home_insurance_cleaned_validation.norm.df[,-c(13)])
validation.class <-apply(home_insurance_validation.prediction$net.result,1,which.max)-1
confusionMatrix(validation.class, home_insurance_cleaned.df[home_insurance_ann_validation,]$LAST_ANN_PREM_GROSS)


#Accuracy
accuracy(home_insurance_cleaned_validation.norm.df[,13], home_insurance_validation.prediction)

#Another accuracy measurement(Not applied)

#real.values <- (home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS)*(max(home_insurance_cleaned.df$LAST_ANN_PREM_GROSS)-min(home_insurance_cleaned.df$LAST_ANN_PREM_GROSS))+min(home_insurance_cleaned.df$LAST_ANN_PREM_GROSS)
#MSE.neuralnetModel  <- sum((real.values - pred_neuralnet.scaled)^2)/nrow(testing_Data)
#MSE.neuralnetModel
#plot(real.values, pred_neuralnet.scaled, col='red',main='Real   vs  predicted',pch=18,cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend='NN',pch=18,col='red',  bty='n')

###############################Don't Consider Begin##########################################3
# run nn with 2 hidden nodes 
# use hidden= with a vector of integers specifying number of hidden nodes in each layer
nn <- neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~ 
                  ALCHL_I + PROFIL_I_R + VEH_INVL + SUR_COND_1 + SUR_COND_2 
                + SUR_COND_3 + SUR_COND_4, data = trainData, hidden = 2)

training.prediction <- compute(nn, trainData[,-c(8:11)])
training.class <- apply(training.prediction$net.result,1,which.max)-1
confusionMatrix(training.class, accidents.df[training,]$MAX_SEV_IR)

validation.prediction <- compute(nn, validData[,-c(8:11)])
validation.class <-apply(validation.prediction$net.result,1,which.max)-1
confusionMatrix(validation.class, accidents.df[validation,]$MAX_SEV_IR)
##############################Don't Consider End################################################

#predict_testNN = compute(NN, testNN[,c(1:5)]
home_insurance.ann_RMSE = (sum((as.numeric(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS) - home_insurance_validation.prediction)^2) / nrow(home_insurance_cleaned_validation.norm.df)) ^ 0.5

#RMSE Evaluation:
home_insurance.ann_RMSE = (sum((home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS - 219.4778)^2) / nrow(home_insurance_cleaned_validation.norm.df)) ^ 0.5
home_insurance.ann_RMSE

RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5


home_insurance_validation.prediction
str(home_insurance_validation.prediction)
str(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS)

head(home_insurance_validation.prediction)


home_insurance_validation.prediction_norm = (home_insurance_validation.prediction$net.result * (max(home_insurance_cleaned.df$LAST_ANN_PREM_GROSS) - min(home_insurance_cleaned.df$rating))) + min(home_insurance_cleaned.df$rating)

#Another plot:
plot(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS, home_insurance_validation.prediction, col='blue', pch=16, ylab = "Predicted Value", xlab = "Actual Value")

#Lift Chart

library(gains)
gain_ann <- gains(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS[!is.na(home_insurance.ann)], home_insurance.ann[!is.na(home_insurance.ann)])
options(scipen=999)
premium_ann <- home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS[!is.na(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS)]

par(pty="s")
plot(c(0,gain_ann$cume.pct.of.total*sum(premium_ann)/1000000)~c(0,gain_ann$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Annual Premium Gross", main = "Lift Chart", type = "l", col = "blue")
#baseline
lines(c(0,sum(premium_ann)/1000000)~c(0,dim(home_insurance_cleaned_validation.norm.df)[1]), col = "gray", lty = 2)
#do it yourself
plot(cumsum(home_insurance_cleaned_validation.norm.df$LAST_ANN_PREM_GROSS[order(home_insurance.ann, decreasing=TRUE)]/1000000), 
     xlab = "# cases", ylab = "Cumulative Annual Premium Gross", main = "Lift Chart", type = "l", col = "blue")
lines(c(0,sum(premium_knn_25)/1000000)~c(0,dim(home_insurance_cleaned_validation.norm.df)[1]), col = "gray", lty = 2)