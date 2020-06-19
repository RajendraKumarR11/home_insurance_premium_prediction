install.packages("psych")
library(psych)
library(eeptools)


home_insurance_cleaned.df <- read.csv("Home_Insurance_Cleaned3.csv")

fa.parallel(home_insurance_cleaned.df,fa="pc",n.iter=100, show.legend = FALSE, main = "Screen plot with 
            parallel analysis (Principal Component Analysis)")


str(home_insurance_cleaned.df)
home_insurance_cleaned_test.df <- home_insurance_cleaned.df[rowSums(is.na(home_insurance_cleaned.df)) == 0,]
#rowSums(is.na(home_insurance_cleaned.df))
        

nrow(home_insurance_cleaned_test.df)
ncol(home_insurance_cleaned_test.df)

str(home_insurance_cleaned_test.df)

pc <- principal(home_insurance_cleaned_test.df, nfactors = 1)



IQR(home_insurance_cleaned_test.df$FLOODING_NUM)

album2 <- album2[, -c(5:7)]

fa.parallel(home_insurance_cleaned_test.df[, -c(4:7)],fa="pc",n.iter=100, show.legend = FALSE, main = "Screen plot with 
            parallel analysis (Principal Component Analysis)")

# 4 components appear above the cutoff line

home_insurance_cleaned.df <- home_insurance_cleaned.df[, -c(4:7)]

home_insurance_cleaned.df_pc <- principal(home_insurance_cleaned.df, nfactors = 2)
home_insurance_cleaned.df_pc
nrow(home_insurance_cleaned.df)
ncol(home_insurance_cleaned.df)

stat(home_insurance_cleaned.df)
home_insurance_standardized.df <- scale(home_insurance_cleaned.df)

home_insurance_standardized.df




###########################Code to be considered###################3

# Dividing the data into training and validation data.

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

# 4 predictors have already been removed theough multicollinearity

# Checking for variance between 'SPEC_ITEM_PREM' and 'SPEC_ITEM_PREM'

# compute PCs on two dimensions
install.packages("ggfortify")
library(ggfortify)
home_insurance_cleaned_training_standardized.df <- scale(home_insurance_cleaned_training.df)
home_insurance_pcs <- prcomp(data.frame(home_insurance_cleaned_training_standardized.df[,'SPEC_SUM_INSURED'], home_insurance_cleaned_training_standardized.df[,'SPEC_ITEM_PREM']))
summary(home_insurance_pcs)
home_insurance_pcs$rot
scores <- home_insurance_pcs$x
head(scores, 5)
autoplot(home_insurance_pcs)

home_insurance_pc <- principal(home_insurance_cleaned_training_standardized.df[,4:5], nfactors = 2)
home_insurance_pc
