#Reading data

home_insurance.df <- read.csv("home_insurance.csv")
home_insurance.df
str(home_insurance.df)

install.packages("ggplot2")
library("ggplot2")
install.packages("eeptools")
library(eeptools)
library(lubridate)
install.packages("dplyr")
library("dplyr")

#home_insurance_null_checked.df <- home_insurance.df[!complete.cases(home_insurance.df),]
nrow(home_insurance.df)

#Find the count of each datatype in the dataset
table(sapply(home_insurance.df, class))

home_insurance.df_filtered <- home_insurance.df[ which( home_insurance.df$POL_STATUS == "Live" | home_insurance.df$POL_STATUS == "Lapsed") , ]
home_insurance.df_filtered = subset(home_insurance.df_filtered, select = -c(Police) )
home_insurance.df_filtered = subset(home_insurance.df_filtered, select = -c(QUOTE_DATE) )
home_insurance.df_filtered$AGE <- floor(age_calc(as.Date(dmy(home_insurance.df_filtered$P1_DOB)), units = "years"))
home_insurance.df_filtered$AGE <- floor(age_calc(as.Date(dmy(home_insurance.df_filtered$P1_DOB)), units = "years"))






# converting 'Flooding' into numerical variable and storing as 'FLOODING_NUM'
home_insurance.df_filtered$FLOODING_NUM <- as.integer(home_insurance.df_filtered$FLOODING)


# converting 'BUILDINGS_COVER' into numerical variable and storing as 'BUILDINGS_COVER_NUM'
home_insurance.df_filtered$BUILDINGS_COVER_NUM <- as.integer(home_insurance.df_filtered$BUILDINGS_COVER)


# converting 'CONTENTS_COVER' into numerical variable and storing as 'CONTENTS_COVER_NUM'
home_insurance.df_filtered$CONTENTS_COVER_NUM <- as.integer(home_insurance.df_filtered$CONTENTS_COVER)



# converting 'P1_MAR_STATUS' into numerical variable and storing as 'P1_MAR_STATUS_NUM'
home_insurance.df_filtered$P1_MAR_STATUS_NUM <- as.integer(home_insurance.df_filtered$P1_MAR_STATUS)

# converting 'P1_SEX' into numerical variable and storing as 'P1_SEX_NUM'
home_insurance.df_filtered$P1_SEX_NUM <- as.integer(home_insurance.df_filtered$P1_SEX)

# converting 'OCC_STATUS' into numerical variable and storing as 'OCC_STATUS_NUM'
home_insurance.df_filtered$OCC_STATUS_NUM <- as.integer(home_insurance.df_filtered$OCC_STATUS)

# converting 'AD_BUILDINGS' into numerical variable and storing as 'AD_BUILDINGS_NUM'
home_insurance.df_filtered$AD_BUILDINGS_NUM <- as.integer(home_insurance.df_filtered$AD_BUILDINGS)

# converting 'AD_CONTENTS' into numerical variable and storing as 'AD_CONTENTS_NUM'
home_insurance.df_filtered$AD_CONTENTS_NUM <- as.integer(home_insurance.df_filtered$AD_CONTENTS)



home_insurance.df_filtered <- home_insurance.df_filtered[,c("YEARBUILT", "BEDROOMS", "FLOODING_NUM", "BUILDINGS_COVER_NUM","AD_BUILDINGS_NUM",
                                                                       "CONTENTS_COVER_NUM","AD_CONTENTS_NUM", "SPEC_SUM_INSURED", "SPEC_ITEM_PREM", 
                                                                       "UNSPEC_HRP_PREM", "RISK_RATED_AREA_B", "RISK_RATED_AREA_C",
                                                                       "OCC_STATUS_NUM", "P1_SEX_NUM", "P1_MAR_STATUS_NUM", "AGE", "LAST_ANN_PREM_GROSS")]
colnames(home_insurance.df_filtered)

nrow(home_insurance.df_filtered)
ncol(home_insurance.df_filtered)
str(home_insurance.df_filtered_new)

rowSums(is.na(home_insurance.df_filtered)) > 0
nrow(home_insurance.df_filtered)
home_insurance.df_filtered_null_removed <- home_insurance.df_filtered[rowSums(is.na(home_insurance.df_filtered)) == 0,]

nrow(home_insurance.df_filtered_null_removed)
rowSums(is.na(home_insurance.df_filtered))
home_insurance.df_filtered_null_removed <- home_insurance.df_filtered[complete.cases(home_insurance.df_filtered), ]

nrow(home_insurance.df_filtered_null_removed)



rowSums(is.na(home_insurance.df_filtered_null_removed)) > 0


write.csv(home_insurance.df_filtered_null_removed,'Home_Insurance_Cleaned5.csv', row.names = FALSE)

home_insurance.df_f_cleaned <- home_insurance.df_filtered_null_removed[-c(4:7)]
write.csv(home_insurance.df_f_cleaned,'Home_Insurance_Cleaned6.csv', row.names = FALSE)

colnames(home_insurance.df_f_cleaned)
table(sapply(home_insurance.df_f_cleaned, class))

sum(is.na(home_insurance.df_f_cleaned))