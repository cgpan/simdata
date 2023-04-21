# Author: Chenguang Pan
# Date:   April,20th, 2023
# Aim:    This first version of this file is to simmulate data for
#         any linear regression model

# import the dataset
df <- read.csv("~/Desktop/PhD_Learning/DeCarlo's Meeting/simdata/01_data/02_processed_data/hsls_sub.csv")
dim(df)
names(df)

# this dataset is too huge I randomly choose 500 observation for data simulation test
rand_obs <- sample(1:nrow(df),500)

# randomly subset the df into 500 observations
df_sub <- df[rand_obs,]
dim(df_sub)

# 