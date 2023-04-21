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
names(df_sub)
# run a regression model to get the coefficient
model_original <- lm(X3TGPAMAT ~ X1SES + X1MTHEFF + X1TXMTSCOR, data = df_sub)
summary(model_original)



# before running the function , please load the package first
library(mvtnorm)
# write a function to generate data
dat_gen <- function(size=500, # datasize sets to 500 by default
                    beta_ = NULL, # input the betas
                    pred_means = NULL, # input the means of variables
                    pred_cov = NULL, # input the covariances of variables
                    error_sd = 1){
  if (is.null(beta_) == TRUE){
    # if user did not give a coefficients array, return warning.
    return("Error: You need to give the coefficients.")
  }else {
    if (is.null(pred_means)== TRUE){
      # if user did not give means of each variables,
      # use 0 as means by default
      predictor_nums <- length(beta_)
      if (is.null(pred_cov)== TRUE){
        return("Error: You have to give the covariance matrix of predictors")
      } else{ ### user gives the predictors covariance matrix
        X <- rmvnorm(n=size,sigma=pred_cov)
        Error <- as.matrix(rnorm(n=size, mean = 0, sd=error_sd))
        X_aug <- cbind(rep(1,nrow(X)), X)
        Y <- X_aug %*% as.matrix(beta_) + Error
        out_data <- cbind(Y,X)
      }
    }else{ ## user gives the means of predictors 
      if (is.null(pred_cov)==TRUE){
        print("Error: You have to give the covariance matrix of predictors.")
      } else{ ### user gives the predictors covariance matrix
        X <- rmvnorm(n=size,mean= pred_means,sigma=pred_cov)
        Error <- as.matrix(rnorm(n=size, mean = 0, sd=error_sd))
        X_aug <- cbind(rep(1,nrow(X)), X)
        Y <- X_aug %*% as.matrix(beta_) + Error
        out_data <- cbind(Y,X)
      }
    }
    # give columns names
    n_ = predictor_nums - 1
    x_vars <- c("Y")
    for (i in 1:n_) {
      x_vars[i+1] <- paste0("X",i)
    }
    colnames(out_data) <- x_vars
    return(as.data.frame(out_data))
  }
}


# type the coefficient
betas <- c(0.01, 0.178, 0.012, 0.043)
predictors_cov <- cov(df_sub[,1:3])
class(predictors_cov)
error_sd <- summary(model_original)$sigma

dataset_00 <- dat_gen(beta_=betas,pred_cov =predictors_cov,error_sd = error_sd)
head(dataset_00)
model_00 <-lm(Y ~ X1 + X2  + X3, data = dataset_00)
summary(model_00)

cov_matrix <- matrix(1,3,3)
X <- rmvnorm(n=10, sigma=cov_matrix)

# test
data(mtcars)
# Fit a linear regression model to predict miles per gallon (mpg) based on horsepower (hp)
model <- lm(mpg ~ hp+wt, data = mtcars)

# View the summary of the model
summary(model)
cvo <- var(mtcars[,c("hp","wt")])

cars_data <- dat_gen(beta_=c(30,-0.032,-3.88),
                     error_sd = summary(model)$sigma)

cars_data 

cars <- lm(Y~X1+X2, data = cars_data )
summary(cars)
X <- rmvnorm(n=500,sigma=cvo)