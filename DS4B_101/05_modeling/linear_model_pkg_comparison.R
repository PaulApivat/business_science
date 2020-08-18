# Linear Models Package Comparison

# Source: https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net

# Session Info (UPGRADE R 4.0.2)
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.5

# Load Libraries ----

set.seed(123) # seed for reproducibility
install.packages('glmnet')
library(glmnet) # for ridge regression
install.packages('tidyverse')
library(tidyverse) # for data cleaning
install.packages('mnormt')   # multivariate normal and t-distribution (dependency for psych package)
library(mnormt)
install.packages('psych')
library(psych)


data('mtcars')

# Standardize x and y via scale() function ----
y <- mtcars %>%
    select(mpg) %>%
    scale(center = TRUE, scale = FALSE) %>%
    as.matrix()

x <- mtcars %>%
    select(-mpg) %>%
    as.matrix()

# they are both standardized matrices
class(x) # all columns, but mpg
class(y) # mpg column

# Perform 10-fold cross-validation to select lambda ----

lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
 
class(lambdas_to_try)

# Setting Alpha = 0 implements Ridge Regression ----

ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, 
          standardize = TRUE, nfolds = 10)

ridge_cv

# Plot cross-validation results
plot(ridge_cv)

# Best cross-validated lambda

lambda_cv <- ridge_cv$lambda.min

# Fit final model, get its sum of squared residuals and multiple R-squared

model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)

y_hat_cv <- predict(model_cv, x)

# sum of square residual
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)

# r-square of ridge regression
rsq_ridge_cv <- cor(y, y_hat_cv)^2

rsq_ridge_cv

# Use Information Criteria to Select Lambda ----

x_scaled <- scale(x)
aic <- c()
bic <- c()

## missing psych package: results unpredictable ##

for (lambda in seq(lambdas_to_try)){
    # Run model (ridge regression)
    model <- glmnet(x, y, alpha = 0, lambda = lambdas_to_try[lambda], standardize = TRUE)
    # Extract coefficients and residuals (remove first row for the intercept)
    betas <- as.vector((as.matrix(coef(model))[-1, ]))
    resid <- y - (x_scaled %*% betas)
    # Compute hat-matrix and degress of freedom
    ld <- lambdas_to_try[lambda] * diag(ncol(x_scaled))
    H <- x_scaled %*% solve(t(x_scaled) %*% x_scaled + ld) %*% t(x_scaled)
    ###### ERROR: could not find function tr #######
    df <- tr(H)
    # compute information criteria
    aic[lambda] <- nrow(x_scaled) * log(t(resid) %*% resid) + 2 * df
    bic[lambda] <- nrow(x_scaled) * log(t(resid) %*% resid) + 2 * df * log(nrow(x_scaled))
}














