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

# RIDGE REGRESSION ----
# Setting Alpha = 0 implements Ridge Regression 

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
    df <- tr(H)
    # compute information criteria
    aic[lambda] <- nrow(x_scaled) * log(t(resid) %*% resid) + 2 * df
    bic[lambda] <- nrow(x_scaled) * log(t(resid) %*% resid) + 2 * df * log(nrow(x_scaled))
}

# Plot information criteria against tried values of lambdas

# AIC: type = 'l' for lambda
plot(log(lambdas_to_try), aic, col = 'orange', type = 'l',
     ylim = c(190, 260), ylab = 'Information Criterion')

# BIC line
lines(log(lambdas_to_try), bic, col = 'skyblue3')

# plot legend
legend("bottomright", lwd = 1, col = c("orange", "skyblue3"), legend = c("AIC", "BIC"))

# Optimal lambdas according to both criteria
lambda_aic <- lambdas_to_try[which.min(aic)]
lambda_bic <- lambdas_to_try[which.min(bic)]

# Fit final models, get their sum of squared residuals and multiple R-squared

# model_aic
model_aic <- glmnet(x, y, alpha = 0, lambda = lambda_aic, standardize = TRUE)
y_hat_aic <- predict(model_aic, x)
ssr_aic <- t(y - y_hat_aic) %*% (y - y_hat_aic)
rsq_ridge_aic <- cor(y, y_hat_aic)^2

# model_bic
model_bic <- glmnet(x, y, alpha = 0, lambda = lambda_bic, standardize = TRUE)
y_hat_bic <- predict(model_bic, x)
ssr_bic <- t(y - y_hat_bic) %*% (y - y_hat_bic)
rsq_ridge_bic <- cor(y, y_hat_bic)^2


# Note how increasing lambda shrinks the coefficients 
# Each line shows coefficients for one variable, for different lambdas
# The higher the lambda, the more the coefficients are shrinked towards zero.

res <- glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = TRUE)

plot(res, xvar = "lambda")

legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)


# LASSO REGRESSION ----

# NOTE: re-use glmnet() function but with alpha parameter set to 1

# Setting alpha = 1, implements lasso regression
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)

# PLot cross-validation results
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv_2 <- lasso_cv$lambda.min

# Fit final model, get it's sum of squared residuals and multiple R-squared
model_cv_2 <- glmnet(x, y, alpha = 1, lambda = lambda_cv_2, standardize = TRUE)

y_hat_cv_2 <- predict(model_cv_2, x)

ssr_cv_2 <- t(y - y_hat_cv_2) %*% (y - y_hat_cv_2)

rsq_lasso_cv <- cor(y, y_hat_cv_2)^2


# Note how increasing lambda shrinks the coefficients 
# Each line shows coefficients for one variable, for different lambdas
# The higher the lambda, the more the coefficients are shrinked towards zero.

res_2 <- glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = TRUE)

plot(res_2, xvar = 'lambda')

legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# RIDGE VS LASSO ----






























