# Linear Models Package Comparison

# Source: https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net

# Load Libraries ----

set.seed(123) # seed for reproducibility
library(glmnet) # for ridge regression
library(dplyr) # for data cleaning

data('mtcars')

# Standardize x and y via scale() function ----
y <- mtcars %>%
    select(mpg) %>%
    scale(center = TRUE, scale = FALSE) %>%
    as.matrix()

x <- mtcars %>%
    select(-mpg) %>%
    as.matrix()

# they are both matrices
class(x)
class(y)

# Perform 10-fold cross-validation to select lambda ----

lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
 
class(lambdas_to_try)

# Setting Alpha = 0 implements Ridge Regression ----

ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, 
          standardize = TRUE, nfolds = 10)

ridge_cv

# Plot cross-validation results
plot(ridge_cv)
