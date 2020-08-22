# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# REGRESSION MODELS ----

# GOAL: BUILD PREDICTION MODEL FOR PRICING ALGORITHM


# LIBRARIES & DATA ----

pkgs <- c("parsnip", "glmnet", "rpart", "rpart.plot", "ranger", "randomForest", "xgboost", "kernlab")
# If any of these packages are not installed, run this: install.packages(pkgs)

# Standard
library(readxl)
library(tidyverse)
library(tidyquant)
install.packages('tidyquant')
# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)

# Modeling Error Metrics
install.packages('yardstick')
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

# Libraries / Packages need to be installed after upgrade to R 4.0.2
install.packages('ggrepel')
library(ggrepel)


# Source Scripts
source("../00_scripts/separate_bikes_and_outlier_detection.R")

# Read Data
bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PROBLEM DEFINITION ----
# - Which Bike Categories are in high demand?
# - Which Bike Categories are under represented?
# - GOAL: Use a pricing algorithm to determine a new product price in a category gap

model_sales_tbl <- bike_orderlines_tbl %>%
    select(total_price, model, category_2, frame_material) %>%
    
    group_by(model, category_2, frame_material) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    arrange(desc(total_sales))

model_sales_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
                fct_reorder(total_sales, .fun = max) %>% 
               fct_rev()) %>%
    
    ggplot(aes(frame_material, total_sales)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    #coord_flip() +
    facet_wrap(~ category_2) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
    theme_tq() +
    labs(
        title = "Total Sales for Each Model",
        x = "Frame Material", y = "Revenue"
    )


# 2.0 TRAINING & TEST SETS ----

bike_features_tbl <- bike_orderlines_tbl %>%
    select(price, model, category_2, frame_material) %>%
    distinct() %>%
    # add row_number to keep track when split into test & train
    mutate(id = row_number()) %>%
    select(id, everything()) %>%
    separate_bike_model(keep_model_column = TRUE, append = TRUE)

bike_features_tbl

# NOTE: 97 observations is a small sample size

set.seed(seed = 1113)
# 80% goes into training set, 20% goes into test set
split_obj <- rsample::initial_split(bike_features_tbl, prop = 0.80, strata = "model_base")

# should have 18 'levels'
# we want all levels represented in the training set


split_obj %>% training() %>% distinct(model_base)

# testing will have 12 'levels
split_obj %>% testing() %>% distinct(model_base)

train_tbl <- training(split_obj)

test_tbl <- testing(split_obj)

# quickly examine model_base
# Pro-Tip: the model_base feature has 18 levels. 
# A random split may not have all levels in the training set, which is bad.
# We can try to prevent this by adding model_base as a stratafication variable (strata = )
bike_features_tbl %>% distinct(model_base)

# 3.0 LINEAR METHODS ----
?linear_reg
?set_engine
?fit
?predict.model_fit
?metrics

# General interface for Linear Regression Models (IN PARSNIP )
# three arguments:
# 1. mode (only possible value for this model is 'regression')
# 2. penalty - amount of regularization in the model
# 3. mixture - types of regularization (1 = pure lasso, 0 = ridge regression)

# Engine types: lm, glmnet, stan, spark, keras


# 3.1 LINEAR REGRESSION - NO ENGINEERED FEATURES ----

# NOTE: Parsnip API - three steps:
# 1. Create a model        --> linear_reg()
# 2. Set an engine         --> set_engine()
# 3. fit the model to data --> fit()

# 3.1.1 Model ----
?lm


model_01_linear_lm_simple <- linear_reg(mode = "regression") %>%
    set_engine("lm") %>%
    # see training_tbl, the actual data to fit the model
    fit(price ~ category_2 + frame_material, data = train_tbl)

# outputs a 19 x 1 tibble of *predicted* price
# compare with price column from test_tbl
model_01_linear_lm_simple %>%
    predict(new_data = test_tbl) %>%
    # calculate Model Metrics manually
    # put actual price with predicted price side-by-side w/ bind_cols
    bind_cols(test_tbl %>% select(price)) %>%
    mutate(residuals = price - .pred) %>%
    # calculate Mean Absolute Error & Root Mean Square Error
    summarize(
        mae = abs(residuals) %>% mean(),
        rmse = mean(residuals^2)^0.5
    )

# simpler way using yardstick() package

model_01_linear_lm_simple %>%
    predict(new_data = test_tbl) %>%
    # calculate Model Metrics manually
    # put actual price with predicted price side-by-side w/ bind_cols
    bind_cols(test_tbl %>% select(price)) %>%
    # can use this instead of bind_cols, mutate and summarize manually
    yardstick::metrics(truth = price, estimate = .pred)


# Model Metrics: Calculate model metrics comparing test data predictions with actual values
# to get baseline model performance

# 3.1.2 Feature Importance ----
model_01_linear_lm_simple$fit %>% class()

model_01_linear_lm_simple$fit %>%
    broom::tidy() %>%
    arrange(p.value) %>%
    mutate(term = as.factor(term) %>% fct_rev()) %>%
    ggplot(aes(x = estimate, y = term)) +
    geom_point() +
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = 'Linear Regression: Feature Importance',
         subtitle = 'Model 01: Simple lm Model')

# 3.1.3 Function to Calculate Metrics ----

# NOTE: calc_metrics() is a helper function

model_01_linear_lm_simple %>%
    predict(new_data = test_tbl) %>%
    # calculate Model Metrics manually
    # put actual price with predicted price side-by-side w/ bind_cols
    bind_cols(test_tbl %>% select(price)) %>%
    # can use this instead of bind_cols, mutate and summarize manually
    yardstick::metrics(truth = price, estimate = .pred)

calc_metrics <- function(model, new_data = test_tbl) {
    model %>%
        predict(new_data = new_data) %>%
        # calculate Model Metrics manually
        # put actual price with predicted price side-by-side w/ bind_cols
        bind_cols(new_data %>% select(price)) %>%
        # can use this instead of bind_cols, mutate and summarize manually
        yardstick::metrics(truth = price, estimate = .pred)
}

model_01_linear_lm_simple %>% calc_metrics(test_tbl)

# 3.2 LINEAR REGRESSION - WITH ENGINEERED FEATURES ----

# 3.2.1 Model ----

train_tbl

model_02_linear_lm_complex <- linear_reg("regression") %>%
    set_engine("lm") %>%
    # price ~ ., means price as a function of ALL predictor columns
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))


model_02_linear_lm_complex %>% calc_metrics(new_data = test_tbl)

# PRO TIP: The NUMBER ONE WAY TO IMPROVE MODEL PERFORMANCE IS
# INCLUDE BETTER FEATURES (spend max time here)
# >>> Advanced models won't help if you don't have good features

# model_01_linear_lm_simple, features = category_2 + frame_material
# vs
# model_02_lienar_lm_complex, features = category_2, frame_material, model_base, black, hi_mod, team, red, ultegra, dura_ace and disc


# 3.2.2 Feature importance ----

model_02_linear_lm_complex$fit %>%
    broom::tidy() %>%
    arrange(p.value) %>%
    mutate(term = as.factor(term) %>% fct_rev()) %>%
    ggplot(aes(x = estimate, y = term)) +
    geom_point() +
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = 'Linear Regression: Feature Importance',
         subtitle = 'Model 02: Complex lm Model')


# 3.3 PENALIZED REGRESSION ----

# General interface for Linear Regression Models (IN PARSNIP )
# three arguments:
# 1. mode (only possible value for this model is 'regression')
# 2. penalty - amount of regularization in the model
# 3. mixture - types of regularization (1 = pure lasso, 0 = ridge regression)

# 3.3.1 Model ----
?linear_reg
?glmnet::glmnet

# mixture can go from 0 to 1
# penalty just needs to be non-negative
model_03_linear_glmnet <- linear_reg(mode = 'regression', penalty = 100, mixture = 0.20) %>%
    set_engine("glmnet") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

# Mean Absolute Error (1086) vs (1130) from previous model_02_linear_complex
# changing penalty from 10 to 100 improves MAE, slightly, from 1086 to 1040
# changing mixture from 0.5 to 0.25 improves MAE,slightly from 1040 to 1009
# changing BOTH penalty & mixture to penalty (100) and mixture (0.20) yields best MAE (1005)
model_03_linear_glmnet %>% calc_metrics(test_tbl)

# CHANGING penalty and mixture to find the best MAE is Hyper Parameter Tuning
# systematically adjusting model parameters to optimize the performance

# GRID SEARCH a popular hyper paramter tuning method produces a "grid" that
# has combinations of parameters

# 3.3.2 Feature Importance ----

model_03_linear_glmnet$fit %>%
    broom::tidy() %>%
    filter(lambda >= 10 & lambda < 11) %>%
    arrange(desc(abs(estimate))) %>%
    mutate(term = as.factor(term) %>% fct_rev()) %>%
    ggplot(aes(x = estimate, y = term)) +
    geom_point() +
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = 'Linear Regression: Feature Importance',
         subtitle = 'Model 03: GLMNET Model')


# 4.0 TREE-BASED METHODS ----

# 4.1 DECISION TREES ----

# 4.1.1 Model ----
?decision_tree
?rpart::rpart

set.seed(1234)
model_04_tree_decision_tree <- decision_tree(mode = "regression", 
                                             cost_complexity = 0.01, 
                                             tree_depth      = 5, 
                                             min_n           = 10) %>%
    set_engine("rpart") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))


model_04_tree_decision_tree %>% calc_metrics(test_tbl)


# 4.1.2 Decision Tree Plot ----

rpart.plot()

model_04_tree_decision_tree$fit %>%
    rpart.plot(roundint = FALSE)


model_04_tree_decision_tree$fit %>%
    rpart.plot(
        roundint = FALSE, 
        type = 1, 
        extra = 101, 
        fallen.leaves = FALSE, 
        cex = 0.7,
        main = "Model 04: Decision Tree",
        box.palette = "Blues")



# 4.2 RANDOM FOREST ----

# 4.2.1 Model: ranger ----
?rand_forest()
?ranger::ranger

set.seed(1234)
model_05_rand_forest_ranger <- rand_forest(
    mode = "regression", mtry = 4, trees = 1000, min_n = 10
    ) %>%
    set_engine("ranger", splitrule = "extratrees", importance = "impurity") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))


model_05_rand_forest_ranger %>% calc_metrics(test_tbl)

# 4.2.2 ranger: Feature Importance ----

# ranger::importance() returns the variable importance information
# make sure to set importance to "impurity" in the seg_engine(), see above.

model_05_rand_forest_ranger$fit %>% 
    ranger::importance() %>%
    enframe() %>%
    arrange(desc(value)) %>%
    mutate(name = as_factor(name) %>% fct_rev()) %>%
    ggplot(aes(value, name)) + 
    geom_point() +
    labs(title = "ranger: Variable Importance",
         subtitle = "Model 05: Ranger Random Forest Model")



# 4.2.3 Model randomForest ----
?rand_forest()
?randomForest::randomForest

set.seed(1234)
model_06_rand_forest_randomForest <- rand_forest("regression") %>%
    set_engine("randomForest") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

## ERROR ##
model_06_rand_forest_randomForest %>% calc_metrics(test_tbl)



# 4.2.4 randomForest: Feature Importance ----

model_06_rand_forest_randomForest$fit %>%
    # this is a matrix
    randomForest::importance() %>%
    # turns matrix into tibble
    as_tibble(rownames = "name") %>%
    arrange(desc(IncNodePurity)) %>%
    mutate(name = as_factor(name) %>% fct_rev()) %>%
    
    ggplot(aes(IncNodePurity, name)) +
    geom_point() +
    labs(
        title = "randomForest: Variable Importance",
        subtitle = "Model 06: randomForest Model"
    )


# 4.3 XGBOOST ----

# 4.3.1 Model ----
?boost_tree
?xgboost::xgboost

set.seed(1234)
model_07_boost_tree_xgboost <- boost_tree(
    mode = "regression",
    learn_rate = 0.15,
    tree_depth = 6
    ) %>%
    set_engine("xgboost") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_07_boost_tree_xgboost %>% calc_metrics(test_tbl)

# 4.3.2 Feature Importance ----
?xgboost::xgb.importance


model_07_boost_tree_xgboost$fit %>%
    xgboost::xgb.importance(model = .) %>%
    as_tibble() %>%
    arrange(desc(Gain)) %>%
    mutate(Feature = as_factor(Feature) %>% fct_rev()) %>%
    
    ggplot(aes(Gain, Feature)) +
    geom_point() +
    labs(
        title = "XGBoost: Variable Importance",
        subtitle = "Model 07: XGBoost Model"
    )



# 5.0 TESTING THE ALGORITHMS OUT ----

g1 <- bike_features_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(price)) %>%
    
    ggplot(aes(category_2, price)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    coord_flip() +
    facet_wrap(~ frame_material) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_tq() +
    labs(
        title = "Unit Price for Each Model",
        y = "", x = "Category 2"
    )

# 5.1 NEW JEKYLL MODEL ----

new_over_mountain_jekyll <- tibble(
    model = "Jekyll Al 1",
    frame_material = "Aluminum",
    category_2 = "Over Mountain",
    model_base = "Jekyll",
    model_tier = "Aluminum 1",
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 

new_over_mountain_jekyll

# Linear Methods ----

predict(model_03_linear_glmnet, new_data = new_over_mountain_jekyll)


# Tree-Based Methods ----

predict(model_07_boost_tree_xgboost, new_data = new_over_mountain_jekyll)

# Iteration

# PRO TIP: Data Frames can be a very useful way to keep models organized!
# Just put them in a 'list-column'

models_tbl <- tibble(
    model_id = str_c("Model 0", 1:6),
    model = list(
        model_01_linear_lm_simple,
        model_02_linear_lm_complex,
        model_03_linear_glmnet,
        model_04_tree_decision_tree,
        model_05_rand_forest_ranger,
        model_07_boost_tree_xgboost
    )
)

models_tbl

# Add Predictions

predictions_new_over_mountain_tbl <- models_tbl %>%
    mutate(predictions = map(model, predict, new_data = new_over_mountain_jekyll)) %>%
    unnest(predictions) %>%
    mutate(category_2 = "Over Mountain") %>%
    left_join(new_over_mountain_jekyll, by = "category_2")

predictions_new_over_mountain_tbl

# Update plot

g1 +
    geom_point(aes(y = .pred), color = "red", alpha = 0.5,
               data = predictions_new_over_mountain_tbl) +
    ggrepel::geom_text_repel(aes(label = model_id, y = .pred),
                             size = 3,
                             data = predictions_new_over_mountain_tbl)

# 5.2 NEW TRIATHALON MODEL ----

new_triathalon_slice_tbl <- tibble(
    model = "Slice Al 1",
    frame_material = "Aluminum",
    category_2 = "Triathalon",
    model_base = "Slice",
    model_tier = "Ultegra",
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 


# Linear Methods ----


# Tree-Based Methods ----






# 6.0 ADDITIONAL ADVANCED CONCEPTS ----

# - CLASSIFICATION - Binary & Multi-Class
# - ADVANCED ALGORITHMS
#   - SVMs - svm_poly() and svm_rbf() - Must be normalized
#   - Neural Networks - keras - Must be normalized
#   - Stacking Models 
# - PREPROCESSING - recipes 
# - HYPERPARAMETER TUNING - purrr
# - SAMPLING & CROSS VALIDATION - rsample 
# - AUTOMATIC MACHINE LEARNING - H2O




