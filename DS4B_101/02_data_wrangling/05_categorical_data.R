# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CATEGORICAL DATA MANIPULATION ----

library(tidyverse)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl



# 1.0 Factor Basics ----

# What is a Factor?
# A way of managing categorical data

# Why do we want factors? 
# 1. Can group numeric values into bin (think price = low, medium, high)
# 2. Can reorder categories for visualization (fct_reorder)
# 3. Can manipulate categories much eaiser (fct_lump)
# 4. Machine learning and modeling algorithms may require factor data type for categorical data. 

# 2.0 Motivating Example -----

# Manipulation
sales_by_cat_2_tbl <- bike_orderlines_tbl %>%
    select(category_2, total_price) %>%
    
    group_by(category_2) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    
    arrange(desc(sales)) %>%
    mutate(category_2 = category_2 %>% as_factor() %>% fct_rev())

# Plotting
sales_by_cat_2_tbl %>%
    ggplot(aes(x = sales, y = category_2)) + 
    geom_point(size = 5, color = "#2c3e50") +
    labs(title = "Sales by Category 2") +
    scale_x_continuous(labels = scales::dollar_format()) +
    theme_tq() +
    expand_limits(x = 0)

# create plot_sales function from ggplot settings
plot_sales <- function(data){
    data %>%
        ggplot(aes(x = sales, y = category_2)) + 
        geom_point(size = 5, color = "#2c3e50") +
        labs(title = "Sales by Category 2") +
        scale_x_continuous(labels = scales::dollar_format()) +
        theme_tq() +
        expand_limits(x = 0)
}

# pipe dataframe into global function
sales_by_cat_2_tbl %>%
    plot_sales()

# Alternative reorder of factor within ggplot
    #ggplot(aes(x = sales, y = reorder(category_2, sales))) + geom_point(size = 5)




# 3.0 Forcats Basics ----


# 3.1 Inspecting Factors ----

# Vector
sales_by_cat_2_tbl %>% pull(category_2) %>% levels()

sales_by_cat_2_tbl %>% pull(category_2) %>% as.numeric()

# Tibble

# NOTE: Working with Factors
# as.character() retrieves text
# as.numeric() retrieves value

# Factors contain both label and value (levels, hidden)
sales_by_cat_2_tbl %>%
    mutate(category_2 = category_2 %>% fct_rev() %>% fct_rev()) %>%
    mutate(
        label = category_2 %>% as.character(),
        value = category_2 %>% as.numeric()
    )




# 3.2 Creating Factors: as_factor() vs as.factor() ----

# as_factor() forcats package - assign factor values base on order in vector (good for visualization)
# as.factor() base-R          - assign factor values base on alphabetical order in factor (NOT helpful for visualization)

sales_by_cat_2_tbl %>%
    mutate(
        category_2 = as.character(category_2),
        category_2_as_factor = as_factor(category_2) %>% as.numeric(),
        category_2_as.factor = as.factor(category_2) %>% as.numeric()
    )



# 3.3 Reording Factors: fct_reorder() and fct_rev() ----





# 3.4 Time-Based Reordering: fct_reorder2() ----





# 3.5 Creating "Other" Category - fct_lump() & fct_shift() ----





