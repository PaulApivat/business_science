# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PRIMER ON PURRR ----
# Programmatically getting Excel files into R
excel_paths_tbl <- fs::dir_info("../00_data/bike_sales/data_raw/")

paths_chr <- excel_paths_tbl %>%
    pull(path)


# What Not To Do: Don't use for loops
excel_list <- list()

for (path in paths_chr){
    excel_list[[path]] <- read_excel(path)
}

excel_list

# What to Do: Use map()
?map

# Method 1: function name
excel_list_2 <- paths_chr %>%
    map(read_excel) %>%
    set_names(paths_chr)

# Method 2: Anonymous Function
paths_chr %>%
    map(~ read_excel(.))

# Method 3: Function Specified with function()
paths_chr %>%
    map(function(x) read_excel(path = x))


# Reading Excel (MULTIPLE) SHEETS

excel_sheets("../00_data/bike_sales/data_raw/bikes.xlsx") %>%
    map(~ read_excel(path = "../00_data/bike_sales/data_raw/bikes.xlsx", sheet = .))


# 2.0 MAPPING DATA FRAMES ----

# 2.1 Column-wise Map ----

bike_orderlines_tbl %>% is.list()

# allows greater flexibility
bike_orderlines_tbl %>%
    map(~ class(.))

# does not allow flexibility
bike_orderlines_tbl %>%
    map(class)



# 2.2 Map Variants ----

# Character Map: convert map output into a list of characters
# converting *first* element of each class to named character
bike_orderlines_tbl %>%
    map_chr(~ class(.)[1])

# Data Frame Map: convert map output into list
# converting into a tibble
bike_orderlines_tbl %>%
    map_df(~ class(.)[1]) %>%
    # put into key-value pair
    gather()

# find length of each column
bike_orderlines_tbl %>%
    map_df(~ length(.))

# find percentage of missing values
# great way to map through a data frame, for each column, see proportion of missing values
bike_orderlines_tbl %>%
    map_df(~ sum(is.na(.)) / length(.)) %>%
    gather()


# 2.3 Row-wise Map ----

# MORE common than mapping over column

# BREAKING CHANGE: convert 'excel_paths_tbl' to tibble

# Store all excel tables in a table (instead of a list)
excel_tbl <- excel_paths_tbl %>%
    as_tibble() %>%
    select(path) %>%
    mutate(data = path %>% map(read_excel))

# list vs tibble
excel_list  # same as excel_tbl$data

excel_tbl



# 3.0 NESTED DATA ----

# Unnest
excel_tbl

excel_tbl$data

# Grabbing each table within the excel list of tibble, one-by-one
excel_tbl$data[[1]]

excel_tbl$data[[2]]

excel_tbl$data[[3]]

# UNNEST: turns list of 3 tibbles into a Single level data-frame (columns don't match up, bunch of NA values)
# note: 97 + 30 + 15,644 = 15,771
# note: 4 + 3 + 7 + path(column) = 15

# NOTE: .id support for unnest() is deprecated, use unnest_legacy() for now

excel_tbl_unnested <- excel_tbl %>%
    unnest_legacy(data, .id = "ID")


excel_tbl_unnested

# Nest

# RE-Nesting Unnested data
excel_tbl_nested <- excel_tbl_unnested %>%
    group_by(ID, path) %>%
    nest()

excel_tbl_nested$data

# Mapping Nested List Columns

# Note: create functions that works on 1 element
# note: mapping nested list columns to DROP NA values

x <- rep(NA, 5)
x

# detect if column is NOT ALL NA
!is.na(x) %>% all()

# detect if column IS ALL NA
is.na(x) %>% all()

y <- c(1:4, NA_real_)
y

# False because NOT all values are NA
is.na(y) %>% all()

# TRUE because NOT all values are NA
!is.na(y) %>% all()

excel_tbl_nested$data[[3]] %>%
    # select_if NOT NA, for all of the column
    select_if(~ !is.na(.) %>% all())


# How to Scale with mutate() + map() w/ FUNCTIONS


excel_tbl_nested

# Method 1: Creating a function outside of purrr::map()

# STEP 1: Create a function that can be mapped to ONE element
select_non_na_columns <- function(data){
    
    data %>%
        select_if(~ !is.na(.) %>% all())
    
}

# STEP 2: Extract an element, and test the function
# Newly created function select_non_na_columns works on ONE data table
excel_tbl_nested$data[[2]] %>%
    select_non_na_columns()

# STEP 3: Use mutate() + map()
# Scaling select_non_na_columns function to MORE than one data table
excel_tbl_nested_fixed <- excel_tbl_nested %>%
    mutate(data_fixed = data %>% map(select_non_na_columns))

# select_non_na_columns function successfully drops all NA columns from multiple data tables
excel_tbl_nested_fixed$data_fixed[[3]]


# 4.0 MODELING WITH PURRR ----

# 4.1 Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))




# 4.2 Modeling Primer ----

# Data Preparation

sales_by_m_cross_country_tbl <- rolling_avg_3_tbl %>%
    filter(category_2 == 'Cross Country Race') %>%
    select(month_end, total_price) %>%
    mutate(month_end_num = as.numeric(month_end))


sales_by_m_cross_country_tbl %>%
    ggplot(aes(month_end_num, total_price)) +
    geom_point() +
    geom_smooth(method = "loess", span = 0.2, se = FALSE)



# Making a loess model
?loess

fit_loess_cross_country <- sales_by_m_cross_country_tbl %>% 
    # first argument is formula - total_price ~ month_end_num
    # second argument is the "data" from sales_by_m_cross_country_tbl (note period: '.')
    # third argument span allows line to fit the points better
    loess(total_price ~ month_end_num, data = ., span = 0.2)


fit_loess_cross_country


# Working With Broom
# to extract information from fit_loess_cross_country

# grab 'fitted' in a data frame using broom library
fit_loess_cross_country %>%
    # returns fitted, std error, residuals from the model
    broom::augment() %>%
    
    # Visualizing results
    ggplot(aes(x = month_end_num, y = total_price)) +
    geom_point() +
    geom_line(aes(y = .fitted), color = 'blue')

    
    

    




# 4.3 Step1: Function To Return Fitted Results ----

## OBJECTIVE: take rolling_avg_3_tbl sales data, add loess smoother to all category_1, and category_2
## at scale - for all groups

rolling_avg_3_tbl_nested <- rolling_avg_3_tbl %>%
    group_by(category_1, category_2) %>%
    # we now have nested dataframe - a dataframe with tibbles in each cell acrsso category_1 and category_2
    nest()


rolling_avg_3_tbl_nested$data[[1]]

## PRO TIP: When making functions, save some testable data as each argument so you can
## interactively test the function while you build it. 

data <- rolling_avg_3_tbl_nested$data[[1]]

tidy_loess <- function(data, span = 0.2){
    
    data_formatted <- data %>%
        select(month_end, total_price) %>%
        mutate(month_end_num = as.numeric(month_end))
    
    fit_loess <- loess(formula = total_price ~ month_end_num, 
                       data    = data_formatted, 
                       span    = span)
    
    output_tbl <- fit_loess %>%
        broom::augment() %>%
        select(.fitted)
    
    return(output_tbl)
}



# 4.4 Test Function on Single Element ----

rolling_avg_3_tbl_nested$data[[3]] %>%
    tidy_loess()



# 4.5 Map Function to All Categories ----

# Map Functions



# Visualize Results



