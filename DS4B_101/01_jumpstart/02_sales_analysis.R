# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis ----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)

# note: set directory
setwd("/Users/paulapivat/Desktop/RCode")


# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "business_science/DS4B_101/00_data/bike_sales/data_raw/bikes.xlsx")

bikeshops_tbl <- read_excel(path = "business_science/DS4B_101/00_data/bike_sales/data_raw/bikeshops.xlsx")

orderlines_tbl <- read_excel(path = "business_science/DS4B_101/00_data/bike_sales/data_raw/orderlines.xlsx")

# 3.0 Examining Data ----

bikes_tbl
glimpse(bikes_tbl)
bikeshops_tbl
orderlines_tbl


# 4.0 Joining Data ----

?left_join

orderlines_tbl
bikes_tbl
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

# join more than two tables
bike_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----

# motivation to wrangle "description" column in bike_orderlines_joined_tbl has multiple descriptions
# "location" column in bike_orderlines_joined_tbl has city, state (multiple descriptors)
# we want one descriptor per cell

bike_orderlines_joined_tbl %>%
    # separate description into cateogory.1, category.2, frame.material
    separate(col = description, 
             into = c('category.1', 'category.2', 'frame.material'),
             sep = " - ",
             remove = TRUE) %>%

    # separate location into city and state
    separate(col = location,
             into = c('city', 'state'),
             sep = ", ",
             remove = FALSE) %>%
    
    # price extended (quantity x unit price)
    mutate(total.price = price * quantity) %>%
    
    # Reorganize (remove specific columns); negative means select 'out'
    select(-...1, -location) %>%
    select(-ends_with(".id")) %>%
    # get order.id column back with bind_cols
    # go back to bike_orderlines_joined_tbl and select order.id to bind back
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
    
    glimpse()


# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate




# Step 2 - Visualize



# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate




# Step 2 - Visualize




# 7.0 Writing Files ----


# 7.1 Excel ----


# 7.2 CSV ----


# 7.3 RDS ----