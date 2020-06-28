# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# DATA WRANGLING OVERVIEW ----

# Wrangling = Cleaning + Preparing

library(tidyverse)
library(readxl)

bikes_tbl           <- read_excel("../00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("../00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bikes_tbl
orderlines_tbl
bike_orderlines_tbl %>% glimpse()


# 1.0 Selecting Columns with select() ----

# Why select? 
# 1. To Remove Columns or 
# 2. Re-arrange Order of Columns

# Three different ways to select the first three columns in bike_orderlines_tbl

# select column names
bike_orderlines_tbl %>% 
    select(order_date, order_id, order_line)

# select by numeric vector of column positions
bike_orderlines_tbl %>% 
    select(1:3)

# select helpers
bike_orderlines_tbl %>%
    select(starts_with("order_"))

# Resuce Columns

bike_orderlines_tbl %>% 
    select(order_date, total_price, category_1, category_2)

# Re-arrange Columns

bike_orderlines_tbl %>%
    select(bikeshop_name:state, everything())

# Select helpers
?starts_with

# use contains, ends_with and start_with
bike_orderlines_tbl %>%
    select(contains("price"))

# Select variables that match a pattern
starts_with
ends_with
contains
matches
num_range
everything()

# Pull

# If you want the average of a column of numbers
# mean() returns NA because it's expecting a numeric vector, not a tibble
bike_orderlines_tbl %>%
    select(total_price) %>% 
    mean()

# use pull() instead select() to pull out numbers from column into numeric vector
bike_orderlines_tbl %>%
    #select(total_price) %>%
    pull(total_price) %>%
    mean()

# pull() vs select()
bike_orderlines_tbl %>%
    pull(model)
    #select(model)

# select_if()
?select_if

# select if column contains character data-type
# note: use is.character, without parentheses is.character()
bike_orderlines_tbl %>%
    select_if(is.character)

# select if numeric
bike_orderlines_tbl %>%
    select_if(is.numeric)

# alternative way using tilde
bike_orderlines_tbl %>%
    select_if(~ is.numeric(.))

# select if NOT numeric (must use tilde)
bike_orderlines_tbl %>%
    select_if(~ !is.numeric(.))

# 2.0 Arranging with arrange() and desc() ----





# 3.0 Filtering Rows with filter() ----

# 3.1 filter(): formula filtering ----



# 3.2 slice(): filtering with row number(s) ----





# 4.0 Adding Columns with mutate() ----




# 5.0 Grouping & Summarizing with group_by() and summarize() ----




# 6.0 Renaming columns with rename() and set_names() ----

# 6.1 rename: One column at a time ----


# 6.2 set_names: All columns at once ---




# 7.0 Reshaping (Pivoting) Data with spread() and gather() ----

# 7.1 spread(): Long to Wide ----


# 7.2 gather(): Wide to Long ----




# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel) ----




# 9.0 Binding Data by Row or by Column with bind_rows() and bind_col() ----

# 9.1 bind_cols() ----




# 9.2 bind_rows() ----



