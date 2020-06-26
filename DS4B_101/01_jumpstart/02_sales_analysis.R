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

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
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
    
    # Reorder columns with select_helpers contains, everything
    select(contains('date'), contains('id'), contains('order'),
           quantity, price, total.price,
           everything()) %>%
    
    # Renaming columns: one at a time or multiple columns
    # dot in names(.) helps pass all column names of in-coming tibble
    # str_replace_all uses regex to replace a pattern
    # replace all . with underscore _  use regex \\ to escape because dot is special character
    rename(order_date = order.date) %>%
    set_names(names(.) %>% str_replace_all("\\.", "_")) 

bike_orderlines_wrangled_tbl %>% glimpse()

# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate

# Matt's method uses select() and mutate() before group()
# bike_orderlines_wrangled_tbl %>% group_by(order_date) %>% summarise(total_sales = sum(total_price))


sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
    # select columns to focus on and add a 'year' column
    select(order_date, total_price) %>%
    # use lubridate package function year() to gather the year from order_date
    mutate(year = year(order_date)) %>%
    
    # group by year and summarize sales
    group_by(year) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    
    # format $ Format Text use scales::dollar() function
    mutate(sales_text = scales::dollar(sales))

sales_by_year_tbl

# Step 2 - Visualize

sales_by_year_tbl %>%
    # setup canvas with year on x-axis and sales on y-axi
    ggplot(aes(x = year, y = sales)) + 
    # Geometries
    geom_col(fill = "#2C3E50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = 'lm', se = FALSE) +
    # formatting
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year",
        subtitle = "Upward trend",
        x = "",
        y = "Revenue"
    )


# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate

sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>%
    # select columns and add year column (add product category)
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>%
    # groupby and summarize by year and category
    group_by(year, category_2) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    
    # format $ text
    mutate(sales_text = scales::dollar(sales))

sales_by_year_cat_2_tbl


# Step 2 - Visualize

sales_by_year_cat_2_tbl %>%
    # setup x and y axis, fill argument
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    # add geometries
    geom_col() +
    geom_smooth(method = 'lm', se = FALSE) +
    # facet by categories
    # facet_wrap(~category_2, ncol = 3, scales = "free_y")
    facet_wrap(~category_2, ncol = 3, scales = 'free_y') +
    
    # formatting
    theme_tq() +
    scale_fill_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year and Category 2",
        subtitle = "Each product category has an upward trend",
        x = "",
        y = "Revenue",
        fill = "Product Secondary Category"
    )



# 7.0 Writing Files ----


# 7.1 Excel ----


# 7.2 CSV ----


# 7.3 RDS ----