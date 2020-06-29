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

bikes_tbl %>%
    select(model, price) %>%
    # by default, arrange goes from lowest to highest
    # use desc() for highest to lowest
    arrange(desc(price)) %>% 
    view()



# 3.0 Filtering Rows with filter() ----

# 3.1 filter(): formula filtering ----

bikes_tbl %>%
    select(model, price) %>%
    # can use mean() or median() for filter
    filter(price > mean(price))

# filter using logical statements OR, AND etc. 
bikes_tbl %>%
    select(model, price) %>%
    # filter using or statement
    filter((price > 5000) | (price < 1000)) %>%
    arrange(desc(price)) %>%
    view()

# filter using logical operators
# > < <= >= is.na() !is.na() %in% ! | &

# filter with multiple columns
# only model Supersix that's above 6000
bikes_tbl %>%
    select(model, price) %>%
    filter(price > 6000,
           model %>% str_detect("Supersix"))

bikes_tbl %>%
    select(model, price) %>%
    filter(model %>% str_detect("Supersix"))

# filter with logical boolean operators
# %in% == and !=

# Filter for one or more conditions using == and %in%
# use %in% for 2 or more categories within a column
bike_orderlines_tbl %>%
    filter(category_2 %in% c("Over Mountain", "Trail", "Endurance Road"))


bike_orderlines_tbl %>%
    filter(category_2=="Over Mountain")

bike_orderlines_tbl %>%
    filter(category_2 != "Over Mountain")

# filter for all categories except Over Mountain, Trail, Enduranc Road 
# use ! NOT 
bike_orderlines_tbl %>%
    filter(!(category_2 %in% c("Over Mountain", "Trail", "Endurance Road")))

# 3.2 slice(): filtering with row number(s) ----

# get top five rows with highest price
# arrange by price in descending order, then grab rows 1:5
bikes_tbl %>%
    arrange(desc(price)) %>%
    slice(1:5)

# get five rows with lowest price
#
bikes_tbl %>%
    arrange(price) %>%
    slice(1:5)


bikes_tbl %>%
    arrange(desc(price)) %>%
    slice(93:97)

bikes_tbl %>%
    arrange(desc(price)) %>%
    # nrow(.) gets total number of rows in the table; nrow(.)-4 = 93, nrow(.) = 97
    slice((nrow(.)-4):nrow(.))

# 3.3 distinct() Unique Values ----

bike_orderlines_tbl %>%
    distinct(category_1)

# distinct combination of category 1 and 2
bike_orderlines_tbl %>%
    distinct(category_1, category_2)

# unique bikeshop_names, city and state
bike_orderlines_tbl %>%
    distinct(bikeshop_name, city, state)



# 4.0 Adding Columns with mutate() ----

# Add column with mutate
bike_orderlines_prices <- bike_orderlines_tbl %>%
    select(order_date, model, quantity, price) %>%
    mutate(total_price = quantity * price)

bike_orderlines_prices

# Overwrite Column (provide new column name and transformation)
bike_orderlines_prices %>%
    mutate(total_price = log(total_price))

# Transformations on Column
bike_orderlines_prices %>%
    mutate(total_price_log = log(total_price)) %>%
    mutate(total_price_sqrt = total_price^0.5)


# Adding Flag (Binary Feature - True or False)
# An example of Feature Engineering - developing features based on knowledge of important aspect of data
bike_orderlines_prices %>%
    # feature eng of is_supersix column
    mutate(is_supersix = model %>% 
                         str_to_lower() %>% 
                         str_detect("supersix")) %>%
    # filter based on new feature
                         filter(is_supersix)


# Binning with ntile()
# useful for grouping into cohorts and detecting relationships within continuous variables

bike_orderlines_prices %>%
    # placing prices in High, Medium, Low bins - wow
    mutate(total_price_binned = ntile(total_price, 3))


# If-Then Statement statements inside mutate()
# case_when() - more flexible binning

# numeric to categorical using case_when()
bike_orderlines_prices %>%
    mutate(total_price_binned = ntile(total_price, 3)) %>%
    mutate(total_price_binned2 = case_when(
        # anything greater than 66th percentile (highest 3rd Bin)
        total_price > quantile(total_price, 0.66) ~ "High",
        total_price > quantile(total_price, 0.33) ~ "Medium",
        TRUE ~ "Low"
    ))

# Numeric to Categorical
# case_when() allows flexible adjustment of criteria for interquartile range
bike_orderlines_prices %>%
    mutate(total_price_binned = ntile(total_price, 3)) %>%
    mutate(total_price_binned2 = case_when(
        # anything greater than 75th percentile (some bin 3 are 'medium')
        total_price > quantile(total_price, 0.75) ~ "High",
        total_price > quantile(total_price, 0.25) ~ "Medium",
        TRUE ~ "Low"
    ))

# Text to Categorical (create categories)
bike_orderlines_prices %>%
    mutate(bike_type = case_when(
        model %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix",
        model %>% str_to_lower() %>% str_detect("jekyll") ~ "Jekyll",
        TRUE ~ "Not Supersix or Jekyll"
    ))


# 5.0 Grouping & Summarizing with group_by() and summarize() ----

# most cleaned data is very granular, need to Aggregate data to seed Trends
# the heart of working with TRANSACTIONAL data
# Typical aggregations: by order, by year, by product category, by customer

bike_orderlines_tbl %>%
    summarize(
        revenue = sum(total_price)
    )

bike_orderlines_tbl %>%
    group_by(category_1) %>%
    summarize(revenue = sum(total_price))

# find total revenue by category_1 and 2 in descending order
bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarize(revenue = sum(total_price)) %>%
    # always ungroup() after summarize(); prevent hard-to-detect errors
    ungroup() %>%
    arrange(desc(revenue))

# business insights: see which combination of category 1 and 2 and frame material
# is highest revenue generator
bike_orderlines_tbl %>%
    group_by(category_1, category_2, frame_material) %>%
    summarize(revenue = sum(total_price)) %>%
    # always ungroup() after summarize(); prevent hard-to-detect errors
    ungroup() %>%
    arrange(desc(revenue))

# Summary Functions (see dplyr cheatsheet)

# Frequency of categories
bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarize(
        count = n(),
        avg = mean(total_price),
        med = median(total_price),
        sd = sd(total_price),
        min = min(total_price),
        max = max(total_price)
    ) %>%
    ungroup() %>%
    arrange(desc(count))

# DETECTING AND HANDLING MISSING VALUES

# summarize_all() - detecting missing values

# inject missing values
bike_orderlines_missing <- bike_orderlines_tbl %>%
    # set first four rows (only) to missing values
    mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

# detect missing values
# ~ is.na(.) applies is.na() to all columns, returning 4 true, 15640 false
# ~ sum(is.na(.)) counts the number of true values, returning 4
bike_orderlines_missing %>%
    summarize_all(~ sum(is.na(.)))

# detect proportion (percentage) of missing data
bike_orderlines_missing %>% 
    summarize_all(~ sum(is.na(.)) / length(.))

# OPTIONS FOR HANDLING MISSING VALUES
# 1. filter() - remove
# 2. drop_na() - remove (filter shortcut)
# 3. fill() - taking leading/lagging value and replace up/down
# 4. replace_na() - replace by specifying
# 5. impute - programmatically replace (advanced)

bike_orderlines_missing %>%
    filter(!is.na(total_price))



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



