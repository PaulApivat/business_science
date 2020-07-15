# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation

order_value_tbl <- bike_orderlines_tbl %>%
    
    select(order_id, order_line, total_price, quantity) %>%
    
    group_by(order_id) %>%
    summarize(
        total_quantity = sum(quantity),
        total_price = sum(total_price)
        ) %>%
    ungroup() 
    
    


# Scatter Plot

order_value_tbl %>%

    ggplot(aes(x = total_quantity, y = total_price)) + 
    geom_point(alpha = 0.2, size = 2, shape = "triangle") +
    geom_smooth(method = 'lm', se = FALSE, color = "red", linetype = "dashed", size = 1.5)


# 2.0 Line Plots ----
# - Great for time series

# Goal: Describe revenue by Month, expose cyclic nature

# Data Manipulation

revenue_by_month_tbl <- bike_orderlines_tbl %>% 
    
    select(order_date, total_price) %>%
    # key for this time-series line chart is converting timestamp into date w/
    # floor_date() and ymd()
    mutate(year_month = floor_date(x = order_date, "months") %>% ymd()) %>%
    
    group_by(year_month) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() 


# Line Plot
revenue_by_month_tbl %>%
    
    ggplot(aes(x=year_month, y=revenue)) +
    geom_line(size = 0.5, linetype = 1) +
    geom_smooth(method = "loess", span = 0.2)



## Pro Tip: use help documentation, you can drill into the algorithm parameters
## find out which parameters can be adjusted.





# 3.0 Bar / Column Plots ----
# - Great for categories

# Goal: Sales by Descriptive Category

# Data Manipulation

revenue_by_category2_tbl <- bike_orderlines_tbl %>% 
    
    select(category_2, total_price) %>%
    
    group_by(category_2) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() 
    
    

# Bar Plot
revenue_by_category2_tbl %>%
    # turn category_2 into a factor, then re-order
    mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
    ggplot(aes(x=category_2, y=revenue)) + 
    geom_col(fill = 'dodgerblue') + 
    coord_flip()


# my solution
revenue_by_category2_tbl %>%
    ggplot(aes(x=revenue, y=reorder(category_2, revenue))) +
    geom_bar(stat = 'identity')



# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a CONTINUOUS variable


# Goal: Unit price of bicycles
# Histogram

bike_orderlines_tbl %>%
    
    distinct(model, price) %>%
    # mapping only price, because this is a UNIvariate plot
    ggplot(aes(price)) +
    # use color to distinguish the bins
    geom_histogram(bins = 25, fill = 'orange', color = "white") 





# Goal: Unit price of bicylce, segmenting by frame material
# Histogram

bike_orderlines_tbl %>%
    # find all distinct model, frame_material and respective prices
    distinct(price, model, frame_material) %>%
    # nothing on y-axis because univariate visualization
    ggplot(aes(price, fill = frame_material)) +
    
    geom_histogram() +
    # note: tilde, also ncol = 1 stack facet wrap ontop (instead of side-by-side)
    facet_wrap(~ frame_material, ncol = 1) +
    # scale_fill from tidyquant package
    scale_fill_tq() +
    theme_tq()


# Density




# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions


# Goal: Unit price of models, segmenting by category 2

# Data Manipulation


# Box Plot


# Violin Plot & Jitter Plot







# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation


# Adding text to bar chart


# Filtering labels to highlight a point





