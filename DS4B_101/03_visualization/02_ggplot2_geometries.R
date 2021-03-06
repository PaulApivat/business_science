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
bike_orderlines_tbl %>%
    # find all distinct model, frame_material and respective prices
    distinct(price, model, frame_material) %>%
    # nothing on y-axis because univariate visualization
    ggplot(aes(price, fill = frame_material)) +
    geom_density(alpha = 0.5) +
    scale_fill_tq() +
    theme_tq() +
    theme(legend.position = "bottom")




# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions

# Goal: Unit price of models, segmenting by category 2

# Data Manipulation

unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
    # can also do distinct(category_2, model, price)
    select(category_2, model, price) %>% 
    distinct() %>%
    # alternative: mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))
    mutate(category_2  = category_2 %>% as_factor() %>% fct_reorder(price))

# Box Plot
unit_price_by_cat_2_tbl %>%
    
    ggplot(aes(x=category_2, y=price)) +
    geom_boxplot() + 
    coord_flip() +
    theme_tq()



# Violin Plot & Jitter Plot

unit_price_by_cat_2_tbl %>%
    ggplot(aes(category_2, price)) + 
    # makes scatter plot with random noise
    geom_jitter(width = 0.15, color = "#2c3e50") +
    geom_violin(alpha = 0.5) +
    coord_flip() +
    theme_tq()





# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() 
    
    


# Adding text to bar chart
# NOTE: 2 ways to incorporate formatted text, new column or in geom_text

revenue_by_year_tbl %>% 
    # formatted dollar figures can be in own column
    #mutate(revenue_text = scales::dollar(revenue, scale = 1e-6, suffix = "M")) %>%
    ggplot(aes(year, revenue)) +
    geom_col(fill = "#2c3e50") +
    geom_smooth(method = 'lm', se = FALSE) + 
    geom_text(aes(label = scales::dollar(revenue, scale = 1e-6, suffix = "M")), 
              vjust = 1.5, color = 'white') +
    # bar chart gets too close to top
    # stretch out y-axis
    geom_label(aes(label = "Major Demand This Year"), 
               vjust = -0.5,
               size = 5,
               fill = "#1f78b4",
               color = "white",
               fontface = "bold",
               # can use data argument to pull in text from the dataset
               data = revenue_by_year_tbl %>%
                   # filter by two or more years
                   filter(year %in% c(2013))) +
    expand_limits(y = 2e7) +
    
    theme_tq()




# Filtering labels to highlight a point





