# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# FORMATTING GGPLOTS ----

# Libraries & Data ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# Data Manipulation

sales_by_year_category_2_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(category_2, year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(category_2 = fct_reorder2(category_2, year, revenue))

sales_by_year_category_2_tbl

sales_by_year_category_2_tbl %>%
    mutate(category_2_num = as.numeric(category_2)) %>%
    arrange(category_2_num)

# 1.0 Working with Colors ----

# 1.1 Color Conversion ----

# Named Colors
colors()

# can use RBG or HEX or Color or even Color Brewer
sales_by_year_category_2_tbl %>%
    ggplot(aes(x=year, y=revenue)) +
    #geom_col(fill="springgreen3")
    
    # grab specific RGB
    #geom_col(fill=rgb(44, 62, 80, maxColorValue = 255))
    
    # grab Blues palette, pick 6th color
    #geom_col(fill=RColorBrewer::brewer.pal(n = 9, name = 'Blues')[6])

    # grab specific color from Viridis palette
    geom_col(fill=viridisLite::viridis(n = 30)[15])

# To RGB
# Red Green Blue

# turn 'color' into RGB
col2rgb("springgreen3")
col2rgb("dodgerblue")

# turn hex into RGB (tidyquant blue)
col2rgb("#2C3E50")

# To HEX
# turn RGB into HEX (tidyquant blue)
rgb(44, 62, 80, maxColorValue = 255)


# 1.2 Color Palettes ----

# tidyquant
tidyquant::palette_light()

# character vector
palette_light()[2]

# convert palette light to rgb
palette_light()[7] %>% col2rgb()

# Brewer
# primarily for discrete data

# display all Brewer colors
RColorBrewer::display.brewer.all()

# get information on all Brewer color palettes
# (divergent, qualitative, sequential etc)
RColorBrewer::brewer.pal.info

# will provide HEX for all palettes in Brewer
RColorBrewer::brewer.pal(n = 9, name = 'Blues')

# turn individual HEX into RGB, for Brewer
RColorBrewer::brewer.pal(n = 9, name = 'Blues')[2] %>% col2rgb()

# Viridis

# get a number of viridis colors
viridisLite::viridis(n = 30)




# 2.0 Aesthetic Mappings ----

# 2.1 Color  -----
# - Used with line and points, Outlines of rectangular objects

sales_by_year_category_2_tbl %>%
    # asthetic mapping done globally 
    ggplot(aes(x=year, y=revenue, color=category_2)) +
    geom_line() +
    geom_point()


sales_by_year_category_2_tbl %>%
    ggplot(aes(x=year, y=revenue)) +
    # asthetic mapping done locally
    # allows for customization
    geom_line(aes(color = category_2), size = 1) +
    # not need to use aes() unless you're mapping to a column (in the data)
    geom_point(color = "dodgerblue", size = 5)

# NOTE: "Tidy Data" is the same in ggplot2 and modeling
# There's one column of interest (target column) and
# there's another column to describe the target



# Usine colors as aesthetics




# 2.2 Fill  -----
# - Used with fill of rectangular objects 

sales_by_year_category_2_tbl %>%
    # aes mapping at global level
    ggplot(aes(x=year, y=revenue, fill=category_2)) +
    geom_col()


sales_by_year_category_2_tbl %>%
    ggplot(aes(x=year, y=revenue)) +
    # aes mapping at local level
    geom_col(aes(fill=category_2))


# 2.3 Size ----
# - Used with points

# run aes() mapping locally in geom_line and geom_point
# only point varies by revenue
sales_by_year_category_2_tbl %>%
    ggplot(aes(x=year, y=revenue)) +
    geom_line(aes(color = category_2), size = 1) +
    geom_point(aes(size = revenue), shape = 'triangle')

# only need to be in aes() if tied to a column
sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue)) +
    # - NO aesthetic mapping - geom_line(size = 4)
    geom_line(aes(size = year)) + 
    geom_point(aes(color = revenue), size = 2)



# run aes() mapping globally in ggplot
# ggplot() aes() mapping can be fill, color or size
# both point and line vary by revenue (because its globally)
sales_by_year_category_2_tbl %>%
    ggplot(aes(x=year, y=revenue, size = revenue)) +
    geom_line(aes(color = category_2)) +
    geom_point()

?geom_point


# 3.0 Faceting ----
# - Great way to tease out variation by category

# Goal: Sales annual sales by category 2

sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, color = category_2)) +
    geom_line(color = 'black') +
    geom_smooth(method = 'lm', se = FALSE) +
    # alternative ways to do facet_wrap
    #facet_wrap(c('category_2'))
    # ncol number of column; free_y frees up y-axis
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    # use expand_limits if you're using free_y
    expand_limits(y = 0)


# 4.0 Position Adjustments (Stack & Dodge) ----

# Stacked Bars & Side-By-Side Bars

sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, fill = category_2)) +
    # default: position = 'stack'
    # geom_col(position = 'stack')
    
    # position = 'dodge', side-by-side, allows further customization
    geom_col(position = position_dodge(width = 0.9), color = 'white')

# Stacked Area

sales_by_year_category_2_tbl %>%
    ggplot(aes(x=year, y=revenue, fill=category_2)) +
    geom_area(color = 'black')



# 5.0 Scales (Colors, Fills, Axis) ----

# 5.1 Plot Starting Points ----
# - Continuous (e.g. Revenue): Changes color via gradient palette
# - Categorical (e.g. ): Changes color via discrete palette

# Plot 1: Faceted Plot, Color = Continuous Scale

# note: revenue is NOT a grouping variable 
# but can facet_wrap by category_2 for  grouping
g_facet_continuous <- sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, color = revenue)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    facet_wrap(~ category_2, scales = "free_y") +
    # when doing "free_y", expand_limits allows things to be plotted
    # in relation to 0
    expand_limits(y = 0) +
    theme_minimal()

g_facet_continuous


# Plot 2: Faceted Plot, Color = Discrete Scale

g_facet_discrete <- sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, color = category_2)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    facet_wrap(~ category_2, scales = "free_y") +
    # when doing "free_y", expand_limits allows things to be plotted
    # in relation to 0
    expand_limits(y = 0) +
    theme_minimal()

g_facet_discrete

# Plot 3: Stacked Area Plot

g_area_discrete <- sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, fill = category_2)) +
    geom_area(color = 'black') +
    theme_minimal()

g_area_discrete

# 5.2 Scale Colors & Fills ----
# - Awesome way to show variation by groups (discrete) and by values (continuous)

# Color by Revenue (Continuous Scale)

g_facet_continuous +
    
    #scale_color_continuous(
    #    low = "black",
    #    high = "cornflowerblue"
    #)
    
    # direction = -1 reverses the color
    # option = 'A' is magma (try B-E)
    scale_color_viridis_c(option = "B", direction = -1)


# Color by Category 2 (Discrete Scale)

g_facet_discrete +
    # default palette = 'Blues'
    scale_color_brewer(palette = 'Blues') +
    theme_dark()


# Fill by Category 2

 


# 5.3 Axis Scales ----





# 6.0 Labels ----




# 7.0 Themes  ----




# 8.0 Putting It All Together ----







