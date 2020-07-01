# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Date & Lubridate Basics ----

# note: differences between Character, Date and Date-Time
# IMPORTANT: We need to convert to date class for lubridate functionality

# 1.1 Character vs Date/Datetime

order_date_tbl <- bike_orderlines_tbl %>% 
    select(order_date)

order_date_tbl %>%
    # pull out vector of content
    pull(order_date) %>%
    class()

# 1.2 Date Classes

# Convert dtmm to chr and date, then back to dttm
order_date_tbl %>%
    mutate(order_date_chr = as.character(order_date)) %>%
    # add time formatted as chr
    mutate(order_date_chr2 = order_date_chr %>% str_c(" 00:00:00")) %>%
    # use ymd() and ymd_hms() to convert character to date/datetime
    mutate(order_date_date = order_date_chr %>% ymd()) %>%
    mutate(order_date_dttm = order_date_chr2 %>% ymd_hms())



# 1.3 Lubridate Functions

# Conversion

# character to date/date time class

# June 1st, 2018 --> date converter output always YYYY-MM-DD
"06/01/18" %>% mdy() %>% class()

# Month-Day-Year + Hour-Minute-Second converts to date-time (POSIXct, POSIXt)
"06.01.18 12:30:15" %>% mdy_hms() %>% class()

"January 1, 1985" %>% mdy()

# other conversion functions
ymd()
dmy()

# Extractor



# Helpers



# Periods & Durations - Add/subract time to/from a date



# Intervals - Calculate time-based distance 



# 2.0 Time-Based Data Grouping ----







# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----





# 3.2 Difference from first observation ----





# 4.0 Cumulative Calculations ----




# 5.0 Rolling Calculations ----



# 6.0 Filtering Date Ranges ---- 



