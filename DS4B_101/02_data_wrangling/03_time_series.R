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

# all conversions to YYYY-MM-DD
"Jan, 23 1985" %>% mdy()
"Jan 23, 1985" %>% mdy()
"25 Apr, 1981" %>% dmy()
"1st Feb, 1999" %>% dmy()
"1983 Sep, 29" %>% ymd()  # Had to be Sep, not Sept
"1983, Sep 29" %>% ymd()

"3rd Mar, 1989" %>% dmy()
"5 Apr, 1990" %>% dmy()
"1999.2.17" %>% ymd()

"08~14~2019" %>% mdy() 
"08*14*2019" %>% mdy()
"08*14/2019" %>% mdy()
"Aug 14th 2019" %>% mdy()
"Aug,14th 2019" %>% mdy()
"Aug,14th,2019" %>% mdy()
"Aug14th2019" %>% mdy()
"August 14th 2019" %>% mdy()
"August14th2019" %>% mdy()
"August 14th2019" %>% mdy()

"Aug 14, 2019" %>% mdy()

# other conversion functions
ymd()
dmy()

# Extractor

"2011-Jan-01" %>% ymd() %>% year()

# label changes from either numeric or factor
"2011-Jan-01" %>% ymd() %>% month(label = TRUE, abbr = FALSE)

"2011-1-01" %>% ymd() %>% month(label = TRUE, abbr = FALSE)

"2011-01-01" %>% ymd() %>% wday() #7
"2011-01-01" %>% ymd() %>% wday(label = TRUE) # Sat
"2011-01-01" %>% ymd() %>% wday(label = TRUE, abbr = FALSE) # Saturday

"2011-01-21" %>% ymd() %>% day()

# Helpers

# "2020-07-01 14:07:47 +07"
now()
# "2020-07-01"
today() %>% class()


# Periods & Durations - Add/subract time to/from a date

# note: Periods account for daylight savings, time and leap year
# note: Duration are just physical time spans without irregularities

today() + days(12)

# ddays() - add or subtract a duration in days
today() + ddays(12)

# no leap years in Asia?
today() + years(4) # Period
today() + dyears(4) # Duration 

# Intervals - Calculate time-based distance 

# Interval Objects 2 snapshots Start -- End
# 2020-07-01 UTC--2020-07-13 UTC
i <- interval(today(), today() + ddays(12))

# always divide interval with Duration (ddays, dminutes, dseconds)
i / ddays(1) # interval / days = how many days within interval
i / dminutes(1)  # interval / minutes = how many minutes within interval
i / dseconds(1)

# Find interval between order_date and today
order_date_tbl %>%
    mutate(today = today()) %>%
    mutate(diff_days = interval(order_date, today) / ddays(1))

# 2.0 Time-Based Data Grouping ----







# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----





# 3.2 Difference from first observation ----





# 4.0 Cumulative Calculations ----




# 5.0 Rolling Calculations ----



# 6.0 Filtering Date Ranges ---- 



