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

bike_sales_y_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    # lubridate into ymd in order to extract later
    mutate(order_date = ymd(order_date)) %>%
    # extract year component
    mutate(year = year(order_date)) %>%
    # group_by and summarize
    group_by(year) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup()

bike_sales_y_tbl

bike_sales_m_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    # lubridate into ymd() in order to extract later
    mutate(order_date = ymd(order_date)) %>%
    mutate(
        year = year(order_date),
        month = month(order_date, label = TRUE, abbr = TRUE)
    ) %>%
    # group_by and summarize()
    group_by(year, month) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup()

bike_sales_m_tbl %>% view()

# Floor Date
# Time Series Aggregation floor_date()
# Pro Tip: Plotting requires a single time series column

bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    # lubridate
    mutate(order_date = ymd(order_date)) %>%
    mutate(year_month = floor_date(order_date, unit = "month")) %>%
    # group_by and summarize
    group_by(year_month) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    # plotting trends now that year and month are in one column
    ggplot(aes(x=year_month, y=sales)) + geom_line() + geom_smooth(method = "lm", se = FALSE)



# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----

# Find % Change Year-to-Year
# lag() useful for comparing previous values in a vector;
# lag() creates another column, n=1 offset by one-year
bike_sales_y_tbl %>%
    mutate(sales_lag_1 = lag(sales, n = 1)) %>%
    # Handle NA
    mutate(sales_lag_1 = case_when(
        # if sales_lag_1 is NA, use sales
        is.na(sales_lag_1) ~ sales,
        # otherwise, keep using sales_lag_1
        TRUE ~ sales_lag_1
    )) %>%
    # calculate diff year-to-year
    mutate(diff_1 = sales - sales_lag_1) %>%
    # calculate % difference, Pro Tip: Always divide diff by reference point - previous year
    mutate(pct_diff_1 = diff_1 / sales_lag_1) %>%
    mutate(pct_diff_1_chr = scales::percent(pct_diff_1))

### Pro Tip: never good idea to "copy-and-paste" code. Create a function!!!
### Function Basics: create a verb name, create arguments, create body

# Find % Change Month-to-Month

calculate_pct_diff <- function(data){
    
    data %>%
        mutate(sales_lag_1 = lag(sales, n = 1)) %>%
        # Handle NA
        mutate(sales_lag_1 = case_when(
            # if sales_lag_1 is NA, use sales
            is.na(sales_lag_1) ~ sales,
            # otherwise, keep using sales_lag_1
            TRUE ~ sales_lag_1
        )) %>%
        # calculate diff year-to-year
        mutate(diff_1 = sales - sales_lag_1) %>%
        # calculate % difference, Pro Tip: Always divide diff by reference point - previous year
        mutate(pct_diff_1 = diff_1 / sales_lag_1) %>%
        mutate(pct_diff_1_chr = scales::percent(pct_diff_1))
}

bike_sales_m_tbl %>%
    calculate_pct_diff() %>%
    # need to combine year and month into year_month
    mutate(
        year = as.character(year),
        month = as.character(month),
        day = 1
    ) %>%
    mutate(year_month = paste(year, month, day)) %>% 
    mutate(year_month = year_month %>% ymd()) %>%
    # plot
    ggplot(aes(x=year_month, y=pct_diff_1)) + geom_line() + geom_smooth(method = "lm", se = FALSE)



# 3.2 Difference from first observation ----

# eg. how everything compares to January of that year
bike_sales_y_tbl %>%
    mutate(sales_2011 = first(sales)) %>%
    mutate(diff_2011 = sales - sales_2011) %>%
    mutate(pct_diff_2011 = diff_2011 / sales_2011) %>%
    mutate(pct_diff_2011_chr = scales::percent(pct_diff_2011))

# measuring change from first month of each year
bike_sales_m_tbl %>%
    group_by(year) %>%
    mutate(sales_jan = first(sales)) %>% 
    mutate(
        diff_jan = sales - sales_jan,
        pct_diff_jan = diff_jan / sales_jan,
        pct_diff_jan_chr = scales::percent(pct_diff_jan)
    ) %>%
    ungroup()


# 4.0 Cumulative Calculations ----

# cumulative sales % always goes up to 100%

# cumulative sales / sales on a yearly basis
bike_sales_y_tbl %>%
    mutate(cumulative_sales = cumsum(sales)) %>%
    mutate(cumulative_sales_pct = cumulative_sales/sum(sales)) %>%
    mutate(cumulative_sales_pct_chr = scales::percent(cumulative_sales_pct))

# one mutate(), instead of three
bike_sales_y_tbl %>% 
    mutate(cumulative_sales_pct = scales::percent(cumsum(sales)/sum(sales)))

bike_sales_m_tbl %>%
    group_by(year) %>%
    mutate(cumulative_sales = cumsum(sales)) %>%
    mutate(cumulative_sales_pct = cumulative_sales/sum(sales)) %>%
    mutate(cumulative_sales_pct_chr = scales::percent(cumulative_sales_pct))

# one mutate(), instead of three
bike_sales_m_tbl %>% 
    mutate(cumulative_sales_pct = scales::percent(cumsum(sales)/sum(sales)))

# 5.0 Rolling Calculations ----

# Moving Averages, Moving Medians, Rolling Medians
# NOTE: rolling mean exposes the trend line in time series; 
# Rolling mean REDUCES effects of outliers enabling analyst to visualize a trend

# Pro Tip: You do NOT want to group_by moving averages; Moving Averages detect trends, difficult to do in a group

bike_sales_m_tbl %>%
    # rollmean() requires input necessary arguments, k, na.pad = TRUE
    # rollmean() centers value by default, we want align = "right" (not left), fill = 0
    mutate(roll_mean_3 = rollmean(sales, k = 3, na.pad = TRUE, align = "right", fill = NA)) %>%
    mutate(roll_mean_6 = rollmean(sales, k = 6, na.pad = TRUE, align = "right", fill = NA))



# 6.0 Filtering Date Ranges ---- 

# Filter between() two dates

bike_orderlines_tbl %>%
    # over write order_date to get date format (not dttm)
    mutate(order_date = ymd(order_date)) %>%
    # filter
    filter(order_date %>% between(left = ymd("2012-01-01"), right = ymd("2013-12-31"))) %>%
    tail()

# Pro Tip: filter() performs intermediate calculations. 
# As long as calculations return a TRUE / FALSE vector, you can use to to filter rows

# another way to do the same thing above
bike_orderlines_tbl %>%
    # over write order_date to get date format (not dttm)
    mutate(order_date = ymd(order_date)) %>%
    # filter by grabing year() of order_date, which includes 2012, 2013
    filter(year(order_date ) %in% c(2012, 2013))

# plot sales line with 6-month rolling average and trendline
bike_sales_m_tbl %>%
    # need to combine year and month into year_month
    mutate(
        year = as.character(year),
        month = as.character(month),
        day = 1
    ) %>%
    mutate(year_month = paste(year, month, day)) %>% 
    mutate(year_month = year_month %>% ymd()) %>%
    mutate(roll_mean_6 = rollmean(sales, k = 6, na.pad = TRUE, align = "right", fill = NA)) %>%
    ggplot(aes(x=year_month, y=sales)) + geom_line() + geom_line(aes(y=roll_mean_6), color = 'red') + geom_smooth(method = 'lm', se=FALSE)
