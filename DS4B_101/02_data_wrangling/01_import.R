# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# IMPORTING DATA INTO R ----


# 1.0 Load libraries ----

# Contians readr
library(tidyverse)   

# Excel Connection
library(readxl)
library(writexl)

# Database Connection
library(odbc)
library(RSQLite)



# 2.0 readr ----

# 2.1 CSV ----
bike_orders_csv_tbl <- readr::read_csv("../00_data/bike_sales/data_wrangled/bike_orderlines.csv")

readr::problems(bike_orders_csv_tbl)

# filter rows by slice() function
bike_orders_csv_tbl %>%
    slice(7916)

# converting order_id column from integer to double using col_double() function
read_csv("../00_data/bike_sales/data_wrangled/bike_orderlines.csv",
         col_types = cols(
             order_id = col_double()
         )) %>%
    slice(7916)

# 2.2 RDS ----

bike_orders_rds_tbl <- readr::read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orders_rds_tbl %>% 
    slice(7916)

# 3.0 Excel ----

bike_orders_excel_tbl <- readxl::read_excel("../00_data/bike_sales/data_wrangled/bike_orderlines.xlsx", sheet = "Sheet1")

# know what sheets are in excel file and pick specific sheets
readxl::excel_sheets("../00_data/bike_sales/data_wrangled/bike_orderlines.xlsx")

bike_orders_excel_tbl


# 4.0 Databases  ----

# establish a connection object
con <- RSQLite::dbConnect(drv = SQLite(), dbname = "../00_data/chinook/Chinook_Sqlite.sqlite")

#return names of tables in the database
dbListTables(con)

# examine a table from a database (NOT pulled into memory)
tbl(con, "Album")

# pull table from data base into local memory
tbl(con, "Album") %>%
    collect()

# save so it shows up in global enviroment
album_tbl <- tbl(con, "Album") %>% collect()

# how to disconnect from a database (best practice)
artist_tbl <- tbl(con, "Artist") %>% collect()

dbDisconnect(con)

