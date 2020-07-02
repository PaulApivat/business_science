# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TEXT MANIPULATION ----

library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl

bikes_tbl <- readxl::read_excel("../00_data/bike_sales/data_raw/bikes.xlsx")

bikes_tbl


# 1.0 Basics ----

# 1.1 Detection: Used with filter() ----

# Vector
# (vectorized approach)


c("Supersix Evo Black Inc.", "Supersix Evo Hi-Mod Team") %>% 
    # str_detect returns true/false boolean, good to use with filter(), case_when()
    str_detect(pattern = "Supersix") 

# Pro Tip: Any vectorized function can be used inside mutate()

# Tibble
bikes_tbl %>%
    select(model) %>%
    # add true/false boolean flag, then convert to 1 or 0
    mutate(supersix = model %>% str_detect("Supersix") %>% as.numeric()) %>%
    mutate(black = model %>% str_detect("Black") %>% as.numeric()) 
    
# Note: building flags of true/false, 1/0 is "feature engineering" with text data

# 1.2 Case & Concatenation ----


# Case
bikeshop_name <- "Ithaca Mountain Climbers"

str_to_upper(bikeshop_name)
str_to_lower(bikeshop_name)
str_to_title(bikeshop_name) # capitalizing first letter of each word

# Concatenation

# Vector
order_id <- 1
order_line <- 1

str_c("Order line: ", order_id, ".", order_line,
      " sent to Customer: ", bikeshop_name)


str_glue("Order line: {order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}")

# Tibble
bike_orderlines_tbl %>%
    select(bikeshop_name, order_id, order_line) %>%
    mutate(purchase_statement = str_glue(
        "Order line: {order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}"
    ) %>% as.character()) 



# 1.3 Separating Text: See tidyr::separate() ----

# NOTE: use separate() over str_split()

# Vector

# str_split() output is a LIST
# str_split(simplify = TRUE) output is a MATRIX
c("Road - Elite Road - Carbon", "Road - Elite Road") %>% 
  str_split(pattern = " - ", simplify = TRUE)


# Tibble
bikes_tbl %>%
  select(description) %>%
  separate(col    = description, 
           into   = c("category_1", "category_2", "frame_material"), 
           sep    = " - ",
           remove = FALSE)


# 1.4 Trimming Text ----

# get rid of space on both sides
" text with spaces  " %>% str_trim(side = "both")
# right or left
" text with spaces  " %>% str_trim(side = "right")
" text with spaces  " %>% str_trim(side = "left")


# 1.5 Replacement: Used with mutate() [and optionally case_when()] ----

# Vector
c("CAAD12", "CAAD", "CAAD8") %>% str_replace(pattern = "[0-9]", replacement = "")

c("CAAD12", "CAAD", "CAAD8") %>% str_replace_all(pattern = "[0-9]", replacement = "")


# Tibble

bikes_tbl %>%
  select(model) %>%
  # pipe from a column
  mutate(model_num_removed = model %>% 
           # take out all numbers
           str_replace_all(pattern = "[0-9]", replacement = "") %>%
           # trim empty space where numbers used to be
           str_trim())



# 1.6 Formatting Numbers ----

# scales::number() is most general


value <- 1e6
(value / 1e6) %>% scales::number(prefix = "$", suffix = "M")

value %>% scales::number(prefix = "$", big.mark = ",", suffix = "M")
value %>% scales::dollar(scale = 1/1e6, suffix = "M")

pct <- 0.15
pct %>% scales::number(scale = 100, suffix = "%")
pct %>% scales::percent()

# values


# percents



# 1.7 Formatting Column Names ----

# Replacing text in column names

bike_orderlines_tbl %>%
  set_names(names(.) %>% str_replace("_", ".") %>% str_to_upper())

# Appending text to column names
bike_orderlines_tbl %>%
  set_names(str_glue("{names(.)}_bike"))

# Appending text to specific column names
bike_orderlines_colnames_tbl <- bike_orderlines_tbl %>%
  rename_at(.vars = vars(model:frame_material),
            # all .vars gets passed to '.' below (so prod_ goes in front)
            .funs = ~ str_c("prod_", .)) %>%
  rename_at(.vars = vars(bikeshop_name:state),
            .funs = ~ str_c("cust_", .)) 

# Once Append text to Column Names, can more easily select them
bike_orderlines_colnames_tbl %>%
  select(contains("cust_"), total_price)


# 2.0 Feature Engineering with Text -----
# Investigating "model" and extracting well-formatted features

# Feature engineering is a pre-cursor to (statistical) MODELING 
# Once you develop "flags" they become variables (like gender) and you can see their effects on the outcome

# Things to look for (possible Feature Engineering):
# - Repeated Information: pieces in model name to turn into 'flags'
# - Data issues: things that need cleaning


bikes_tbl %>%
  select(model) %>%
  # fix typo on row 14 CAAD
  mutate(model = case_when(
    model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
    model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
    model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
    TRUE ~ model
  )) %>%
  
  # separate using spaces - spread features out (makes ure last column is NA)
  # these columns are simply to split model into model_base and model_tier
  # will remove these interim columns later
  separate(col    = model, 
           into   = str_c("model_", 1:7), 
           sep    = " ", 
           remove = FALSE, 
           fill   = "right") %>%
  
  # creating a base feature
  mutate(model_base = case_when(
    # Fix Supersix Evo
    str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
    # Fix Fat CAAD bikes
    str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
    # Fix Beast of the East
    str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
    # Fix Bad Habit
    str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
    # Fix Scapel 29: note model_2
    str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
    # catch all
    TRUE ~ model_1)
    ) %>%
  
  # get "Tier" feature
  mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
 
  
  # Remove unnecessary columns
  select(-matches("[0-9]")) %>%
  
  # Create Flags
  mutate(
    black = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
    hi_mod = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
    team = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
    red = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
    ultegra = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
    dura_ace = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
    disc = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
  ) %>%
  view() 
  
# Pro Tip: The "model_" gets recycled when comining with sequence 1:7 in str_c()
# useful to quickly make a character vector of repetitive column names

# OBJECTIVE ----

# Build feature set from Model, segment into BASE + TIER
# then broke out to 7 'FLAGS' column to help MODELING PROCESS
# detect what the PRICE of the model_base should be
# but if it has "TIER" in it, can help decide how much 
# to increase the VALUE of that FEATURE
