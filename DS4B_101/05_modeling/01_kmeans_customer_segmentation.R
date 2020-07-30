# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----


library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 CUSTOMER TRENDS ----
# - GOAL: Mine Customer Purchase History for similarity to other "like" customers
# - TECHNIQUES: K-Means Clustering, UMAP 2D Projection

# 1.1 Get Customer Trends ----

# PRO-TIP: When understanding customer trends, collect unique customer name, product attributes and quantity/total prices
# PRO-TIP: Convert customer trends by aggregating within customer-product groups, then Normalizing within customer groups to get % of each product purchased by customer

customer_trends_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, price, model, category_1, category_2, frame_material, quantity) %>%
    # Summarization & Group By
    # aggregating within customer-product group
    group_by(bikeshop_name, price, model, category_1, category_2, frame_material) %>%
    summarize(quantity_purchased = sum(quantity)) %>%
    ungroup() %>%

    # NORMALIZATION - customer purchase by proportion (prop_of_total)
    group_by(bikeshop_name) %>%
    mutate(prop_of_total = quantity_purchased / sum(quantity_purchased)) %>%
    ungroup()

customer_trends_tbl

# 1.2 Convert to User-Item Format (e.g. Customer-Product) ----




# 2.0 MODELING: K-MEANS CLUSTERING ----

# 2.1 Performing K-Means ----


# 2.2 Tidying a K-Means Object ----


# 2.3 How many centers (customer groups) to use? ----
 

# 2.4 Skree Plot ----




# 3.0 VISUALIZATION: UMAP ----

# 3.1 Use UMAP to get 2-D Projection ----


# 3.2 Use K-Means to Add Cluster Assignments ----


# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----




# 4.0 ANALYZE PURCHASING TRENDS ----




