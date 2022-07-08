# Data Preparation
# This script will aggregate daily sales values of the top 100
# highest selling products overall.

# Load libraries
library(tidyverse)
library(vroom)

# Load M5 Walmart Sales Data
sales_train <- vroom("~/R/MIS480_Portfolio/data/sales_train_evaluation.csv")

# Load calendar data
calendar <- vroom("~/R/MIS480_Portfolio/data/calendar.csv")

# Select date variables
calendar_tbl <- calendar %>% select(date, d)

# Pivot daily sales values to long format
df <- sales_train %>%
    select(item_id, starts_with("d_")) %>%
    pivot_longer(cols = starts_with("d_"), names_to = "d")

# Summarize daily sales by item and day
df2 <- df %>%
    group_by(item_id, d) %>%
    summarise(value = sum(value)) %>%
    ungroup()

# Summarize total sales by item
df3 <- df2 %>%
    group_by(item_id) %>%
    summarise(value = sum(value)) %>%
    ungroup()

# Pull top 100 items with highest sales
top_100 <- df3 %>%
    arrange(desc(value)) %>%
    slice(1:100) %>%
    pull(item_id)

# Pull top 2 items lowest sales
bottom <- df3 %>%
    arrange(desc(value)) %>%
    slice_tail(n=2) %>%
    pull(item_id)

# Filter for top 100 and bottom 2 items from full data set
df4 <- df2 %>%
    filter(item_id %in% c(top_100, bottom)) %>%
    mutate(item_id = factor(item_id, levels = c(top_100, bottom))) %>%
    arrange(item_id, d)

# Join calendar dates
sales_100_tbl <- df4 %>%
    left_join(calendar_tbl) %>%
    select(-d) %>%
    arrange(item_id, date)

# Include short time series data
household_2_101_short <- sales_100_tbl %>%
    filter(as.numeric(item_id) == 102) %>%
    slice_tail(n = 90)

# Write to file
sales_100_tbl %>%
    filter(!item_id == "HOUSEHOLD_2_101") %>%
    bind_rows(household_2_101_short) %>%
    write_rds("data/walmart_item_sales.rds")
