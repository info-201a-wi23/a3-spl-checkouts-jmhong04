library(openintro)
library(tidyverse)

# load the data
jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# What is the title of the item written by John Green with the highest number of checkouts?
item_most_checkouts <- jg_df %>% 
  group_by(Title) %>% 
  summarize(sum_checkouts_by_title = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkouts_by_title == max(sum_checkouts_by_title, na.rm = TRUE)) %>% 
  pull(Title)

# What is the group of subjects that has the highest number of checkouts?
subjects_most_checkouts <- jg_df %>% 
  group_by(Subjects) %>% 
  summarize(sum_checkouts_by_subjects = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkouts_by_subjects == max(sum_checkouts_by_subjects, na.rm = TRUE)) %>% 
  pull(Subjects)

# What year and month had the most amount of checkouts for John Green's works?
date_most_checkouts <- jg_df %>% 
  mutate(checkout_date = paste0(CheckoutMonth, "/", CheckoutYear)) %>% 
  group_by(checkout_date) %>% 
  summarize(sum_checkouts_by_date = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkouts_by_date == max(sum_checkouts_by_date, na.rm = TRUE)) %>% 
  pull(checkout_date)

# What month of the year does John Green's items experience the most checkouts?
month_highest_checkouts <- jg_df %>% 
  mutate(month_name = month.name[CheckoutMonth]) %>% 
  group_by(month_name) %>% 
  summarize(sum_checkouts_by_month = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkouts_by_month == max(sum_checkouts_by_month, na.rm = TRUE)) %>% 
  pull(month_name)

# Which publishing company is associated with the most amount of checkouts?
publish_cmpny_most_checkouts <- jg_df %>% 
  group_by(Publisher) %>% 
  summarize(sum_checkouts_by_publisher = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkouts_by_publisher == max(sum_checkouts_by_publisher, na.rm = TRUE)) %>% 
  pull(Publisher)

# What type of material is least checked out for items written by John Green?
medium_least_checkouts <- jg_df %>% 
  group_by(MaterialType) %>% 
  summarize(sum_checkouts_by_medium = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkouts_by_medium == min(sum_checkouts_by_medium, na.rm = TRUE)) %>% 
  pull(MaterialType)

