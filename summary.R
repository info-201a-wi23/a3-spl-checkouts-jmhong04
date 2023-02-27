library(openintro)
library(tidyverse)

# load the data
jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# In the SPL, what item written by John Green has the highest number of checkouts?
item_most_checkouts <- jg_df %>%
  group_by(Title) %>%
  summarize(sum_checkouts_by_title = sum(Checkouts, na.rm = TRUE)) %>%
  filter(sum_checkouts_by_title == max(sum_checkouts_by_title, na.rm = TRUE)) %>%
  mutate(
    Title = str_remove(Title, "\\/.*"),
    Title = str_to_title(Title)
  ) %>%
  pull(Title)

# What year and month had the most amount of checkouts for John Green's works?
date_most_checkouts <- jg_df %>%
  mutate(name_date = paste0(month.name[CheckoutMonth], " ", CheckoutYear)) %>%
  group_by(name_date) %>%
  summarize(sum_checkouts_by_date = sum(Checkouts, na.rm = TRUE)) %>%
  filter(sum_checkouts_by_date == max(sum_checkouts_by_date, na.rm = TRUE)) %>%
  pull(name_date)

# What month of the year does John Green's items experience the most checkouts?
month_highest_checkouts <- jg_df %>%
  mutate(month_name = month.name[CheckoutMonth]) %>%
  group_by(month_name) %>%
  summarize(sum_checkouts_by_month = sum(Checkouts, na.rm = TRUE)) %>%
  filter(sum_checkouts_by_month == max(sum_checkouts_by_month, na.rm = TRUE)) %>%
  pull(month_name)

# What type of material is most checked out for items written by John Green?
medium_most_checkouts <- jg_df %>%
  mutate(MaterialType = tolower(MaterialType)) %>%
  group_by(MaterialType) %>%
  summarize(sum_checkouts_by_medium = sum(Checkouts, na.rm = TRUE)) %>%
  filter(sum_checkouts_by_medium == max(sum_checkouts_by_medium, na.rm = TRUE)) %>%
  pull(MaterialType)

# Does the answer to the first question change when only looking at books?
# a.k.a. Among all books, which book has the most amount of checkouts?
book_most_checkouts <- jg_df %>%
  filter(MaterialType == "BOOK", na.rm = TRUE) %>%
  mutate(
    Title = str_remove(Title, "\\/.*"),
    Title = str_remove(Title, "\\[.*"),
    Title = str_to_title(Title)
  ) %>%
  group_by(Title) %>%
  summarize(sum_checkouts_by_book = sum(Checkouts, na.rm = TRUE)) %>%
  filter(sum_checkouts_by_book == max(sum_checkouts_by_book, na.rm = TRUE)) %>%
  pull(Title)
