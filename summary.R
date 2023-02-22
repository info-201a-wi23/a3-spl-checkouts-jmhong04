library(openintro)
library(tidyverse)

jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# **checkout year**, **number of checkouts**, **subjects of the book**, **publication year**, and **publishing company**

# What is the book with the highest number of checkouts?
book_most_checkouts <- jg_df %>% 
  group_by(Title) %>% 
  summarize(sum_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkouts == max(sum_checkouts, na.rm = TRUE)) %>% 
  pull(Title)

# What is the genre with the highest total of book checkouts?
genre_most_checkouts <- jg_df %>% 
  group_by(Subjects) %>% 
  summarize(sum_genre_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_genre_checkouts == max(sum_genre_checkouts, na.rm = TRUE)) %>% 
  pull(Subjects)

# What year and month had the most amount of checkouts for John Green's books?
date_most_checkouts <- jg_df %>% 
  mutate(checkout_date = paste0(CheckoutMonth, "/", CheckoutYear)) %>% 
  group_by(checkout_date) %>% 
  summarize(sum_checkout_date = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkout_date == max(sum_checkout_date, na.rm = TRUE)) %>% 
  pull(checkout_date)

# Which publishing company had the most amount of checkouts?
publish_cmpny_most_checkout <- jg_df %>% 
  group_by(Publisher) %>% 
  summarize(sum_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_checkouts == max(sum_checkouts, na.rm = TRUE)) %>% 
  pull(Publisher)

# What type of medium is most popular for books written by John Green?
most_medium <- jg_df %>% 
  group_by(MaterialType) %>% 
  summarize(sum_medium = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(sum_medium == max(sum_medium, na.rm = TRUE)) %>% 
  pull(MaterialType)

