# load the libraries and dataframe
library(ggplot2)
library(tidyverse)
library(openintro)

jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# filter a new dataframe to books by John Green and find the sum checkouts for each year for each book
jgdf_chart1 <- jg_df %>%
  filter(MaterialType == "BOOK", na.rm = TRUE) %>%
  mutate(
    Title = str_remove(Title, "\\/.*"),
    Title = str_remove(Title, "\\[.*"),
    Title = str_to_title(Title)
  ) %>%
  group_by(CheckoutYear, Title) %>%
  summarize(sum_checkouts_by_book = sum(Checkouts, na.rm = TRUE))

# create line plot of Year v. Number of Checkouts for John Green's Books
ggplot(data = jgdf_chart1) +
  geom_line(mapping = aes(
    x = CheckoutYear,
    y = sum_checkouts_by_book,
    color = Title
  )) +
  labs(
    title = "Total Checkouts per Year for Each John Green Book in the SPL",
    x = "Year of Book Checkout",
    y = "Number of Checkouts",
    color = "Title of Book"
  ) +
  scale_x_continuous(breaks = seq(2005, 2023, 2))
