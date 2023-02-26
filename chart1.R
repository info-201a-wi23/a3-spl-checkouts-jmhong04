# load the libraries and dataframe
library(ggplot2)
library(tidyverse)
library(openintro)

jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# create a variable of only Ebooks with English titles
engl_titles <- c("An Abundance of Katherines", "Let It Snow: Three Holiday Romances", "Looking for Alaska", "Paper Towns", "The Anthropocene Reviewed: Essays on a Human-Centered Planet", "The Fault in Our Stars", "Turtles All the Way Down", "Will Grayson, Will Grayson")

# create new dataframe and with a new date column
jgdf_chart1 <- jg_df %>%
  mutate(date  = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))

jgdf_chart1$date <- as.Date(jgdf_chart1$date, format = "%Y-%m-%d")

# filter this new dataframe to English titles and find the sum checkouts for each month of each year
jgdf_chart1 <- jgdf_chart1 %>% 
  filter(Title %in% engl_titles, na.rm = TRUE) %>% 
  group_by(Title, date) %>% 
  summarize(sum_checkouts = sum(Checkouts, na.rm = TRUE))

# create line plot of Date v. Number of Checkouts for John Green's English e-books
ggplot(data = jgdf_chart1) +
  geom_line(mapping = aes(
    x = date,
    y = sum_checkouts,
    color = Title
  )) +
  labs(
    title = "Total Checkouts per Month for John Green's Ebooks (English versions)",
    x = "Date of Ebook Checkout",
    y = "Number of Checkouts",
    color = "Title of Ebook")
