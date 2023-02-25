# load the libraries and dataframe
library(ggplot2)
library(tidyverse)
library(openintro)

jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# create a variable of only Ebooks with English titles
engl_titles <- c("An Abundance of Katherines", "Let It Snow: Three Holiday Romances", "Looking for Alaska", "Paper Towns", "The Anthropocene Reviewed: Essays on a Human-Centered Planet", "The Fault in Our Stars", "Turtles All the Way Down", "Will Grayson, Will Grayson")

# create new dataframe and filter Title to English titles
jgdf_chart1 <- jg_df %>%
  filter(Title %in% engl_titles)

# create line plot of Year v. Number of Checkouts for John Green's English e-books
ggplot(data = jgdf_chart1) +
  geom_line(mapping = aes(
    x = CheckoutYear,
    y = Checkouts,
    color = Title
  )) +
  labs(
    title = "Number of Checkouts Overtime for John Green's Ebooks (English versions)",
    x = "Year of Ebook Checkout",
    y = "Number of Checkouts",
    color = "Title of Ebook"
  )
