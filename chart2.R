# load the libraries and dataframe
library(ggplot2)
library(tidyverse)
library(openintro)

jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# create new dataframe and with a new date column
jgdf_chart2 <- jg_df %>%
  mutate(date  = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))

jgdf_chart2$date <- as.Date(jgdf_chart2$date, format = "%Y-%m-%d")

# filter this new dataframe to English titles and find the sum checkouts for each month of each year
jgdf_chart2 <- jgdf_chart2 %>% 
  group_by(MaterialType, date) %>% 
  summarize(sum_checkouts = sum(Checkouts, na.rm = TRUE))

# create line plot of Date v. Number of Checkouts for John Green's English e-books
ggplot(data = jgdf_chart2) +
  geom_line(mapping = aes(
    x = date,
    y = sum_checkouts,
    color = MaterialType
  )) +
  labs(
    title = "Total Checkouts per Month for John Green's Works by Material Type",
    x = "Date of Checkout",
    y = "Number of Checkouts",
    color = "Material Type")
