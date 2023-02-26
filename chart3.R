# load the libraries and dataframe
library(ggplot2)
library(tidyverse)
library(openintro)
library("scales")
library("dplyr")
library("ggplot2")
library(stringr)

jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# create new dataframe and with a new date column
jgdf_chart3 <- jg_df %>%
  mutate(date  = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))

jgdf_chart3$date <- as.Date(jgdf_chart3$date, format = "%Y-%m-%d")

# filter this new dataframe to English titles and find the sum checkouts for each month of each year
jgdf_chart3 <- jgdf_chart3 %>% 
  filter(str_detect(Title, "(?i)stars") | str_detect(Title, "(?i)estrella"), na.rm = TRUE)

# create line plot of Date v. Number of Checkouts for John Green's English e-books
ggplot(data = jgdf_chart3) +
  geom_col(aes(x = CheckoutYear, y = Checkouts,
               fill = MaterialType)) + 
  scale_x_continuous(breaks = seq(2006, 2023, 1)) + 
  labs(
    title = 'Total Checkouts per Year for "The Fault in Our Stars"',
    x = "Year of Checkout",
    y = "Number of Checkouts",
    fill = "Material Type")


