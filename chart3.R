# load the libraries and dataframe
library(ggplot2)
library(tidyverse)
library(openintro)
library(stringr)

jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# create new dataframe and with a new date column
jgdf_chart3 <- jg_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

jgdf_chart3$date <- as.Date(jgdf_chart3$date, format = "%Y-%m-%d")

# filter this new dataframe to checkouts 2020 to present and find the sum checkouts for each month of each year
jgdf_chart3 <- jgdf_chart3 %>%
  filter(str_detect(Title, "(?i)stars") & date >= "2020-01-01", na.rm = TRUE) %>%
  group_by(MaterialType, CheckoutYear) %>%
  summarize(sum_checkout = sum(Checkouts, na.rm = TRUE))

# create line plot of Date v. Number of Checkouts for John Green's English e-books
ggplot(data = jgdf_chart3) +
  geom_col(
    mapping = aes(
      x = CheckoutYear, y = sum_checkout,
      fill = MaterialType
    ),
    position = "fill"
  ) +
  scale_fill_brewer(palette = "PiYG") +
  labs(
    title = 'Total Checkouts per Month for "The Fault in Our Stars"',
    x = "Year of Checkout",
    y = "Proportion of Checkouts"
  )
