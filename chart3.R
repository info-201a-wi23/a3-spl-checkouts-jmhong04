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

# filter this new dataframe to "The Fault in Our Stars" checkouts from 2020 to 2023 and find the sum checkouts for each year
jgdf_chart3 <- jgdf_chart3 %>%
  filter(str_detect(Title, "(?i)stars") & date >= "2020-01-01", na.rm = TRUE) %>%
  group_by(CheckoutYear, MaterialType) %>%
  summarize(sum_checkout = sum(Checkouts, na.rm = TRUE))

# create a stacked bar chart of Year v. Proportion of Checkouts for John Green's English works
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
    title = 'Annual Proportion of Checkouts by Material Type for "The Fault in Our Stars"',
    x = "Year of Checkout",
    y = "Proportion of Checkouts"
  )
