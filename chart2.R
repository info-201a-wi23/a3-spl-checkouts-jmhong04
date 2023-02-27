# load the libraries and dataframe
library(ggplot2)
library(tidyverse)
library(openintro)

jg_df <- read.csv("John_Green_Checkouts.csv", stringsAsFactors = FALSE)

# group the number of checkouts for each year and for each material type in a new dataframe
# find the sum checkouts for each year
jgdf_chart2 <- jg_df %>%
  filter(MaterialType != "REGPRINT") %>%
  group_by(CheckoutYear, MaterialType) %>%
  summarize(sum_checkouts = sum(Checkouts, na.rm = TRUE))

# create line plot of Year v. Number of Checkouts for John Green's works per Material Type
ggplot(data = jgdf_chart2) +
  geom_line(mapping = aes(
    x = CheckoutYear,
    y = sum_checkouts,
    color = MaterialType
  )) +
  labs(
    title = "Total Annual Checkouts of John Green's Works in the SPL per Material Type",
    x = "Year of Checkout",
    y = "Number of Checkouts",
    color = "Material Type"
  ) +
  scale_x_continuous(breaks = seq(2005, 2023, 2))
