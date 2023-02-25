# Chart 2 Visualization

library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")

# read in data
spl_df <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Convert date using as.Date
spl_df <- spl_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

# filter spl_df into just Dickens
Dickens_df <- spl_df %>% 
  filter(str_detect(Creator, "Dickens"))

# sum up checkouts by each date and material type
Dickens_type_checkouts_df <- Dickens_df %>% 
  group_by(date, MaterialType) %>% 
  summarize(Dickens_total_checkouts = sum(Checkouts))

# create line plot
material_plot <- ggplot(data = Dickens_type_checkouts_df) +
  geom_line(aes(x = date,
                y = Dickens_total_checkouts,
                color = MaterialType)) +
  scale_color_brewer(palette = "Set2") +
    labs(title = "Comparing the Material Type of Checkout for Charles Dicken's Books",
         subtitle = "From 2022 - 2023 at the Seattle Public Library",
       x = "Date",
       y = "Total Checkouts") +
  guides(color = guide_legend(title = "Material Type"))

