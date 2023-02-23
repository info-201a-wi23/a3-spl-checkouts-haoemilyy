# Chart 3 Visualization

library("dplyr")
library("stringr")
library("ggplot2")
library("plotly")

spl_df <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

Dickens_df <- spl_df %>% 
  filter(str_detect(Creator, "Dickens"))

Dickens_checkouts_df <- Dickens_df %>% 
  group_by(Title) %>% 
  filter(UsageClass == "Digital") %>% 
  summarize(Dickens_total_checkouts = sum(Checkouts))

ggplot(Dickens_checkouts_df[tail(order(Dickens_checkouts_df$Dickens_total_checkouts), 10), ], ) + 
  aes(x = Title, y = Dickens_total_checkouts, fill = Title) + geom_col() 

bar_chart <- ggplot(Dickens_checkouts_df[tail(order(Dickens_checkouts_df$Dickens_total_checkouts), 10), ], ) +
  geom_col(mapping = aes(
    x = Dickens_total_checkouts,
    y = reorder(Title, +Dickens_total_checkouts),
    fill = Title)) +
  labs(title = "Top 10 Charles Dickens Digital Book Checkouts",
       x = "Total Checkouts",
       y = "Book Title") +
  guides(fill="none")
