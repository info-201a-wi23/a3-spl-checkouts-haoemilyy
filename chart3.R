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

Dickens_df$Title[str_detect(Dickens_df$Title, "Oliver Twist")] <- "Oliver Twist"
Dickens_df$Title[str_detect(Dickens_df$Title, "Great Expectations")] <- "Great Expectations"
Dickens_df$Title[str_detect(Dickens_df$Title, "A Tale of Two Cities")] <- "A Tale of Two Cities"
Dickens_df$Title[str_detect(Dickens_df$Title, "A Christmas Carol")] <- "A Christmas Carol"
Dickens_df$Title[str_detect(Dickens_df$Title, "David Copperfield")] <- "David Copperfield"
Dickens_df$Title[str_detect(Dickens_df$Title, "Bleak House")] <- "Bleak House"
Dickens_df$Title[str_detect(Dickens_df$Title, "Martin Chuzzlewit")] <- "Martin Chuzzlewit"
Dickens_df$Title[str_detect(Dickens_df$Title, "The Old Curiosity Shop")] <- "The Old Curiosity Shop"
Dickens_df$Title[str_detect(Dickens_df$Title, "The Pickwick Papers")] <- "The Pickwick Papers"
Dickens_df$Title[str_detect(Dickens_df$Title, "Our Mutual Friend")] <- "Our Mutual Friend"
Dickens_df$Title[str_detect(Dickens_df$Title, "The Life and Adventures of Nicholas Nickleby")] <- "The Life and Adventures of Nicholas Nickleby"

Dickens_checkouts_df <- Dickens_df %>% 
  group_by(Title) %>% 
  filter(UsageClass == "Digital") %>% 
  summarize(Dickens_total_checkouts = sum(Checkouts))

bar_chart <- ggplot(Dickens_checkouts_df[tail(order(Dickens_checkouts_df$Dickens_total_checkouts), 10), ], ) +
  geom_col(mapping = aes(
    x = Dickens_total_checkouts,
    y = reorder(Title, +Dickens_total_checkouts),
    fill = Title)) +
  labs(title = "Top 10 Charles Dickens Digital Book Checkouts 2022-23",
       x = "Total Checkouts",
       y = "Book Title") +
  guides(fill="none")
