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

Dickens_df$Title <- tolower(Dickens_df$Title)

Dickens_df$Title[str_detect(Dickens_df$Title, "oliver twist")] <- "Oliver Twist"
Dickens_df$Title[str_detect(Dickens_df$Title, "great expectations")] <- "Great Expectations"
Dickens_df$Title[str_detect(Dickens_df$Title, "a tale of two cities")] <- "A Tale of Two Cities"
Dickens_df$Title[str_detect(Dickens_df$Title, "a christmas carol")] <- "A Christmas Carol"
Dickens_df$Title[str_detect(Dickens_df$Title, "david copperfield")] <- "David Copperfield"
Dickens_df$Title[str_detect(Dickens_df$Title, "bleak house")] <- "Bleak House"
Dickens_df$Title[str_detect(Dickens_df$Title, "martin chuzzlewit")] <- "Martin Chuzzlewit"
Dickens_df$Title[str_detect(Dickens_df$Title, "the old curiosity shop")] <- "The Old Curiosity Shop"
Dickens_df$Title[str_detect(Dickens_df$Title, "the pickwick papers")] <- "The Pickwick Papers"
Dickens_df$Title[str_detect(Dickens_df$Title, "our mutual friend")] <- "Our Mutual Friend"
Dickens_df$Title[str_detect(Dickens_df$Title, "the life and adventures of nicholas nickleby")] <- "The Life and Adventures of Nicholas Nickleby"

Dickens_checkouts_df <- Dickens_df %>% 
  group_by(Title) %>% 
  filter(UsageClass == "Digital") %>% 
  summarize(Dickens_total_checkouts = sum(Checkouts))

bar_chart <- ggplot(Dickens_checkouts_df[tail(order(Dickens_checkouts_df$Dickens_total_checkouts), 10), ], ) +
  geom_col(mapping = aes(
    x = Dickens_total_checkouts,
    y = reorder(Title, +Dickens_total_checkouts),
    fill = Title)) +
  labs(title = "Top 10 Charles Dickens Digital Book Checkouts",
       subtitle = "From 2022 - 2023 at the Seattle Public Library",
       x = "Total Checkouts",
       y = "Book Title") +
  guides(fill="none")
