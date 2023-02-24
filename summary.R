# Summary
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

# Total Checkouts
total_spl_checkouts <- spl_df %>% 
  summarize(total_checkout = sum(Checkouts)) %>% 
  pull(total_checkout)

# Total Charles Dickens Checkouts
total_dickens_checkouts <- Dickens_df %>% 
  summarize(total_checkout = sum(Checkouts)) %>% 
  pull(total_checkout)

# Proportion Dickens / SPL
proportion <- total_dickens_checkouts / total_spl_checkouts
proportion <- round(proportion, digits = 6)

# Which digital book had the most checkouts and in which year and month?
digital_most_title_monthly <- Dickens_df %>% 
  filter(UsageClass == "Digital", na.rm = TRUE) %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(Title)

digital_most_month_checkout <- Dickens_df %>% 
  filter(UsageClass == "Digital", na.rm = TRUE) %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(date)

digital_most_monthly_checkout <- Dickens_df %>% 
  filter(UsageClass == "Digital", na.rm = TRUE) %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(Checkouts)

# which physical book had the most checkouts and when?
physical_most_title_checkout <- Dickens_df %>% 
  filter(UsageClass == "Physical", na.rm = TRUE) %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(Title)

physical_most_month_checkout <- Dickens_df %>% 
  filter(UsageClass == "Physical", na.rm = TRUE) %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(date)

physical_most_monthly_checkout <- Dickens_df %>% 
  filter(UsageClass == "Physical", na.rm = TRUE) %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(Checkouts)

# Which digital book has the least amount of checkouts, and which digital book has the most amount of checkouts?
# most
digital_most_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Digital", na.rm = TRUE) %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts)) %>% 
  pull(total_checkouts)

digital_title_most_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Digital", na.rm = TRUE) %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts)) %>% 
  pull(Title)

# least
digital_least_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Digital", na.rm = TRUE) %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == min(total_checkouts)) %>% 
  pull(total_checkouts)

digital_title_least_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Digital", na.rm = TRUE) %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == min(total_checkouts)) %>% 
  pull(Title)

# Which physical book has the most amount of checkouts, and which physical book has the least amount of checkouts?
# most
physical_most_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Physical", na.rm = TRUE) %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts)) %>% 
  pull(total_checkouts)

physical_title_most_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Physical", na.rm = TRUE) %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == max(total_checkouts)) %>% 
  pull(Title)


# least
physical_least_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Physical", na.rm = TRUE) %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == min(total_checkouts)) %>% 
  pull(total_checkouts)

physical_title_least_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Physical", na.rm = TRUE) %>% 
  group_by(Title) %>% 
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts == min(total_checkouts)) %>% 
  pull(Title)

# How many total digital Charles Dickens books were checked out from 2022-23?
digital_book_total_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Digital", na.rm = TRUE) %>% 
  summarize(total_digital_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(total_digital_checkouts)

# How many total physical Charles Dickens books were checked out from 2022-23?
physical_book_total_checkouts <- Dickens_df %>% 
  filter(UsageClass == "Physical", na.rm = TRUE) %>% 
  summarize(total_physical_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(total_physical_checkouts)

