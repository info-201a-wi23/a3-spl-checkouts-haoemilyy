# chart 1 visualization

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

# filter spl_df for just Dickens' book
Dickens_df <- spl_df %>% 
  filter(str_detect(Creator, "Dickens"))

# lowercase the titles before filtering
Dickens_df$Title <- tolower(Dickens_df$Title)

# search for titles with key words then replace with proper title name
Dickens_df$Title[str_detect(Dickens_df$Title, "oliver twist")] <- "Oliver Twist"
Dickens_df$Title[str_detect(Dickens_df$Title, "great expectations")] <- "Great Expectations"
Dickens_df$Title[str_detect(Dickens_df$Title, "a tale of two cities")] <- "A Tale of Two Cities"
Dickens_df$Title[str_detect(Dickens_df$Title, "a christmas carol")] <- "A Christmas Carol"
Dickens_df$Title[str_detect(Dickens_df$Title, "david copperfield")] <- "David Copperfield"

# create new df for specific books
Dickens_titles <- Dickens_df %>% 
  filter(Title %in% 
           c("Oliver Twist", "Great Expectations", "A Tale of Two Cities", "A Christmas Carol", "David Copperfield"))

# create new df for the total checkouts summing up checkouts for each digital book by title and date
Dickens_checkouts_df <- Dickens_titles %>% 
  group_by(date, Title) %>% 
  summarize(Dickens_total_checkouts = sum(Checkouts))

# create line chart
Dickens_plot <- ggplot(data = Dickens_checkouts_df) +
  geom_line(aes(x = date,
                y = Dickens_total_checkouts,
                color = Title)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Comparing Charles Dickens' Book Title Digital Checkouts",
       subtitle = "From 2022 - 2023 at the Seattle Public Library",
       x = "Date",
       y = "Total Checkouts") +
  guides(color = guide_legend(title = "Book Title"))  

