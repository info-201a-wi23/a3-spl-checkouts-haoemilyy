# Data Summary
library("dplyr")
library("stringr")
library("ggplot2")

spl_df <- read.csv("~/2022-2023-All-Checkouts-SPL-Data.csv/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

spl_df <- spl_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

Dickens_df <- spl_df %>% 
  filter(str_detect(Creator, "Dickens"))

Dickens_titles <- Dickens_df %>% 
  filter(Title %in% 
           c("Oliver Twist", "Great Expectations", "A Tale of Two Cities"))

Dickens_checkouts_df <- Dickens_titles %>% 
  group_by(date, Title) %>% 
  summarize(Dickens_total_checkouts = sum(Checkouts))

ggplot(data = Dickens_checkouts_df) +
  geom_line(aes(x = date,
                y = Dickens_total_checkouts,
                color = Title)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Comparing Charles Dickens' Book Title Digital Checkouts",
       x = "Date",
       y = "Total Checkouts") +
  guides(color = guide_legend(title = "Book Title"))  
