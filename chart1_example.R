
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

Dickens_titles <- Dickens_df %>% 
  filter(Title %in% 
           c("Oliver Twist", "Great Expectations", "A Tale of Two Cities", "A Christmas Carol", "David Copperfield"))

Dickens_checkouts_df <- Dickens_titles %>% 
  group_by(date, Title) %>% 
  summarize(Dickens_total_checkouts = sum(Checkouts))

Dickens_plot <- ggplot(data = Dickens_checkouts_df) +
  geom_line(aes(x = date,
                y = Dickens_total_checkouts,
                color = Title)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Comparing Charles Dickens' Book Title Digital Checkouts",
       x = "Date",
       y = "Total Checkouts") +
  guides(color = guide_legend(title = "Book Title"))  

