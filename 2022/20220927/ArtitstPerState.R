library(tidyverse)
library(ggplot2)
library(ggtext) 

# load data
tuesdata <- tidytuesdayR::tt_load(2022, week=39)
artists <- tuesdata$artists

# replace NAs with 0
artists <- artists %>% 
  mutate(artists_n = coalesce(artists_n, 0),
  artists_share = coalesce(artists_share, 0))

# aggreagate to get sum of workers and sum of artists per state
worker_count <- aggregate(artists$all_workers_n, by=list(Category=artists$state), FUN=sum)
worker_count <- worker_count %>% rename(worker_sum = x, state = Category)
artists_count <- aggregate(artists$artists_n, by=list(Category=artists$state), FUN=sum)
artists_count <- artists_count %>% rename(artists_sum = x, state = Category)

# merge DataFrames
artists_clean <- merge(worker_count, artists_count)
artists_clean$artists_share <- artists_clean$artists_sum * 100 / artists_clean$worker_sum

# Function to plot
ggplot(data=artists_ordered) + 
  geom_col(aes(x = reorder(state, +artists_share), y = artists_share), fill="steelblue") +
  coord_flip() +
  labs(
    title = "Share of <span style='color:#0072B2;'>Artists</span> in every State ", 
    subtitle = "Artists per State compared to the whole workforce",
    caption = "TidyTuesday Week 39 | Data: arts.gov | Graphic: M.Müller",
    y = "Artists share in %",
    x ="State" 
  ) +
  theme(
    plot.title = element_markdown(lineheight = 1.1, face="bold", hjust=0.5),
    plot.subtitle = element_text(hjust=0.5),
    plot.caption = element_text(hjust=0.5),
)
