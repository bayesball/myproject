library(tidyverse)
sc <- read_csv("data/sc2017ip.csv")
sc %>% filter(events == "home_run") %>% 
  mutate(Count = paste(balls, strikes, sep="-")) ->
  sc_hr

ggplot(sc_hr, aes(Count)) +
  geom_bar()
