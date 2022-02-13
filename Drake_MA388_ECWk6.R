library(tidyverse)
library(ggplot2)
library(plotly)
library(gapminder)

airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

StateFunction <- function(data){
  data %>%
    group_by(state) %>%
    mutate(AVG = mean(number_of_aerial_victory_credits)) %>%
    select(state,number_of_aerial_victory_credits) %>%
    head(1)
    
}

States <- airmen %>%
  filter(number_of_aerial_victory_credits > 0) %>%
  split(pull(.,state)) %>%
  map_dfr(StateFunction,.id="state")

p <- States%>%
  ggplot( aes(state, number_of_aerial_victory_credits, size = number_of_aerial_victory_credits)) +
  geom_point() +
  theme_bw()+
  labs(x="State",y="Average Number of Aerial Victories (among pilots who shot at least one plane down)",title="Interactive Chart Showing Average Number of Aerial Victories by Pilot's Home State")

ggplotly(p)

