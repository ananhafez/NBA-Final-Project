library(shiny)
library(gganimate)
library(janitor)
library(skimr)
library(tidyr)
library(stringr)
library(httr)
library(RCurl)
library(lubridate)
library(readxl)
library(gt)
library(reshape2)
library(ggplot2)
library(purrr)
library(moderndive)
library(fs)
library(infer)
library(googlesheets4)
library(scales)
library(TeachBayes)
library(sf)
library(dplyr)
library(httr)
library(jsonlite)
library(transformr)
library(ggridges)
library(cowplot)
library(broom)
library(rstanarm)
library(tidyverse)

nba_season_stats <- read.csv("NBA_App/Data/Seasons_stats_complete.csv") %>% filter(Year != "0")

player_career_stats <- read.csv("NBA_App/Data/players.csv")

dirty_player_salaries <- read.csv("NBA_App/Data/salaries_1985to2018.csv")

player_salaries <- player_career_stats %>% 
  left_join(dirty_player_salaries, by = c("X_id" = "player_id")) %>% 
  select(name, season_start, salary, team) %>% 
  filter(season_start != "NA")

plot_1 <- nba_season_stats %>% 
  group_by(Year) %>% 
  summarize(total_shots = sum(FGA), total_3p = sum(X3PA)) %>% 
  mutate(prop_3p = round((total_3p/total_shots), digits = 2)) %>% 
  ggplot(aes(x = Year, y = prop_3p)) + geom_line() + 
  labs(title = "How the 3-Point Shot grew in the NBA", 
       x = "NBA Season", y = "Proportion of All Shots that were 3-Pointers") +
  xlim(1950, 2020)

points_over_time <- nba_season_stats %>% 
  group_by(Year) %>% 
  summarize(total_2points = sum(X2P * 2), 
            total_3points = sum(X3P * 3), 
            total_ft = sum(FT), 
            total_points = (total_2points + total_3points + total_ft), 
            prop_2points = total_2points/total_points, 
            prop_3points = total_3points/total_points, 
            prop_ft = total_ft/total_points) 

plot_2 <- ggplot(points_over_time, aes(Year)) + 
  geom_line(aes(y = prop_2points, color = "var0")) + 
  geom_line(aes(y = prop_3points, colour = "var1")) + 
  geom_line(aes(y = prop_ft, colour = "var2")) + 
  ylim(0,1) + 
  scale_colour_manual(labels = c("2-Pointers", "3-Pointers", "Free Throws"), values = c("red", "green", "blue")) + 
  xlim(1950, 2020) + 
  labs(title = "Sources of NBA Points Over Time", 
       y = "Percentage of Total Points", x = "NBA Season") + 
  theme(legend.title = element_blank())

year_options <- nba_season_stats %>% group_by(Year) %>% select(Year) %>% count() %>% select(Year)
