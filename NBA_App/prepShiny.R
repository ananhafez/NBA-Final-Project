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
library(transformr)
library(ggridges)
library(cowplot)
library(broom)
library(rstanarm)
library(tidyverse)

nba_season_stats <- read.csv("NBA_App/Data/Seasons_stats_complete.csv") %>% filter(Year != "0")

player_career_stats <- read.csv("NBA_App/Data/players.csv")

dirty_player_salaries <- read.csv("NBA_App/Data/salaries_1985to2018.csv")

dirty_curry_stats <- read.csv("NBA_App/Data/curry_shooting.csv") %>% select(shot_made_flag, shot_type, shot_distance)

player_salaries <- player_career_stats %>% 
  left_join(dirty_player_salaries, by = c("X_id" = "player_id")) %>% 
  select(name, season_start, salary, team) %>% 
  filter(season_start != "NA")

curry_stats <- dirty_curry_stats %>% 
  group_by(shot_distance) %>% 
  count(shot_made_flag) %>% 
  mutate(shot_made_flag = as.logical(shot_made_flag)) %>%
  # mutate(shot_made_flag = if_else(TRUE, "made", "missed")) %>% 
  pivot_wider(names_from = shot_made_flag, values_from = n) %>% 
  mutate_all(~replace(., is.na(.), 0)) 

colnames(curry_stats)<- c("shot_distance","made","missed")

curry_stats_2<- curry_stats %>% 
  mutate(total = made + missed) %>% 
  mutate(fgp = made/total) %>% 
  mutate(efficiency = if_else(shot_distance < 22, fgp * 2, fgp * 3)) %>% 
  filter(shot_distance %in% c(5:30))

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

plot_3 <- ggplot(curry_stats_2, aes(shot_distance, fgp)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ylim(0,1) + geom_vline(xintercept = 22, colour="#BB0000", alpha = 0.7) +
  labs(title = "Steph Curry's Shot Accuracy by Distance",
                   x = "Shot Distance",
                   y = "Field Goal %",
                   caption = "Data from 2015-2016 Season courtesy of NBA.com",
                   subtitle = "Only Minor Drop-off after 3-Point Line (Red Line)")

plot_4 <- ggplot(curry_stats_2, aes(shot_distance, efficiency)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  geom_vline(xintercept = 22, colour="#BB0000", alpha = 0.7) +
  geom_hline(yintercept = 1.35, alpha = 0.7) +
  ylim(0,2) +
  labs(title = "Steph Curry's Shot Efficiency by Distance",
       x = "Shot Distance",
       y = "Average Points per Shot",
       caption = "Data from 2015-2016 Season courtesy of NBA.com",
       subtitle = "Curry's top 6 Efficiencies are behind 3-Point Line (Red Line)")

year_options <- nba_season_stats %>% group_by(Year) %>% select(Year) %>% count() %>% select(Year)
