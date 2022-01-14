library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)
library(factoextra)
library(grid)
library(gridExtra)

data <- read.csv("data/fifa_players.csv", sep="|", encoding = 'UTF-8')

# Create global position variable (Defender, Midfielder or Forward)
data$global_position <- ifelse(data$player_position %in% c("RB","RWB","LB","LWB","CB") ,"Defender",
                               ifelse(data$player_position %in% c("CDM","CM","CAM","LM","RM"), "Midfielder",
                                      "Forward"))

# https://dplyr.tidyverse.org/reference/ (library for select, filter, rename...)
data <- data %>% select(id, short_name, club_name,
                        league_name, nationality_name, player_position, global_position,
                        age, value_eur, wage_eur, preferred_foot,
                        pace, shooting, passing, dribbling, defending, physic,
                        search_name)
# search_name has to be the last one

# Rename columns (new name = older name)
data <- data %>% rename(Name = short_name, Club = club_name, League = league_name, 
                        Nation = nationality_name, Position = player_position, 
                        Global.Position = global_position,
                        Age = age, Value = value_eur, Salary = wage_eur, Foot = preferred_foot,
                        Pace = pace, Shooting = shooting, 
                        Passing = passing, Dribbling = dribbling, Defending = defending, 
                        Physic = physic)
# These are the last names of the columns

stats_names <- c('Pace','Shooting','Dribbling', 'Passing', 'Defending', 'Physic')

# Clustering
data_cluster <- data %>% filter(Global.Position == "Forward") 
row.names(data_cluster) <- data_cluster$id 
data_cluster <- data_cluster %>% select(stats_names)
data_cluster

k2 <- kmeans(data_cluster, center = 2, nstart = 25)   
k3 <- kmeans(data_cluster, centers = 3, nstart = 25)
k4 <- kmeans(data_cluster, centers = 4, nstart = 25)
k5 <- kmeans(data_cluster, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = data_cluster, ggtheme = theme_minimal())+ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = data_cluster, ggtheme = theme_minimal())+ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = data_cluster, ggtheme = theme_minimal())+ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = data_cluster, ggtheme = theme_minimal())+ggtitle("k = 5")

options(repr.plot.width = 15, repr.plot.height = 8)
grid.arrange(p1,p2,p3,p4)
