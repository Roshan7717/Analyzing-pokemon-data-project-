# Analyzing-pokemon-data-project
rm(list = ls())
install.packages("tidyverse")
library(tidyverse) #helps wrangle data
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
#LOADING DATA 
pokemon<- read.csv("pokemon.csv",header = TRUE)

#REANAMING COLUMNS NAMES 
pokemon <- pokemon %>% rename(primary_type= Type.1, secondry_type= Type.2,pokemom_name = Name)

summary(pokemon)
glimpse(pokemon)

pokemon$Generation <- as.factor(pokemon$Generation)
pokemon$primary_type <- as.factor(pokemon$primary_type)
pokemon$secondry_type <- as.factor(pokemon$secondry_type)
pokemon$Legendary <- as.factor(pokemon$Legendary)

grass_pokemon <- pokemon %>% filter(pokemon$primary_type=="Grass")

pokemon_stats  <- pokemon %>%   group_by(primary_type) %>% 
  summarise(avg_attack =mean(Attack),avg_speed= mean(Speed),number_of_pokemon=n(),avg_total= mean(Total)) %>% 
  arrange(primary_type)

pokemon_stats_1  <- pokemon %>%   group_by(primary_type, Generation) %>% 
  summarise(avg_attack =mean(Attack),avg_speed= mean(Speed),number_of_pokemon=n(),avg_total= mean(Total)) %>% 
  arrange(primary_type)



pokemon_stats_legendry  <- pokemon %>% filter(pokemon$Legendary=="True") %>%  group_by(primary_type) %>% 
  summarise(avg_attack =mean(Attack),avg_speed= mean(Speed),number_of_pokemon=n(),avg_total= mean(Total)) %>% 
  arrange(primary_type)

ggplot(data= pokemon) +geom_bar(mapping= aes(x=primary_type,fill=Generation))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(pokemon,aes(x=primary_type,fill=Generation)) +geom_bar(position = "fill")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(pokemon,aes(x=primary_type,fill=Legendary)) +geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(pokemon,aes(Total, fill= Legendary)) +geom_density(alpha=0.5) 

ggplot(pokemon, aes(primary_type,Total,fill=Legendary))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(pokemon,aes(Attack,Speed))+geom_point(aes(colour= Legendary))+geom_smooth(method = "lm")+facet_grid(.~Legendary,scales = "free")

