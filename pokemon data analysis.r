# installing libraries 
rm(list = ls())
install.packages("tidyverse")
library(tidyverse) #helps wrangle data
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("skimr")
library(skimr)

#LOADING DATA 
pokemon<- read.csv("pokemon.csv",header = TRUE)

#WRANGLE DATA

#REANAMING COLUMNS NAMES 
pokemon <- pokemon %>% rename(primary_type= Type.1, secondry_type= Type.2,pokemom_name = Name)

summary(pokemon) #Statistical summary of data. Mainly for numerics
glimpse(pokemon) #get glimpse of data 
skim(pokemon) #get summary of data, check missing data

#Convert primary_type,secondry_type,Generation and Legendary to factors so they can be used FOR ANALYSIS
pokemon$Generation <- as.factor(pokemon$Generation)
pokemon$primary_type <- as.factor(pokemon$primary_type)
pokemon$secondry_type <- as.factor(pokemon$secondry_type)
pokemon$Legendary <- as.factor(pokemon$Legendary)

#CONDUCT DESCRIPTIVE ANALYSIS

#Number of pokemon in each generation
ggplot(pokemon,aes(Generation, fill = Generation))+geom_bar()+ guides(fill= "none") + ggtitle("Number of pokemon in each generation")

#Number of pokemon in each type by generation
ggplot(data= pokemon) +geom_bar(mapping= aes(x=primary_type,fill=Generation))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title='Number of pokemon in each type by generation',x='primary_type',y='Number of pokemon')

#Percentage of pokemon in each type by generation
ggplot(pokemon,aes(x=primary_type,fill=Generation)) +geom_bar(position = "fill")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title='Percentage of pokemon in each type by generation',x='primary_type',y='Percentage of pokemon')

#Percentage of legendary pokemon in each type
ggplot(pokemon,aes(x=primary_type,fill=Legendary)) +geom_bar(position = "fill")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title='Percentage of legendary pokemon in each type ',x='primary_type',y='Percentage of pokemon')

#Total power of legendary vs non legendary pokemon 
ggplot(pokemon,aes(Total, fill= Legendary)) +geom_density(alpha=0.5) 

#Total power of legendary vs non legendary pokemon by primary type
ggplot(pokemon, aes(primary_type,Total,fill=Legendary))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#correlation between diffrent stats of pokemon 
cor.pokemon <- cor(pokemon[,c('HP','Attack','Defense','Speed','Sp..Atk','Sp..Def')])
ggcorrplot(cor.pokemon,hc.order = TRUE,lab=TRUE)

ggplot(pokemon,aes(Sp..Atk,Sp..Def))+geom_point(aes(colour= Legendary))+geom_smooth(method = "lm")+facet_grid(.~Legendary,scales = "free")
