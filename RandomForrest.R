## Importing packages

library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse) 
library(DataExplorer)
library(gridExtra)

## Reading in files

list.files(path = "../data")
chess<- read.csv("~/Documents/Data_IA/R /Projet/data/games.csv")


#view chess dataframe
head(chess,2)

opening<-filter(summarise(group_by(chess,opening_name), count=length(opening_name)),count>200)

ggplot(opening,aes(x=opening_name,y=count))+geom_col()+coord_flip()+theme_classic()

ggplot(chess, aes(x = white_rating, y = black_rating, color = winner, shape = winner))+
  geom_smooth()+
  theme_minimal()+
  labs(y = "Black Rating", x = "White Rating", color = "Winner")

#Prédire le nombre de coup moyen / tyoe de victoire (classification)

chess  %>% count(victory_status) %>% 
  ggplot(aes(reorder(victory_status,n),n, fill = victory_status))+
  geom_col(show.legend = FALSE)+
  scale_fill_viridis_d()+
  theme_minimal()+
  coord_flip()+
  labs(x = "Victory Status", y = "Frequency")


ggplot(chess, aes(victory_status, fill = winner))+
  geom_bar(position = "dodge")+
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  coord_flip()+
  labs(x = "Victory Status", y = "Frequency", fill = "Winner")


chess  %>% group_by(winner) %>% count(opening_name, sort = TRUE) %>% head(30) %>% 
  ggplot(aes(reorder(opening_name,n), n, fill = winner, color = winner))+
  geom_col(position = "fill")+
  coord_flip()+
  theme_minimal()+
  scale_fill_manual(values = c("black", "cornsilk"))+
  scale_color_manual(values = c("cornsilk", "black"))+
  labs(y = "Winning Ratio", x = "Opening Name", fill = "Winner")+
  guides(color = FALSE)+
  theme(legend.position = "bottom")

#Valeurs manquantes == 0 
colSums(sapply(chess, is.na))

dim(chess)

# summarize the class Winner
percentage <- prop.table(table(chess$victory_status)) * 100
cbind(freq=table(chess$victory_status), percentage=percentage)

# summarize the class Winner
percentage <- prop.table(table(chess$winner)) * 100
cbind(freq=table(chess$winner), percentage=percentage)

#Numérisation factors to numeric (Black, Draw, White)
col_win = c(1,0,2)[as.numeric(games$winner)]
#Aajout des élément numéric au dataset "games"
games$col_win <- col_win
#Numérisation factors to numeric (Draw, Mate, Outoftime, Resign)
col_vict = c(0,1,2,3)[as.numeric(games$victory_status)]
#Ajout de la column
games$col_vict <- col_vict

percentage <- prop.table(table(chess$opening_eco)) * 100
cbind(freq=table(chess$opening_eco), percentage=percentage)


X = games %>% select(col_win,opening_ply,turns)
Y = games %>% select(col_vict)