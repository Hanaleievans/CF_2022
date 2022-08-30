#Calice Morphology Data Analysis 

#Installing Packages
library(tidyverse)
library(ggplot2)

#Importing Data 
Raw_Data <-read.csv('~/Desktop/Github/CF_2022/Transplants_Calice_Master.csv')


#Seperating ID's
raw_calice <-separate(Raw_Data, Label, into= c("species", "treatment", "sample", "replicate"), sep = "_")


#Merge multiple calices per sample

calice_W_means <- raw_calice %>% 
  filter(MEASUREMENT=="W") %>%
  group_by(species, treatment, sample, replicate) %>%
  summarise(., mean_W = mean(Length)) 

#Graphs
ggplot(calice_W_means, aes(x=Label, y=mean_W)) +
  geom_point() +
  labs (y= "Mean Calice Width", x= "Sample Name")

graph1<- ggplot(calice_W_means, aes(x=treatment, y=mean_W)) +
  geom_boxplot()

ggsave("Calice_W.jpg", plot = graph1, path ='~/Desktop')











#Installing Packages
library(tidyverse)
library(ggplot2)

#Importing Data 
Raw_Data <-read.csv('~/Desktop/Github/CF_2022/Transplants_Calice_Master.csv')


#Merge multiple calices per sample

calice_H_means <- Raw_Data %>% 
  filter(MEASUREMENT=="H") %>%
  group_by(Label) %>%
  summarise(., mean_H = mean(Length)) 

#Graphs
ggplot(calice_H_means, aes(x=Label, y=mean_H)) +
  geom_point() +
  labs (y= "Mean Calice Height", x= "Sample Name")












  
  
