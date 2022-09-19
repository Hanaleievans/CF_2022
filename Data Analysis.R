# CF 2022: Data Analysis Codes --------------------------------------------

#Calice Morphology Data Analysis 
#Installing Packages
library(tidyverse)
library(ggplot2)

#Importing Data 
Raw_Data <-read.csv('~/Desktop/Github/CF_2022/Transplants_Calice_Master.csv')


#Seperating ID's
raw_calice <-separate(Raw_Data, Label, into= c("species", "treatment", "sample", "replicate"), sep = "_")

#combining treatment and species values
raw_calice <- raw_calice %>%
  unite('full_treatment', species:treatment, remove= FALSE)


#Merge multiple calices per sample

calice_W_means <- raw_calice %>% 
  filter(MEASUREMENT=="W") %>%
  group_by(full_treatment,species,treatment, sample, replicate,MEASUREMENT) %>%
  summarise(., mean_W = mean(Length)) 


calice_W_means <- calice_W_means[(calice_W_means$MEASUREMENT=="W"),] %>%
  .[!(.$full_treatment=="IMG_3872"),]




#Width means by treatment

tapply(calice_W_means$mean_W, calice_W_means$full_treatment, summary)


#Graphs
ggplot(calice_W_means, aes(x=Label, y=mean_W)) +
  geom_point() +
  labs (y= "Mean Calice Width", x= "Sample Name")

graph1<- ggplot(calice_W_means, aes(x=treatment, y=mean_W)) +
  geom_boxplot()

ggsave("Calice_W.jpg", plot = graph1, path ='~/Desktop')
















# T-tests -----------------------------------------------------------------

model_W <- lm(mean_W ~ species, data = calice_W_means)
summary(model_W)


# Graph Practice ----------------------------------------------------------


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












  
  




#







# CODING CHEATSHEET -------------------------------------------------------
# %>%: "PIPELINING" continue using previous information 
#!: Delete everything but "_____"
#"." Place holder after pipe lining for previous names
