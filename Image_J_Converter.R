#ImageJ Converter 

library(tidyverse)

# Command for loading cheatsheet
cheatsheet <- read.csv('~/Desktop/GitHub/CF_2022/CALICE.CHEATSHEET.csv')

# Loading second sample
ImageJSample <- read.csv('~/Desktop/GitHub/CF_2022/Image_J/OFAV_PP_9AZ5.csv',na.strings=c("",".","NaN"))

#Loading Master File
master <- read.csv('~/Desktop/Github/CF_2022/Transplants_Calice_Master.csv')

#Erase .jpg files
ImageJSample$Label <- gsub(".jpg","",as.character(ImageJSample$Label))

#Merging Samples 
x <- merge(x= ImageJSample, y=cheatsheet, by= 0)

x <- merge(x= ImageJSample, y=cheatsheet, by= 0) %>%
  .[c(1,3:11)]

#Write new csv 
write.csv(x, '~/Desktop/GitHub/CF_2022/RawData/Merged_OFAV_PP_9AZ5.csv')


#append the new data to the master
#the distinct command makes sure replicate lines are not being added 
master <- rbind(master, x) %>% distinct()
#save updated master
write.csv(master, '~/Desktop/Github/CF_2022/Transplants_Calice_Master.csv', row.names=FALSE)

