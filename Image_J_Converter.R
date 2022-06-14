#ImageJ Converter 

# Command for loading cheatsheet
cheatsheet <- read.csv('~/Desktop/GitHub/CF_2022/CALICE.CHEATSHEET.csv')

# Loading second sample
ImageJSample <- read.csv('~/Desktop/Image_J/OFRA_PP_9GX5_P1.csv')

#Erase .jpg files
ImageJSample$Label <- gsub(" .png","",as.character(ImageJSample$Label))

#Merging Samples 
x <- merge(x= ImageJSample, y=cheatsheet, by= 0)

#Write new csv 
write.csv(x, '~/Desktop/GitHub/CF_2022/RawData/Merged_OFRA_PP_9GX5_P1.csv')

#