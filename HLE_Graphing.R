# Install Packages
library(tidyverse)
library(tidyr)
library(ggplot2)
library("gridExtra")
library("cowplot")
library(ggpubr)

#OFAV P VALUES
#filter to just have OFAV data 
raw_OFAV_PPvPS <- raw_calice %>%
  filter(Species == "OFAV") %>%
  filter(Treatment == c("PP", "PS"))

#make dataset long 
raw_long <- pivot_longer(raw_filter,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                         names_to = "measurement")
#Changing measurement labels
raw_long$measurement <- raw_long$measurement %>%
  gsub("mean_W","Width",.)%>%
  gsub("mean_CW","Columella Width",.)%>%
  gsub("mean_H","Height",.)%>%
  gsub("mean_CH","Columella Height",.)%>%
  gsub("mean_A","Area",.)%>%
  gsub("mean_CA","Columella Area",.)


#make graph of calice measurements 
plots_OFRAVOFAV <- ggplot(raw_long, aes(x=measurement, y=value, color=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean (cm)", x = "Treatment")


# compare means
plots_OFAV_pvalues <- plots_OFRAVOFAV + stat_compare_means(method = "t.test", size = 2)
ggsave("TRANSPLANT_GRAPH.jpg", plot = plots_OFRAVOFAV_pvalues, path = '~/Desktop/GITHUB/CF_2022')

plots_OFRAVOFAV_pvalues



#OFRA PVALUES

#filter to just have OFRA data 
raw_OFRA <- raw_calice %>%
  filter(Species == "OFRA")

#make dataset long 
raw_OFRA_long <- pivot_longer(raw_OFRA,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                              names_to = "measurement")
#Changing measurement labels
raw_long$measurement <- raw_long$measurement %>%
  gsub("mean_W","Width",.)%>%
  gsub("mean_CW","Columella Width",.)%>%
  gsub("mean_H","Height",.)%>%
  gsub("mean_CH","Columella Height",.)%>%
  gsub("mean_A","Area",.)%>%
  gsub("mean_CA","Columella Area",.)

#make graph of calice measurements 
plots_OFRA <- ggplot(raw_OFRA_long, aes(x=measurement, y=value, color=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean (cm)", x = "Treatment")
theme_bw()
plots_OFRA



# compare means
plots_OFRAVOFAV_pvalues <- plots_OFRAVOFAV + stat_compare_means(method = "t.test", size = 2)
ggsave("TRANSPLANT_GRAPH.jpg", plot = plots_OFRAVOFAV_pvalues, path = '~/Desktop/GITHUB/CF_2022')

plots_OFRAVOFAV_pvalues

