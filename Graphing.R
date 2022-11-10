# Calice Morphology Data Analysis 
# Written by Taylor Lindsay & Hana-lei Evans, Summer '22
# Data analysis on coral calice morphometrics from common garden experiment in Puerto Rico. 

# Packages & Data Import  -------------------------------------------------

# Install Packages
library(tidyverse)
library(tidyr)
library(ggplot2)
library("gridExtra")
library("cowplot")
library(ggpubr)



# Import data 
raw_calice <- read.csv('~/Desktop/GITHUB/CF_2022/Transplants_Calice_Averages.csv')


# General boxplot graphs ------------------------------------------------------------------

#width 
plot_W <- ggplot(raw_calice, aes(x=Treatment, y=mean_W, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")
#Columnella width 
plot_CW <- ggplot(raw_calice, aes(x=Treatment, y=mean_CW, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")
#combine widths 
plots_W <- plot_grid(plot_W, plot_CW, labels=c("A", "B"), ncol = 2, nrow = 1) 
#save 
ggsave("plot_calice_W.jpg", plot = plots_W, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')


#Height 
plot_H <- ggplot(raw_calice, aes(x=Treatment, y=mean_H, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Height", x = "Treatment")
#Columnella height
plot_CH <- ggplot(raw_calice, aes(x=Treatment, y=mean_CH, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Height", x = "Treatment")
#combine widths 
plots_H <- plot_grid(plot_H, plot_CH, labels=c("A", "B"), ncol = 2, nrow = 1) 
#save 
ggsave("plot_calice_H.jpg", plot = plots_H, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#Area
plot_A <- ggplot(raw_calice, aes(x=Treatment, y=mean_A, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Area", x = "Treatment")
#Columnella area
plot_CA <- ggplot(raw_calice, aes(x=Treatment, y=mean_CA, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Area", x = "Treatment")
#combine widths 
plots_A <- plot_grid(plot_A, plot_CA, labels=c("A", "B"), ncol = 2, nrow = 1) 
#save 
ggsave("plot_calice_A.jpg", plot = plots_A, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#Density
plot_D <- ggplot(raw_calice, aes(x=Treatment, y=Calice_Density, color=Species)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
#save 
ggsave("plot_calice_D.jpg", plot = plot_D, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

# OFRA Graphs -------------------------------------------------------------

#filter to just have OFRA data 
raw_OFRA <- raw_calice %>%
  filter(Species == "OFRA")

#make dataset long 
raw_OFRA_long <- pivot_longer(raw_OFRA,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                              names_to = "measurement")

#make graph of calice measurements 
plots_OFRA <- ggplot(raw_OFRA_long, aes(x=measurement, y=value, color=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")
#save graph
ggsave("plot_OFRA_Calice.jpg", plot = plots_OFRA, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')
# compare means
plots_OFRA_pvalues <- plots_OFRA + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFRA_pvalues.jpg", plot = plots_OFRA_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#compare Density of OFRA
plot_D_OFRA <- ggplot(raw_OFRA, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plots_OFRA_D_pvalues <- plot_D_OFRA + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFRA_D_pvalues.jpg", plot = plots_OFRA_D_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')


# OFAV Control Graphs -----------------------------------------------------------

#filter to just have OFAV data 
raw_OFAV_control <- raw_calice %>%
  filter(Species == "OFAV") %>%
  filter(Treatment == c("PP", "SS"))

#make dataset long 
raw_OFAV_control_long <- pivot_longer(raw_OFAV_control,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                                      names_to = "measurement")

#make graph of calice measurements 
plots_OFAV_control <- ggplot(raw_OFAV_control_long, aes(x=measurement, y=value, color=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")
# compare means
plots_OFAV_control_pvalues <- plots_OFAV_control + stat_compare_means(method = "t.test", size = 2)
ggsave("plots_OFAV_control_pvalues.jpg", plot = plots_OFAV_control_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#compare Density of OFAV Controls
plot_D_OFAV_control <- ggplot(raw_OFAV_control, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plots_OFAV_control_D_pvalues <- plot_D_OFAV_control + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_control_D_pvalues.jpg", plot = plots_OFAV_control_D_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')



# OFAV PP PS --------------------------------------------------------------

#filter to just have OFAV data 
raw_OFAV_PPvPS <- raw_calice %>%
  filter(Species == "OFAV") %>%
  filter(Treatment == c("PP", "PS"))

#make dataset long 
raw_OFAV_PPvPS_long <- pivot_longer(raw_OFAV_PPvPS,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
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
plots_OFAV_PPvPS <- ggplot(raw_OFAV_PPvPS_long, aes(x=measurement, y=value, color=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")


# compare means
plots_OFAV_PPvPS_pvalues <- plots_OFAV_PPvPS + stat_compare_means(method = "t.test", size = 2)
ggsave("plots_OFAV_PPvPS_pvalues.jpg", plot = plots_OFAV_PPvPS_pvalues, path = '~/Desktop/GITHUB/CF_2022')
plots_OFAV_PPvPS

#compare Density of OFAV Controls
plot_D_OFAV_PPvPS <- ggplot(raw_OFAV_PPvPS, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plot_D_OFAV_PPvPS_pvalues <- plot_D_OFAV_PPvPS + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_PPvPS_D_pvalues.jpg", plot = plot_D_OFAV_PPvPS_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')



# OFAV SS SP --------------------------------------------------------------

#filter to just have OFAV data 
raw_OFAV_SSvSP <- raw_calice %>%
  filter(Species == "OFAV") %>%
  filter(Treatment == c("SS", "SP"))

#make dataset long 
raw_OFAV_SSvSP_long <- pivot_longer(raw_OFAV_SSvSP,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                                    names_to = "measurement")

#make graph of calice measurements 
plots_OFAV_SSvSP <- ggplot(raw_OFAV_SSvSP_long, aes(x=measurement, y=value, color=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")
# compare means
plots_OFAV_SSvSP_pvalues <- plots_OFAV_SSvSP + stat_compare_means(method = "t.test", size = 2)
ggsave("plots_OFAV_SSvSP_pvalues.jpg", plot = plots_OFAV_SSvSP_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#compare Density of OFAV Controls
plot_D_OFAV_SSvSP <- ggplot(raw_OFAV_SSvSP, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plot_D_OFAV_SSvSP_pvalues <- plot_D_OFAV_SSvSP + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_SSvSP_D_pvalues.jpg", plot = plot_D_OFAV_SSvSP_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')





# Example Analysis  ----------------------------------------------------------------

summary(calice_W_means)
tapply(calice_W_means$mean_length, calice_W_means$full_treatment, summary)

calice_area_means <- raw_calice %>%
  #filter(MEASUREMENT == "W") %>%
  group_by(species, treatment, sample, replicate, MEASUREMENT) %>%
  summarize(., mean_area = mean(Area)) %>%
  .[(.$MEASUREMENT=="CA"),] %>%
  .[!(.$species=="IMG"),]

calice_W_means <-  calice_W_means[(calice_W_means$MEASUREMENT=="W"),] %>%
  .[!(.$full_treatment=="IMG_3872"),]
calice_H_means <-  calice_means[(calice_means$MEASUREMENT=="H"),] %>%
  .[!(.$species=="IMG"),]

# T-tests!  ---------------------------------------------------------------

# To run a t-test, you have to create a linear model 

# Build linear model, the ~ indicates "by", so here we are asking it to analyse mean_length by species
model_W <- lm(mean_length ~ species, data = calice_W_means)

# To view summary statistics of the linear model, use the summary command. 
# You can also save these stats as an item 
summary(model_W)









#Graphing


# OFRA Graphs -------------------------------------------------------------

#filter to just have OFAV data 
raw_filter <- raw_calice %>%
  filter(Treatment == c("SS","PP"))


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
plots_OFRAVOFAV <- ggplot(raw_long, aes(x=measurement, y=value, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean (cm)", x = "Treatment")+
  theme_bw()


plots_OFRAVOFAV

# compare means
plots_OFRAVOFAV_pvalues <- plots_OFRAVOFAV + stat_compare_means(method = "t.test", size = 2)
ggsave("TRANSPLANT_GRAPH.jpg", plot = plots_OFRAVOFAV_pvalues, path = '~/Desktop/GITHUB/CF_2022')

plots_OFRAVOFAV_pvalues



#compare Density of OFRA
plot_D_OFRA <- ggplot(raw_OFRA, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plots_OFRA_D_pvalues <- plot_D_OFRA + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFRA_D_pvalues.jpg", plot = plots_OFRA_D_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')
