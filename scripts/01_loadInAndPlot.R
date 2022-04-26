# Project: The fast and the sorrow
# Simulated by Timo Roettger
# Part 01: merge raw data, plot results
# Date: 12/18/2018

##############
## code book #
##############

## vpX_data            data frames for individual subjects
# var_001:             picture category: sad vs. happy 
# var_002:             unique id of experimental images (n = 64)
# var_003:             date of data collection
# var_004:             assigned list of trials sequence 
# var_005:             dependent variable: words per minute
# var_006              dependent variable: syllables per minute
# var_007:             dependent variable: phonemes per second
# var_008:             sadness rating (1-7)
# vp_id:               unique id of subject (n = 20)

##################
## preprocessing #
##################

## load in packages
library(readbulk)
library(rstudioapi)
library(ggplot2)
library(dplyr)

# load in data
## get the path of this script
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))                              

## choose sister folder and load in raw data
setwd("../raw/")                                         
data <- read_bulk(extension = ".csv")                 


##########
## plot ##
##########

# plot mean phoneme/second as a function of gender and mood
Figure1 <- data %>%
  group_by(vp_id, var_001) %>%
  summarise(mean = mean(var_007)) %>%
  ggplot(aes(x = var_001, y = mean, colour = var_001, group = vp_id)) +
  geom_line(colour = "grey", alpha = 0.3) +
  geom_point(size = 3, alpha = 0.8, position = position_jitter(0.05)) +
  scale_colour_manual("var_001",
                      guide = guide_legend(title = "Mood"),
                      values = c("#0072B2", "#D55E00")) +
  #scale_y_continuous(expand = c(0, 0), breaks = (c(6.8,7,7.2,7.4)), limits = c(6.8,7.4)) +
  labs(title = "Speech rate is influenced by mood",
       subtitle = "averages of individual speakers\n",
       y = "Phonemes per second\n",
       x = "\nMood") +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_blank())

# set working directory and store plot
setwd("../plots/")        
ggsave(filename = "Figure1.png", 
       plot = Figure1,
       width = 150, 
       height = 100,
       units = "mm",
       dpi = 300)