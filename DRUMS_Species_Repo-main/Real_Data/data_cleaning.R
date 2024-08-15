# file to perform functions on the real dataset
library(readr)
library(plotly)
# import data

setwd("~/DRUMS_Species_Repo/Data") # change if needed
data <- read.csv("4experiement_comboresults.csv")

# find instances from AI model 1
temp <- data[grep("ex1_", data$`File Name`), ]
write.csv(temp, "1experiment_comboresults.csv")

#test plot

ai1 <- temp
plot <- subset(ai1, common_name == '') # extract deer
plot <- subset(ai1, transect_id == 'Cam7 02-10-2024') # choose camera
plot <- subset(plot, sequence_id == 8666828) # choose sequence
plot$filename = substring(plot$filename, 6, 8) # shorten filenames
plot_ly(x=plot$world_x, y=plot$world_y, z=plot$world_z, type="scatter3d", mode="markers", color=plot$filename)

library(dplyr)
test <- ai1 %>% group_by(sequence_id, timestamp, deployment_id, common_name) %>% summarise(Count = n())
