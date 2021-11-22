# R Code: Load data
library(tidyverse)
edge = read_csv("edge_influence.csv")
node = read_csv("node_influence.csv")

full_data = left_join(edge,node, by = "rater_ID")


# R Code: Calculate Dyadic exposure
full_data$exposure <- full_data$exposure_time * full_data$negative_affect_0	

# Calculate mean exposure 
mean_exposure <- full_data %>% group_by(rater_ID) %>% summarize(exposure_mean = mean(exposure,na.rm = TRUE))

# Include mean exposure in the node's data and handle cases where there was no exposure
node = left_join(node,mean_exposure, by = "rater_ID")
node$exposure_mean[is.na(node$exposure_mean)] <- 0


# R Code: Run OLS analysis
model1 <- lm(negative_affect_1 ~ negative_affect_0 + exposure_mean, data = node)
summary(model1)
