rm(list=ls())

library(ggplot2)

load("treatment_coefficient_samples_1")
data_1 <- data.frame(value = treatment_coefficient_samples, label = rep("Therapy 2", length(treatment_coefficient_samples)))
load("treatment_coefficient_samples_2")
data_2 <- data.frame(value = treatment_coefficient_samples, label = rep("Therapy 3", length(treatment_coefficient_samples)))
load("treatment_coefficient_samples_3")
data_3 <- data.frame(value = treatment_coefficient_samples, label = rep("Therapy 4", length(treatment_coefficient_samples)))

data <- rbind(data_1, data_2, data_3)

data$label <- factor(data$label, levels = c("Therapy 2", "Therapy 3", "Therapy 4"))

ggplot(data, aes(x = label, y = value)) + labs(x = NULL, y = NULL) +
  geom_boxplot(fill = "lightblue", alpha = 0.8,  size = 0.8, outlier.size = 0.9,  outlier.fill = "black") + theme_minimal() + 
  scale_y_continuous(limits = c(-0.8, 1.7), breaks = c(-0.5, 0, 0.5, 1, 1.5))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14, face = "plain", family = "Times New Roman"), axis.ticks.x = element_blank()) + 
  theme(axis.text.y = element_text(size = 14, face = "plain", family = "Times New Roman"))

print(median(data_1$value))
print(median(data_2$value))
print(median(data_3$value))
