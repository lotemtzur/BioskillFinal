library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(RColorBrewer)
library(ggpubr)

data <- read.csv("somatic_alterations_from_tumors.csv")
smoking_status_unique <- data %>%
  filter(Smoke.Status %in% c("Smoker", "Nonsmoker"),
         Cancer.Type == "Non Small Cell Lung Cancer") %>%
  select(PATIENT_ID, Smoke.Status, TMB..nonsynonymous.) %>%
  distinct(PATIENT_ID, .keep_all = TRUE) %>%
  mutate(Smoke.Status = factor(Smoke.Status, levels = c("Smoker", "Nonsmoker")))

p <- ggplot(smoking_status_unique, aes(x = Smoke.Status, y = TMB..nonsynonymous., fill = Smoke.Status)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  stat_compare_means(
    method = "wilcox.test",
    label.y = max(smoking_status_unique$TMB..nonsynonymous., na.rm = TRUE) * 0.95
  ) +
  scale_fill_manual(
    values = c("Smoker" = "#d73027", "Nonsmoker" = "#1a9850")
  ) +
  labs(
    title = "TMB by Smoking Status in Non Small Cell Lung Cancer (Unique Patients)",
    x = "Smoking Status",
    y = "Nonsynonymous Mutation Count (TMB)"
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian(ylim = c(0, 1.5)) +
  theme(plot.title = element_text(hjust = 0.5), 
  legend.position = "none")

ggsave("ExplanatoryPlot.png",
       plot = p,
       bg = "white",         
       dpi = 300,            
       units = "in",         
       width = 10, height = NA)  