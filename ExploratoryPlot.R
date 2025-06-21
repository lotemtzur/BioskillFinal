library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(RColorBrewer)

data <- read.csv("somatic_alterations_from_tumors.csv")


# Order Chromosome by natural order: 1–22 then X
ordered_chr <- c(as.character(1:22), "X")
data$Chromosome <- factor(data$Chromosome, levels = ordered_chr)


summary_df <- data %>%
  group_by(Cancer.Type, Chromosome) %>%
  summarise(TMB_count = n()) %>%
  filter(!is.na(Chromosome)) %>%
  ungroup() %>%
  complete(Cancer.Type, Chromosome, fill = list(TMB_count = 0)) %>%
  group_by(Cancer.Type) %>%
  mutate(TMB_total = sum(TMB_count)) %>%
  ungroup() %>%
  mutate(Cancer.Type = fct_reorder(Cancer.Type, TMB_total, .desc = TRUE))
  

# Find top 10 cancers by TMB count
top10_cancers <- summary_df %>%
  group_by(Cancer.Type) %>%
  summarise(TMB_total = sum(TMB_count), .groups = "drop") %>%
  arrange(desc(TMB_total)) %>%
  slice_head(n = 10) %>%
  pull(Cancer.Type)

# Filter data by top 10 cancers
summary_top10 <- summary_df %>%
  filter(Cancer.Type %in% top10_cancers) %>%
  group_by(Cancer.Type) %>%
  mutate(TMB_total = sum(TMB_count)) %>%
  ungroup() %>%
  mutate(Cancer.Type = fct_reorder(Cancer.Type, TMB_total, .desc = TRUE))


# Plot exploratory heatmap 
p <- ggplot(summary_top10, aes(x = Chromosome, y = Cancer.Type, fill = TMB_count)) +
  geom_tile(color = "white") +
    scale_fill_distiller(
    name = "TMB Count",
    palette = "OrRd",
    direction = 1,
    limits = c(0, max(summary_top10$TMB_count, na.rm = TRUE))
  ) +
  theme_minimal() +
  labs(title = "TMB Count by Cancer Type and Chromosome",
       x = "Chromosome",
       y = "Cancer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8), 
		plot.title = element_text(hjust = 0.5))

ggsave("ExploratoryPlot.png",
       plot = p,
       bg = "white",         
       dpi = 300,            
       units = "in",         
       width = 10, height = NA)  