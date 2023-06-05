# Plot Accuracy for all data sets 

set.seed(81) # because of geom_jitter

library(tidyverse)
library(patchwork)
library(equatiomatic)
library(ggtext)

load("processed_data/allValues/allValuesAuditory.Rda")
load("processed_data/allValues/allValuesVestibular.Rda")
load("processed_data/allValues/allValuesVisual.Rda")

# Add to new dataframe
dfplot_auditory <- allValuesAuditory
dfplot_vestibular <- allValuesVestibular
dfplot_visual <- allValuesVisual

# Auditory
dfplot_auditory$intensity <- abs(dfplot_auditory$intensity)

dfplot_auditory <- dfplot_auditory |> 
  mutate(across(c("stimulus", "intensity"), factor)) |> 
  group_by(subject, stimulus, intensity) |> 
  summarise(accuracy = mean(correct))

# Vestibular
dfplot_vestibular <- dfplot_vestibular |> 
  mutate(across(c("stimulus", "intensity_ord"), factor)) |> 
  group_by(subject, stimulus, intensity_ord) |> 
  summarise(accuracy = mean(correct))

# Visual
dfplot_visual <- dfplot_visual |> 
  filter(!(intensity %in% 0))

dfplot_visual <- dfplot_visual |> 
  mutate(across(c("stimulus", "intensity_ord"), factor)) |> 
  group_by(subject, stimulus, intensity_ord) |> 
  summarise(accuracy = mean(correct))



# Plots

p1 <- dfplot_auditory |> 
  ggplot(aes(x = intensity, y = accuracy, fill = stimulus)) +
  geom_boxplot(outlier.shape = NA, alpha = 1, lwd = 0.2) +
  geom_jitter(aes(color = stimulus), size = 0.1, width = 0.1) +
  scale_color_manual(name = "Stimulus",
                     labels = c("A, /ka/, left, down", "B, /to/, right, up"),
                     values = c("#244f81", "#46ab45")) + 
  scale_fill_manual(name = "Stimulus",
                    labels = c("A, /ka/, left, down", "B, /to/, right, up"),
                    values = c("#244f81", "#46ab45")) +
  ylim(-0.05,1.05) +
  theme_classic() +
  labs(title = "A",
       subtitle = "Auditory",
       x = "Stimulus intensity",
       y = "Accuracy") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        plot.subtitle = element_text(size=10),
        # legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))


p2 <- dfplot_vestibular |> 
  ggplot(aes(x = intensity_ord, y = accuracy, fill = stimulus)) +
  geom_boxplot(outlier.shape = NA, alpha = 1, lwd = 0.2) +
  geom_jitter(aes(color = stimulus), size = 0.1, width = 0.1) +
  scale_color_manual(name = "Stimulus",
                     labels = c("left", "right"),
                     values = c("#244f81", "#46ab45")) + 
  scale_fill_manual(name = "Stimulus",
                    labels = c("left", "right"),
                    values = c("#244f81", "#46ab45")) + 
  ylim(-0.05,1.05) +
  theme_classic() +
  labs(title = "B",
       subtitle = "Vestibular",
       x = " ",
       y = "Accuracy") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        plot.subtitle = element_text(size=10),
        legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))



p3 <- dfplot_visual |> 
  ggplot(aes(x = intensity_ord, y = accuracy, fill = stimulus)) +
  geom_boxplot(outlier.shape = NA, alpha = 1, lwd = 0.2) +
  geom_jitter(aes(color = stimulus), size = 0.1, width = 0.1) +
  scale_color_manual(name = "Stimulus",
                     labels = c("Down", "Up"),
                     values = c("#244f81", "#46ab45")) + 
  scale_fill_manual(name = "Stimulus",
                    labels = c("Down", "Up"),
                    values = c("#244f81", "#46ab45")) + 
  ylim(-0.05,1.05) +
  theme_classic() +
  labs(title = "C",
       subtitle = "Visual",
       x = "Motion coherence",
       y = "Accuracy") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        plot.subtitle = element_text(size=10),
        legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))


p4 <- guide_area() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        plot.subtitle = element_text(size=10),
        legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))
  

layout_accuracyPlot <- 
  "
     AB
     CD
"

accuracyPatchwork <- (p1 + p2 + p3 + p4) +
                      plot_layout(design = layout_accuracyPlot, guides = "collect") +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", color = NA),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                legend.background = element_rect(fill = "transparent", color = NA)))


accuracyPatchwork



ggsave(filename = "plot_Accuracy.png", 
       plot = accuracyPatchwork, 
       width = 18, 
       height = 18, 
       path = "/Users/daniel.schlunegger/Desktop/Figures_iScience/",
       dpi = 300, 
       bg = "transparent", 
       units = "cm")

