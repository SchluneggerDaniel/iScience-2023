# Plot all Likelihood Plots

library(tidyverse)
library(patchwork)
library(ggpubr) 
library(rstatix)

### === LOAD DATA === ###

load("processed_data/Results/df_LLhood_absolute_vs_Stim.Rdata")
load("processed_data/Results/df_LLhood_sig_absolute_vs_Stim.Rdata")

# Relevel factor
dfabs_LLhood_vs_Stim$level <- factor(dfabs_LLhood_vs_Stim$level, levels = c("easy", "difficult"))

# Change factor labels 
levels(dfabs_LLhood_vs_Stim$learningmodel) <- list("Transition Probability" = "TP",
                                                   "Item Frequency" = "IF",
                                                   "Alternation Frequency" = "AF",
                                                   "Stimulus-Only" = "Stim")

# Create subdataframes for modality 
dfabs_auditory <- dfabs_LLhood_vs_Stim |> 
  filter(modality %in% "auditory")

dfabs_vestibular <- dfabs_LLhood_vs_Stim |> 
  filter(modality %in% "vestibular")

dfabs_visual <- dfabs_LLhood_vs_Stim |> 
  filter(modality %in% "visual")

# Auditory 
p1_auditoryAbsolute <- dfabs_auditory |> 
  ggboxplot(x = "level",
            y = "loglikelihood_centered", 
            color = "learningmodel",
            size = 0.35, 
            alpha = 1,
            outlier.shape = NA) +
  scale_color_manual(name = "Learning model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF",
                                "Stimulus-Only" = "#000000")) +
  theme_classic() +
  labs(title = " ",
       subtitle = "Auditory",
       x = " ",
       y = "\nlog-likelihood \ncentered") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))


# only significant comparisons are plotted according to absolute_LLhood_learningmodels_vs_Stim
p1_auditoryAbsolute <- p1_auditoryAbsolute +
  geom_bracket(xmin = 0.71, xmax = 1.3, y.position = 830, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "auditory" &
                 absolute_LLhood_learningmodels_vs_Stim$level == "easy" &
                 absolute_LLhood_learningmodels_vs_Stim$comparison == "TP vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 0.91, xmax = 1.3, y.position = 680, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "auditory" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "easy" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "IF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.1, xmax = 1.3, y.position = 530, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "auditory" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "easy" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "AF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.71, xmax = 2.3, y.position = 550, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "auditory" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "TP vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.91, xmax = 2.3, y.position = 450, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "auditory" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "IF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 2.1, xmax = 2.3, y.position = 350, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "auditory" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "AF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02)


# Vestibular
p1_vestibularAbsolute <- dfabs_vestibular |> 
  ggboxplot(x = "level",
            y = "loglikelihood_centered", 
            color = "learningmodel",
            size = 0.35, 
            alpha = 1,
            outlier.shape = NA) +
  scale_color_manual(name = "Learning model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF",
                                "Stimulus-Only" = "#000000")) +
  theme_classic() +
  labs(title = " ",
       subtitle = "Vestibular",
       x = "\nStimuli",
       y = " ") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "bottom",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))


# only significant comparisons are plotted according to absolute_LLhood_learningmodels_vs_Stim
p1_vestibularAbsolute <- p1_vestibularAbsolute +
  geom_bracket(xmin = 1.7, xmax = 2.3, y.position = 570, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "vestibular" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "TP vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.9, xmax = 2.3, y.position = 470, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "vestibular" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "IF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 2.1, xmax = 2.3, y.position = 370, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "vestibular" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "AF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02)



# Visual
p1_visualAbsolute <- dfabs_visual |> 
  ggboxplot(x = "level",
            y = "loglikelihood_centered", 
            color = "learningmodel",
            size = 0.35, 
            alpha = 1,
            outlier.shape = NA) +
  scale_color_manual(name = "Learning model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF",
                                "Stimulus-Only" = "#000000")) +
  theme_classic() +
  labs(title = " ",
       subtitle = "Visual",
       x = " ",
       y = " ") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))


# only significant comparisons are plotted according to absolute_LLhood_learningmodels_vs_Stim

p1_visualAbsolute <- p1_visualAbsolute +
  geom_bracket(xmin = 0.71, xmax = 1.3, y.position = 1600, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "visual" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "easy" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "TP vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 0.91, xmax = 1.3, y.position = 1450, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "visual" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "easy" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "IF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.1, xmax = 1.3, y.position = 1300, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "visual" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "easy" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "AF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.71, xmax = 2.3, y.position = 750, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "visual" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "TP vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.91, xmax = 2.3, y.position = 600, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "visual" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "IF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 2.1, xmax = 2.3, y.position = 450, 
               label = absolute_LLhood_learningmodels_vs_Stim$p.symbol[
                 absolute_LLhood_learningmodels_vs_Stim$modality == "visual" &
                   absolute_LLhood_learningmodels_vs_Stim$level == "difficult" &
                   absolute_LLhood_learningmodels_vs_Stim$comparison == "AF vs Stim-Only"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02)


plot_LLhood_vs_Stim <- ((p1_auditoryAbsolute | p1_vestibularAbsolute | p1_visualAbsolute) +
                          plot_layout(guides = "auto")) + 
  plot_annotation()


ggsave(filename = "Figure2.tiff",
       plot = plot_LLhood_vs_Stim,
       width = 18,
       height = 7,
       path = "/Users/daniel.schlunegger/Desktop/Figures_iScience/",
       dpi = 300,
       bg = "white",
       units = "cm")

