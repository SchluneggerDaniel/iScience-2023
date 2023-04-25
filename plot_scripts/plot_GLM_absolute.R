# Plot GLM (with interaction) absolute values

library(tidyverse)
library(patchwork)
library(ggpubr) 
library(rstatix)

# Load data
load("processed_data/Results/df_GLM_absolute.Rdata")
load("processed_data/Results/df_GLM_sig_absolute.Rdata")

# Change factor labels
levels(dfabs_GLM$modality) <- list("Auditory" = "auditory",
                                   "Vestibular" = "vestibular",
                                   "Visual" = "visual")

levels(dfabs_GLM$learningmodel) <- list("Transition Probability" = "TP",
                                        "Item Frequency" = "IF",
                                        "Alternation Frequency" = "AF",
                                        "Repetition Preference" = "RepPref")

plot_absoluteGLM <- dfabs_GLM |> 
  ggboxplot(x = "modality",
            y = "Nagelkerke",
            color = "learningmodel") +
  ylim(0,1.09) +
  scale_color_manual(name = "Learning Model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF",
                                "Repetition Preference" = "#ED6925FF")) +
  theme_classic() +
  labs(title = "GLM (probit regression)",
       subtitle = "stimulus + intensity × learning model",
       x = "\nModality",
       y = "Nagelkerke\n") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "bottom",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))


# Label does not to be taken from data frame (because all comparisons are *** - significant)

plot_absoluteGLM <- plot_absoluteGLM +
  geom_bracket(xmin = 1.1, xmax = 1.3, y.position = 0.97, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) + 
  geom_bracket(xmin = 0.9, xmax = 1.3, y.position = 1.02, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 0.7, xmax = 1.3, y.position = 1.07, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 2.1, xmax = 2.3, y.position = 0.75, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.9, xmax = 2.3, y.position = 0.8, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.7, xmax = 2.3, y.position = 0.85, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 3.1, xmax = 3.3, y.position = 0.9, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 2.9, xmax = 3.3, y.position = 0.95, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 2.7, xmax = 3.3, y.position = 1, 
               label = "***", 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02)



ggsave(filename = "plot_GLM_absolute.png",
       plot = plot_absoluteGLM,
       width = 18,
       height = 10,
       path = "/Users/daniel.schlunegger/Dropbox/LaTeX/iScience-2023/Figures/",
       dpi = 300,
       bg = "transparent",
       units = "cm")
