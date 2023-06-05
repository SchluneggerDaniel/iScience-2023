# Plot best fitting Omega for different learning models in difficult trials

library(tidyverse)
library(patchwork)
library(ggpubr) 
library(rstatix)

load("processed_data/Results/df_LLhood_absolute.Rdata")

# Create subdataframe with difficult trials only
df_Omega <- dfabs_LLhood |> 
  filter(level == "difficult") |> 
  droplevels()

# Change levels for plotting
levels(df_Omega$learningmodel) <- list("Transition Probability" = "TP",
                                        "Item Frequency" = "IF",
                                        "Alternation Frequency" = "AF")

levels(df_Omega$modality) <- list("Auditory" = "auditory",
                                   "Vestibular" = "vestibular",
                                   "Visual" = "visual")

# Change Omega == Inf --> Omega == 31 for plotting purposes
df_Omega <- df_Omega |>
  mutate(Omega = ifelse(Omega == Inf, yes = 31, no = Omega))


# Plot 
plot_LLhood_Omega <- df_Omega |> 
  ggplot() +
  geom_point(aes(x = modality, 
                 y = Omega,
                 color = learningmodel,
                 fill = learningmodel),
             position = position_jitterdodge()) +
  geom_boxplot(aes(x = modality,
                   y = Omega, 
                   color = learningmodel),
               outlier.colour = NA) +
  scale_y_continuous(breaks = c(1, 7, 13, 19, 25, 31),
                     labels = c("1", "7", "13", "19", "25", "Inf")) +
  scale_color_manual(name = "Learning Model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  scale_fill_manual(name = "Learning Model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  theme_classic() +
  labs(title = "",
       subtitle = "Best fitting timescale of integration",
       x = "\nModality",
       y = "\U03C9\n") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "bottom",
        plot.subtitle = element_text(size=10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9, family = ""),
        axis.title.x = element_text(size = 9))

plot_LLhood_Omega

ggsave(filename = "plot_bestOmega.png",
       plot = plot_LLhood_Omega,
       width = 18,
       height = 10,
       path = "/Users/daniel.schlunegger/Desktop/Figures_iScience/",
       dpi = 300,
       bg = "transparent",
       units = "cm")

