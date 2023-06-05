# Plot all Likelihood Plots

# Initial comparison (i.e. response-based model vs. generative-based model)

# 1. delta in LLhood
# 2. absolute in LLhood
# 3. constantOmega in LLhood

library(tidyverse)
library(patchwork)
library(ggpubr) 
library(rstatix)

### === DELTA LL HOOD === ###

load("processed_data/Results/df_LLhood_delta.Rdata")

### === PLOT === ###

# Prepare
levels(delta_LLhood_EvD$model) <- list("Transition Probability" = "TP",
                                   "Item Frequency" = "IF",
                                   "Alternation Frequency" = "AF")

# Create Subdataframes 
delta_LLhood_EvD_auditory <- delta_LLhood_EvD |> 
  filter(modality %in% "auditory")

delta_LLhood_EvD_vestibular <- delta_LLhood_EvD |> 
  filter(modality %in% "vestibular")

delta_LLhood_EvD_visual <- delta_LLhood_EvD |> 
  filter(modality %in% "visual")


# Auditory Plot
p1_auditoryDelta <- delta_LLhood_EvD_auditory |> 
  ggplot(aes(x = level,
             y = mean_difference,
             color = model,
             shape = Significance)) +
  geom_errorbar(aes(ymin=mean_difference - (SEM), 
                    ymax=mean_difference + (SEM)), width=.2,
                position=position_dodge(0.35)) +
  geom_line(aes(group = model),
            position=position_dodge(width=0.35),
            key_glyph = "crossbar") +
  geom_point(position=position_dodge(width=0.35)) +
  scale_color_manual(name = "Learning Model", 
                     values = c("Transition Probability" = "#2B758EFF", 
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  scale_shape_discrete(name= "Responses-based \nvs Stimulus-based", 
                       labels = c("Difference significant", "Difference non-significant")) +
  theme_classic() +
  labs(title = "A",
       subtitle = "Auditory",
       x = " ",
       y = "\U0394 log-likelihood \n <- Response-based is better \n Stimulus-based is better ->") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "bottom",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))
 

# Vestibular Plot
p1_vestibularDelta <- delta_LLhood_EvD_vestibular |> 
  ggplot(aes(x = level,
             y = mean_difference,
             color = model,
             shape = Significance)) +
  geom_errorbar(aes(ymin=mean_difference - (SEM), 
                    ymax=mean_difference + (SEM)), width=.2,
                position=position_dodge(0.35)) +
  geom_line(aes(group = model),
            position=position_dodge(width=0.35)) +
  geom_point(position=position_dodge(width=0.35)) +
  scale_color_manual(name = "Learning Model", 
                     values = c("Transition Probability" = "#2B758EFF", 
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  scale_shape_discrete(name= "Responses-based \nvs Stimulus-based", 
                       labels = c("Difference significant", "Difference non-significant")) +
  theme_classic() +
  labs(title = " ",
       subtitle = "Vestibular",
       x = "\nStimuli",
       y = " ") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))



# Visual Plot
p1_visualDelta <- delta_LLhood_EvD_visual |> 
  ggplot(aes(x = level,
             y = mean_difference,
             color = model,
             shape = Significance)) +
  geom_errorbar(aes(ymin=mean_difference - (SEM), 
                    ymax=mean_difference + (SEM)), width=.2,
                position=position_dodge(0.35)) +
  geom_line(aes(group = model),
            position=position_dodge(width=0.35)) +
  geom_point(position=position_dodge(width=0.35)) +
  scale_color_manual(name = "Learning Model", 
                     values = c("Transition Probability" = "#2B758EFF", 
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  scale_shape_discrete(name= "Responses-based \nvs Stimulus-based", 
                       labels = c("Difference significant", "Difference non-significant")) +
  theme_classic() +
  labs(title = " ",
       subtitle = "Visual",
       x = " ",
       y = "") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))





### === LOAD DATA === ###

load("processed_data/Results/df_LLhood_absolute.Rdata")
load("processed_data/Results/df_LLhood_sig_absolute.Rdata")

# Relevel factor
dfabs_LLhood$level <- factor(dfabs_LLhood$level, levels = c("easy", "difficult"))

# Change factor labels 
levels(dfabs_LLhood$learningmodel) <- list("Transition Probability" = "TP",
                                           "Item Frequency" = "IF",
                                           "Alternation Frequency" = "AF")

# Create subdataframes for modality 
dfabs_auditory <- dfabs_LLhood |> 
  filter(modality %in% "auditory")

dfabs_vestibular <- dfabs_LLhood |> 
  filter(modality %in% "vestibular")

dfabs_visual <- dfabs_LLhood |> 
  filter(modality %in% "visual")


# Auditory 
p1_auditoryAbsolute <- dfabs_auditory |> 
  ggboxplot(x = "level",
            y = "loglikelihood_centered", 
            color = "learningmodel",
            size = 0.35, 
            alpha = 1,
            outlier.shape = NA) +
  scale_color_manual(name = "Learning Model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  theme_classic() +
  labs(title = "B",
       subtitle = " ",
       x = " ",
       y = "\nlog-likelihood \ncentered") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))


# only significant comparisons are plotted according to absolute_LLhood_learningmodels
p1_auditoryAbsolute <- p1_auditoryAbsolute +
  geom_bracket(xmin = 0.735, xmax = 0.985, y.position = 42, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "auditory" &
                                                               absolute_LLhood_learningmodels$level == "easy" &
                                                               absolute_LLhood_learningmodels$comparison == "TP vs IF"], 
              size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.015, xmax = 1.265, y.position = 42, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "auditory" &
                                                               absolute_LLhood_learningmodels$level == "easy" &
                                                               absolute_LLhood_learningmodels$comparison == "IF vs AF"], 
              size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 0.735, xmax = 1.265, y.position = 52, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "auditory" &
                                                               absolute_LLhood_learningmodels$level == "easy" &
                                                               absolute_LLhood_learningmodels$comparison == "TP vs AF"], 
              size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.715, xmax = 1.985, y.position = 60, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "auditory" &
                                                               absolute_LLhood_learningmodels$level == "difficult" &
                                                               absolute_LLhood_learningmodels$comparison == "TP vs IF"], 
              size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.715, xmax = 2.27,  y.position = 70, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "auditory" &
                                                               absolute_LLhood_learningmodels$level == "difficult" &
                                                               absolute_LLhood_learningmodels$comparison == "TP vs AF"], 
              size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) 
 


# Vestibular
p1_vestibularAbsolute <- dfabs_vestibular |> 
  ggboxplot(x = "level",
            y = "loglikelihood_centered", 
            color = "learningmodel",
            size = 0.35, 
            alpha = 1,
            outlier.shape = NA) +
  ylim(-30, 55) +
  scale_color_manual(name = "Learning Model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  theme_classic() +
  labs(title = " ",
       subtitle = " ",
       x = "\nStimuli",
       y = " ") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))


# only significant comparisons are plotted according to absolute_LLhood_learningmodels
p1_vestibularAbsolute <- p1_vestibularAbsolute +
  geom_bracket(xmin = 1.735, xmax = 2, y.position = 35, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "vestibular" &
                                                                 absolute_LLhood_learningmodels$level == "difficult" &
                                                                 absolute_LLhood_learningmodels$comparison == "TP vs IF"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.735, xmax = 2.27,  y.position = 42, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "vestibular" &
                                                                 absolute_LLhood_learningmodels$level == "difficult" &
                                                                 absolute_LLhood_learningmodels$comparison == "TP vs AF"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) 




# Visual
p1_visualAbsolute <- dfabs_visual |> 
  ggboxplot(x = "level",
            y = "loglikelihood_centered", 
            color = "learningmodel",
            size = 0.35, 
            alpha = 1,
            outlier.shape = NA) +
  ylim(-50, 180) +
  scale_color_manual(name = "Learning Model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  theme_classic() +
  labs(title = " ",
       subtitle = " ",
       x = " ",
       y = " ") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))


# only significant comparisons are plotted according to absolute_LLhood_learningmodels
p1_visualAbsolute <- p1_visualAbsolute +
  geom_bracket(xmin = 1.015, xmax = 1.265, y.position = 145, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "visual" &
                                                                 absolute_LLhood_learningmodels$level == "easy" &
                                                                 absolute_LLhood_learningmodels$comparison == "IF vs AF"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 0.735, xmax = 1.265, y.position = 164, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "visual" &
                                                                 absolute_LLhood_learningmodels$level == "easy" &
                                                                 absolute_LLhood_learningmodels$comparison == "TP vs AF"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.735, xmax = 2, y.position = 58, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "visual" &
                                                                 absolute_LLhood_learningmodels$level == "difficult" &
                                                                 absolute_LLhood_learningmodels$comparison == "TP vs IF"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) +
  geom_bracket(xmin = 1.735, xmax = 2.27,  y.position = 98, 
               label = absolute_LLhood_learningmodels$p.symbol[absolute_LLhood_learningmodels$modality == "visual" &
                                                                 absolute_LLhood_learningmodels$level == "difficult" &
                                                                 absolute_LLhood_learningmodels$comparison == "TP vs AF"], 
               size = 0.2, label.size = 2, vjust = 0.5, tip.length = 0.02) 





### === LOAD DATA === ###

load("processed_data/LLhood_LearningModels/auditory_learningModels.Rda")
load("processed_data/LLhood_LearningModels/vestibular_learningModels.Rda")
load("processed_data/LLhood_LearningModels/visual_learningModels.Rda")


# Remove Stim-Only Model for plotting purposes and change levels

auditory_LearningModels <- auditory_LearningModels |> 
  filter(!(learningmodel == "Stim")) |> 
  droplevels()

levels(auditory_LearningModels$learningmodel) <- list("Transition Probability" = "TP",
                                                      "Item Frequency" = "IF",
                                                      "Alternation Frequency" = "AF")


vestibular_LearningModels <- vestibular_LearningModels |> 
  filter(!(learningmodel == "Stim")) |> 
  droplevels()

levels(vestibular_LearningModels$learningmodel) <- list("Transition Probability" = "TP",
                                                        "Item Frequency" = "IF",
                                                        "Alternation Frequency" = "AF")

visual_LearningModels <- visual_LearningModels |> 
  filter(!(learningmodel == "Stim")) |> 
  droplevels()

levels(visual_LearningModels$learningmodel) <- list("Transition Probability" = "TP",
                                                    "Item Frequency" = "IF",
                                                    "Alternation Frequency" = "AF")


# Auditory 
p1_auditoryOmega <- auditory_LearningModels |> 
  ggplot(aes(x = Omega,
             y = LLhood,
             ymin = LLhood - (SEM_LLhood),
             ymax = LLhood + (SEM_LLhood),
             color = learningmodel,
             fill = learningmodel)) +
  geom_line() +
  geom_ribbon(alpha = 0.15, colour = NA) +
  scale_x_continuous(breaks = c(1, 7, 13, 19, 25, 31),
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
  labs(title = "C",
       subtitle = " ",
       x = bquote(italic(omega)["response"]),
       y = "log-likelihood") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 10))



# Vestibular
p1_vestibularOmega <- vestibular_LearningModels |> 
  ggplot(aes(x = Omega,
             y = LLhood,
             ymin = LLhood - (SEM_LLhood),
             ymax = LLhood + (SEM_LLhood),
             color = learningmodel,
             fill = learningmodel)) +
  geom_line() +
  geom_ribbon(alpha = 0.15, colour = NA) +
  scale_x_continuous(breaks = c(1, 7, 13, 19, 25, 31),
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
       subtitle = " ",
       x = bquote(italic(omega)["response"]),
       y = " ") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 10))




# Visual
p1_visualOmega <- visual_LearningModels |> 
  ggplot(aes(x = Omega,
             y = LLhood,
             ymin = LLhood - (SEM_LLhood),
             ymax = LLhood + (SEM_LLhood),
             color = learningmodel,
             fill = learningmodel)) +
  geom_line() +
  geom_ribbon(alpha = 0.15, colour = NA) +
  scale_x_continuous(breaks = c(1, 7, 13, 19, 25, 31),
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
       subtitle = " ",
       x = bquote(italic(omega)["response"]),
       y = " ") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 10))


### === PATCHWORK === ###



layout_LLhood <- 
  "
  AAA
  BBB
  CCC
  DDD
    
"



plot_LLhood<- ((p1_auditoryDelta | p1_vestibularDelta | p1_visualDelta) /
                          (p1_auditoryAbsolute | p1_vestibularAbsolute | p1_visualAbsolute) /
                          (p1_auditoryOmega | p1_vestibularOmega | p1_visualOmega) /
                 guide_area() +
                          plot_layout(design = layout_LLhood, 
                                      guides = "collect", nrow = 4, ncol = 3))  +
  plot_annotation()


ggsave(filename = "Figure3.tiff",
       plot = plot_LLhood,
       width = 18,
       height = 22,
       path = "/Users/daniel.schlunegger/Desktop/Figures_iScience/",
       dpi = 300,
       bg = "white",
       units = "cm")

