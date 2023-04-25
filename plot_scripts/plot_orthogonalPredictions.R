# Plot orthogonal predicitions

# Plot A: Example sequence with difficult and easy trials
# Plot B: Plot to quantify the overall effect

library(fs)
library(tidyverse)
library(patchwork)
library(ggpubr) 


### === LOAD DATA === ###

file_list <- dir_ls("processed_data/LLhood/") |> 
  path_filter(" |Stim_loglik.Rda", invert = TRUE)

# Load all response-based model data at once
lapply(file_list, load, .GlobalEnv)

file_list_generative <- dir_ls("processed_data/LLhood_generative/")

# Load all stimulus-based model data at once
lapply(file_list_generative, load, .GlobalEnv)

# Check if omega are comparable!
cbind(auditoryIF_loglik$Omega, auditoryIF_loglik_generative$Omega)

# Example: Auditory IF, participant 7 (BD98), Omega = 3, Omega_generative = 4

Create_df_IF <- function(Omega, Omega_generative, df, df_generative) {
  
  newdf <- tibble(df$response,  
                  df$IF,
                  df_generative$IF) |> 
    rename(response = `df$response`,
           IF = `df$IF`,
           IF_generative = `df_generative$IF`)
  
  # Choose variables for plotting
  index <- 1:nrow(df)
  intensity_ord <- df$intensity_ord
  stimulus <- df$stimulus
  Omega <- rep(Omega, times = nrow(df))
  Omega_generative <- rep(Omega_generative, times = nrow(df))
  
  df <- tibble(index, stimulus, intensity_ord, Omega, Omega_generative, newdf)
  
  return(df)
  
}


indices_auditory <- 7

results_plotdf_auditory_IF <- map(indices_auditory, ~ Create_df_IF(auditoryIF_loglik$Omega[[.]],
                                                                   auditoryIF_loglik_generative$Omega[[.]],
                                                                   auditoryIF_loglik$data[[.]], 
                                                                   auditoryIF_loglik_generative$data[[.]]))
# Convert to dataframe
plotdf_auditory_IF <- as.data.frame(do.call(rbind, results_plotdf_auditory_IF))

# Backtransform the predictions with plogis for plotting!
plotdf_auditory_IF <- plotdf_auditory_IF |> 
  mutate(IF = plogis(IF),
         IF_generative = plogis(IF_generative))


# Take a subset of the data:
plotdf_auditory_IF <- plotdf_auditory_IF |> 
  filter(index %in% 155:170)

# Write function in order to change key_glyph
draw_key_custom <- function(data, params, size) {
  if (data$colour == "#5BFB72FF") {
    data$size <- 1
    draw_key_timeseries(data, params, size)
  } else if (data$colour == "#23C3E4FF") {
    data$size <- 1
    draw_key_timeseries(data, params, size)
  } else {
    data$size <- 2
    draw_key_point(data, params, size)
  }
}

palette <- c("#5BFB72FF","#23C3E4FF","#FCB036FF","#362160FF")

plotdf_auditory_IF <- plotdf_auditory_IF |> 
  mutate(stimulus = ifelse(test = stimulus == -0.5, yes = 0.1, no = 0.9),
         level = ifelse(test = intensity_ord >= 3, yes = "Easy trials", no = "Difficult trials")) |> 
  mutate(level = factor(level, levels = c("Easy trials", "Difficult trials")))

plot_oP_auditoryIF <- plotdf_auditory_IF |> ggplot() +
  geom_point(data = plotdf_auditory_IF, 
             aes(x = index, 
                 y = response),
             size = 2,
             key_glyph = "custom") +
  geom_point(data = plotdf_auditory_IF, 
             aes(x = index, 
                 y = stimulus,
                 colour = level),
             size = 2,
             key_glyph = "custom") +
  geom_line(data = plotdf_auditory_IF, 
            aes(x = index, 
                y = IF,
                colour = "Response-based \nprediction \U03C9 = 3"),
            linewidth = 1,
            key_glyph = "custom") +
  geom_line(data = plotdf_auditory_IF, 
            aes(x = index, 
                y = IF_generative,
                colour = "Stimulus-based \nprediction \U03C9 = 4"),
            linewidth = 1,
            key_glyph = "custom") +
  geom_hline(yintercept = 0.5, linetype = "dashed", linewidth = 0.25) +
  geom_vline(xintercept = c(157, 158, 160, 162, 164), linetype = "dashed", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(155, 170, 1)) +
  scale_y_continuous(name = "Prediction",
                     breaks = 0:1,
                     sec.axis = sec_axis(~.,name="Responses/Stimuli",
                                         breaks = c(0, 0.1, 0.9, 1),
                                         labels = c("Response A","Stimulus A"," Stimulus B", "Response B"))) +
  scale_color_manual(name = "Levels/Predictions", 
                     values = c("Response-based \nprediction \U03C9 = 3" = palette[1], 
                                "Stimulus-based \nprediction \U03C9 = 4" = palette[2],
                                "Easy trials" = palette[3],
                                "Difficult trials" = palette[4])) +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(shape = c(16,16,16,16), 
                              color = c("#362160FF","#FCB036FF", "#5BFB72FF","#23C3E4FF")))) +
  labs(title = "A",
       subtitle = "Example of orthogonal predictions between response-based and stimulus-based model",
       x = "\nTrial-Index",
       y = " ") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "bottom",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y.right = element_text(angle = 90))





### === LOAD DATA === ###

file_list <- dir_ls("processed_data/LLhood_orthogonalPredictions/")

# Load all response-based model data at once
lapply(file_list, load, .GlobalEnv)


# Combine dataframes for plotting
auditoryOrtho <- tibble(orthogonalTP_auditory, orthogonalIF_auditory, orthogonalAF_auditory) |> 
  pivot_longer(cols = everything(), names_to = "learningmodel", values_to = "Ratio") |> 
  mutate(learningmodel = as_factor(learningmodel),
         modality = "auditory") |> 
  relocate(modality, .before = "learningmodel")

levels(auditoryOrtho$learningmodel) <- list("Transition Probability" = "ratioTP",
                                            "Item Frequency" = "ratioIF",
                                            "Alternation Frequency" = "ratioAF")

vestibularOrtho <- tibble(orthogonalTP_vestibular, orthogonalIF_vestibular, orthogonalAF_vestibular) |> 
  pivot_longer(cols = everything(), names_to = "learningmodel", values_to = "Ratio") |> 
  mutate(learningmodel = as_factor(learningmodel),
         modality = "vestibular") |> 
  relocate(modality, .before = "learningmodel")

levels(vestibularOrtho$learningmodel) <- list("Transition Probability" = "ratioTP",
                                              "Item Frequency" = "ratioIF",
                                              "Alternation Frequency" = "ratioAF")

visualOrtho <- tibble(orthogonalTP_visual, orthogonalIF_visual, orthogonalAF_visual) |> 
  pivot_longer(cols = everything(), names_to = "learningmodel", values_to = "Ratio") |> 
  mutate(learningmodel = as_factor(learningmodel),
         modality = "visual") |> 
  relocate(modality, .before = "learningmodel")

levels(visualOrtho$learningmodel) <- list("Transition Probability" = "ratioTP",
                                          "Item Frequency" = "ratioIF",
                                          "Alternation Frequency" = "ratioAF")

dfOrtho <- rbind(auditoryOrtho, vestibularOrtho, visualOrtho)


plot_oP_quantify <- dfOrtho |> 
  ggboxplot(x = "modality",
            y = "Ratio",
            color = "learningmodel",
            outlier.shape = NA) +
  ylim(0, 4.2) +
  scale_color_manual(name = "Learning Model",
                     values = c("Transition Probability" = "#2B758EFF",
                                "Item Frequency" = "#440154FF",
                                "Alternation Frequency" = "#FDE725FF")) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.2) +
  theme_classic() +
  labs(title = "B",
       subtitle = "Quantifying orthogonal predictions between response-based and stimulus-based model",
       x = "\n Modality",
       y = "Ratio\n") +
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



# Patchwork
plotOrtho <- (plot_oP_auditoryIF | plot_oP_quantify) +
  plot_layout(guides = "auto", nrow = 2)  +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", color = NA),
                                panel.background = element_rect(fill = "transparent", color = NA))) 


ggsave(filename = "plot_orthogonalPredictions.png",
       plot = plotOrtho,
       width = 18,
       height = 18,
       path = "/Users/daniel.schlunegger/Dropbox/LaTeX/iScience-2023/Figures/",
       dpi = 300,
       bg = "transparent",
       units = "cm")

