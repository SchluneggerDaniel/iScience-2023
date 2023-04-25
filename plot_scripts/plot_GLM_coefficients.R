# Plot GLM Coefficients for all modalities and learning models. 

library(tidyverse)
library(patchwork)
library(ggtext)

load("processed_data/Results/df_GLM_absolute.Rdata")

# Filter for "auditory", "vestibular", and "visual" modalities separately
sub_auditory_TP <- dfabs_GLM  |> 
  filter(modality == "auditory",
         learningmodel == "TP") 

sub_auditory_IF <- dfabs_GLM  |> 
  filter(modality == "auditory",
         learningmodel == "IF")

sub_auditory_AF <- dfabs_GLM  |> 
  filter(modality == "auditory",
         learningmodel == "AF") 

sub_vestibular_TP <- dfabs_GLM |>
  filter(modality == "vestibular",
         learningmodel == "TP") 

sub_vestibular_IF <- dfabs_GLM |>
  filter(modality == "vestibular",
         learningmodel == "IF") 

sub_vestibular_AF <- dfabs_GLM |>
  filter(modality == "vestibular",
         learningmodel == "AF") 

sub_visual_TP <- dfabs_GLM |>
  filter(modality == "visual",
         learningmodel == "TP") 

sub_visual_IF <- dfabs_GLM |>
  filter(modality == "visual",
         learningmodel == "IF") 

sub_visual_AF <- dfabs_GLM |>
  filter(modality == "visual",
         learningmodel == "AF") 


# Extract estimates from "coef" list for specified variables
variables <- c("term", "estimate")

variables_to_extract_TP <- c("(Intercept)",
                          "stimulus", 
                          "intensity_ord.L",
                          "intensity_ord.Q",
                          "TP",
                          "intensity_ord.L:TP",
                          "intensity_ord.Q:TP")

variables_to_extract_IF <- c("(Intercept)",
                             "stimulus", 
                             "intensity_ord.L",
                             "intensity_ord.Q",
                             "IF",
                             "intensity_ord.L:IF",
                             "intensity_ord.Q:IF")

variables_to_extract_AF <- c("(Intercept)",
                             "stimulus", 
                             "intensity_ord.L",
                             "intensity_ord.Q",
                             "AF",
                             "intensity_ord.L:AF",
                             "intensity_ord.Q:AF")

# Extract Estimates for "auditory" modality
coefs_auditory_TP <- sub_auditory_TP |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_TP) |>
                      select(all_of(variables))))

coefs_auditory_IF <- sub_auditory_IF |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_IF) |>
                      select(all_of(variables))))

coefs_auditory_AF <- sub_auditory_AF |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_AF) |>
                      select(all_of(variables))))


# Extract Estimates for "vestibular" modality
coefs_vestibular_TP <- sub_vestibular_TP |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_TP) |>
                      select(all_of(variables))))

coefs_vestibular_IF <- sub_vestibular_IF |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_IF) |>
                      select(all_of(variables))))

coefs_vestibular_AF <- sub_vestibular_AF |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_AF) |>
                      select(all_of(variables))))

# Extract Estimates for "visual" modality
coefs_visual_TP <- sub_visual_TP |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_TP) |>
                      select(all_of(variables))))

coefs_visual_IF <- sub_visual_IF |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_IF) |>
                      select(all_of(variables))))

coefs_visual_AF <- sub_visual_AF |>
  select(subject, coef) |>
  mutate(coef = map(coef, ~filter(.x, term %in% variables_to_extract_AF) |>
                      select(all_of(variables))))


# Convert lists of tibbles to single tibbles
coefs_auditory_TP <- coefs_auditory_TP |>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

coefs_auditory_IF <- coefs_auditory_IF |>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

coefs_auditory_AF <- coefs_auditory_AF|>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

coefs_vestibular_TP <- coefs_vestibular_TP |>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

coefs_vestibular_IF <- coefs_vestibular_IF |>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

coefs_vestibular_AF <- coefs_vestibular_AF|>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

coefs_visual_TP <- coefs_visual_TP |>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

coefs_visual_IF <- coefs_visual_IF |>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

coefs_visual_AF <- coefs_visual_AF|>
  unnest(coef) |> 
  mutate(term = forcats::as_factor(term))

# palette
palette_TP <- c("#0B0405FF", "#2B1C35FF", "#3E356BFF", "#3B5698FF", "#2B758EFF", "#357BA2FF", "#359EAAFF")
palette_IF <- c("#0B0405FF", "#2B1C35FF", "#3E356BFF", "#3B5698FF", "#440154FF", "#357BA2FF", "#359EAAFF")
palette_AF <- c("#0B0405FF", "#2B1C35FF", "#3E356BFF", "#3B5698FF", "#FDE725FF", "#357BA2FF", "#359EAAFF")

# Auditory
plot_auditory_TP <- ggplot(coefs_auditory_TP, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "TP",
                              "TP × Intensity linear", 
                              "TP × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_TP) +
  theme_classic() +   
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Auditory, TP",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))

plot_auditory_IF <- ggplot(coefs_auditory_IF, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "IF",
                              "IF × Intensity linear", 
                              "IF × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_IF) +
  theme_classic() +
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Auditory, IF",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))

plot_auditory_AF <- ggplot(coefs_auditory_AF, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "AF",
                              "AF × Intensity linear", 
                              "AF × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_AF) +
  theme_classic() +
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Auditory, AF",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))

# Vestibular
plot_vestibular_TP <- ggplot(coefs_vestibular_TP, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "TP",
                              "TP × Intensity linear", 
                              "TP × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_TP) +
  theme_classic() +
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Vestibular, TP",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))

plot_vestibular_IF <- ggplot(coefs_vestibular_IF, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "IF",
                              "IF × Intensity linear", 
                              "IF × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_IF) +
  theme_classic() +
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Vestibular, IF",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))

plot_vestibular_AF <- ggplot(coefs_vestibular_AF, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "AF",
                              "AF × Intensity linear", 
                              "AF × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_AF) +
  theme_classic() +
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Vestibular, AF",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))

# Visual
plot_visual_TP <- ggplot(coefs_visual_TP, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "TP",
                              "TP × Intensity linear", 
                              "TP × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_TP) +
  theme_classic() +
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Visual, TP",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))

plot_visual_IF <- ggplot(coefs_visual_IF, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "IF",
                              "IF × Intensity linear", 
                              "IF × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_IF) +
  theme_classic() +
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Visual, IF",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))

plot_visual_AF <- ggplot(coefs_visual_AF, aes(x = term, y = estimate, color = term)) +
  geom_jitter(width = 0.1, size = 0.2) +
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, alpha = 0.85) +
  scale_x_discrete(labels = c("Intercept",
                              "Stimulus",
                              "Intensity linear",
                              "Intensity quadratic",
                              "AF",
                              "AF × Intensity linear", 
                              "AF × Intensity quadratic"),
                   guide = guide_axis(angle = 45)) +
  scale_color_manual(values = palette_AF) +
  theme_classic() +
  ylim(-2, 4) +
  labs(title = " ",
       subtitle = "Visual, AF",
       x = " ",
       y = expression(italic(beta))) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        legend.position = "none",
        plot.subtitle = element_text(size=10),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9))


layout_GLM_coefficients <- 
  "
     AAA
     BBB
     CCC
"

plot_GLM_coefficients <- ((plot_auditory_TP | plot_vestibular_TP | plot_visual_TP) /
                          (plot_auditory_IF | plot_vestibular_IF | plot_visual_IF) /
                          (plot_auditory_AF | plot_vestibular_AF | plot_visual_AF) +
                                  plot_layout(design = layout_GLM_coefficients, 
                                              guides = "collect", 
                                              nrow = 3, 
                                              ncol = 3)) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", color = NA),
                                panel.background = element_rect(fill = "transparent", color = NA)))



ggsave(filename = "plot_GLM_coefficients.png", 
       plot = plot_GLM_coefficients, 
       width = 18, 
       height = 22, 
       path = "/Users/daniel.schlunegger/Dropbox/LaTeX/iScience-2023/Figures",
       dpi = 300, 
       bg = "transparent", 
       units = "cm")
