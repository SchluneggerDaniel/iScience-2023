# Conditional probabilities and corresponding correlation matrices for conditions

library(tidyverse)
library(patchwork)
library(ggcorrplot)
library(ggtext)

### === OVERVIEW === ###

# First part of the script creates the plots for the psychometric curves
# Second part of the script creates corresponding correlation matrices

# Before this script: data_preprocessing_psychometricVisual.R

### === FIRST PART === ###

source("functions/CountConditionalProbabilitiesExtended.R")

# Data pre-processing 
load("processed_data/allValues/allValuesVisual.Rda")

# Remove variables 
df_visual <- allValuesVisual |> 
  select(-(starts_with("pred")))

# Visual
df_visual <- df_visual |> 
  mutate(rightresponse = response,
         leftresponse = if_else(response == 0, 1, 0)) |> 
  tibble()

# Create subdataframes
df_visual_neut <- df_visual |> 
  filter(condition %in% "Neutral")

df_visual_rep <- df_visual |> 
  filter(condition %in% "Repetitive")

df_visual_alt <- df_visual |> 
  filter(condition %in% "Alternating")



# Condition on the previous response
df_function_neut <- CountConditionalProbabilityExtended(df_visual_neut$response)
df_function_rep <- CountConditionalProbabilityExtended(df_visual_rep$response)
df_function_alt <- CountConditionalProbabilityExtended(df_visual_alt$response)

df_neut <- bind_cols(df_visual_neut, df_function_neut)
df_rep <- bind_cols(df_visual_rep, df_function_rep)
df_alt <- bind_cols(df_visual_alt, df_function_alt)

df_neut <- df_neut |> 
  mutate(across(c(starts_with("given")), factor))

df_rep <- df_rep |> 
  mutate(across(c(starts_with("given")), factor))

df_alt <- df_alt |> 
  mutate(across(c(starts_with("given")), factor))


# Subset dataframes (you need 5 dataframes, for 1 plot)

filtergivenAA <- function(x){x <- x |> filter(x$givenAA %in% "1")}
filtergivenAB <- function(x){x <- x |> filter(x$givenAB %in% "1")}
filtergivenBA <- function(x){x <- x |> filter(x$givenBA %in% "1")}
filtergivenBB <- function(x){x <- x |> filter(x$givenBB %in% "1")}

# Fully stochastic
df_givenAA_neut <- filtergivenAA(df_neut)
df_givenAB_neut <- filtergivenAB(df_neut)
df_givenBA_neut <- filtergivenBA(df_neut)
df_givenBB_neut <- filtergivenBB(df_neut)

# Repetition-bised
df_givenAA_rep <- filtergivenAA(df_rep)
df_givenAB_rep <- filtergivenAB(df_rep)
df_givenBA_rep <- filtergivenBA(df_rep)
df_givenBB_rep <- filtergivenBB(df_rep)

# Alternation-biased
df_givenAA_alt <- filtergivenAA(df_alt)
df_givenAB_alt <- filtergivenAB(df_alt)
df_givenBA_alt <- filtergivenBA(df_alt)
df_givenBB_alt <- filtergivenBB(df_alt)


# Fully stochastic
df1_all_neut <- df_neut |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAA_neut <- df_givenAA_neut |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAB_neut <- df_givenAB_neut |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBA_neut <- df_givenBA_neut |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBB_neut <- df_givenBB_neut |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))


# Repetition-biased
df1_all_rep <- df_rep |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAA_rep <- df_givenAA_rep |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAB_rep <- df_givenAB_rep |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBA_rep <- df_givenBA_rep |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBB_rep <- df_givenBB_rep |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))


# Alternation-biased
df1_all_alt <- df_alt |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAA_alt <- df_givenAA_alt |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAB_alt <- df_givenAB_alt |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBA_alt <- df_givenBA_alt |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBB_alt <- df_givenBB_alt |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))



# Fully stochastic: Add proportion
df1_all_neut$pRight <- df1_all_neut$nRight / (df1_all_neut$nRight + df1_all_neut$nLeft)
df1_givenAA_neut$pRight <- df1_givenAA_neut$nRight / (df1_givenAA_neut$nRight + df1_givenAA_neut$nLeft)
df1_givenAB_neut$pRight <- df1_givenAB_neut$nRight / (df1_givenAB_neut$nRight + df1_givenAB_neut$nLeft)
df1_givenBA_neut$pRight <- df1_givenBA_neut$nRight / (df1_givenBA_neut$nRight + df1_givenBA_neut$nLeft)
df1_givenBB_neut$pRight <- df1_givenBB_neut$nRight / (df1_givenBB_neut$nRight + df1_givenBB_neut$nLeft)

# Repetition-baised: Add proportion
df1_all_rep$pRight <- df1_all_rep$nRight / (df1_all_rep$nRight + df1_all_rep$nLeft)
df1_givenAA_rep$pRight <- df1_givenAA_rep$nRight / (df1_givenAA_rep$nRight + df1_givenAA_rep$nLeft)
df1_givenAB_rep$pRight <- df1_givenAB_rep$nRight / (df1_givenAB_rep$nRight + df1_givenAB_rep$nLeft)
df1_givenBA_rep$pRight <- df1_givenBA_rep$nRight / (df1_givenBA_rep$nRight + df1_givenBA_rep$nLeft)
df1_givenBB_rep$pRight <- df1_givenBB_rep$nRight / (df1_givenBB_rep$nRight + df1_givenBB_rep$nLeft)

# Alternation-baised: Add proportion
df1_all_alt$pRight <- df1_all_alt$nRight / (df1_all_alt$nRight + df1_all_alt$nLeft)
df1_givenAA_alt$pRight <- df1_givenAA_alt$nRight / (df1_givenAA_alt$nRight + df1_givenAA_alt$nLeft)
df1_givenAB_alt$pRight <- df1_givenAB_alt$nRight / (df1_givenAB_alt$nRight + df1_givenAB_alt$nLeft)
df1_givenBA_alt$pRight <- df1_givenBA_alt$nRight / (df1_givenBA_alt$nRight + df1_givenBA_alt$nLeft)
df1_givenBB_alt$pRight <- df1_givenBB_alt$nRight / (df1_givenBB_alt$nRight + df1_givenBB_alt$nLeft)



# p1: Neutral

df1_all_neut$Responses <- factor("all")
df1_givenAA_neut$Responses <- factor("preceding AA")
df1_givenAB_neut$Responses <- factor("preceding AB")
df1_givenBA_neut$Responses <- factor("preceding BA")
df1_givenBB_neut$Responses <- factor("preceding BB")


df1_neut <- rbind(df1_givenBB_neut, df1_givenAB_neut, df1_all_neut, df1_givenBA_neut, df1_givenAA_neut)


p1_visual <- ggplot() +
  geom_point(data = df1_neut, aes(x = intensity, y = pRight, colour = Responses), show.legend = F)



# Fit glm
model_all_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_neut, family = binomial())
model_givenAA_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_neut, family = binomial())
model_givenAB_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_neut, family = binomial())
model_givenBA_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_neut, family = binomial())
model_givenBB_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_neut, family = binomial())

# Create data points
xseq <- seq(-0.6, 0.6, len = 1000)

# Predict
yseq_all_neut <- predict(model_all_neut, data.frame(intensity = xseq), type = "response")
yseq_givenAA_neut <- predict(model_givenAA_neut, data.frame(intensity = xseq), type = "response")
yseq_givenAB_neut <- predict(model_givenAB_neut, data.frame(intensity = xseq), type = "response")
yseq_givenBA_neut <- predict(model_givenBA_neut, data.frame(intensity = xseq), type = "response")
yseq_givenBB_neut <- predict(model_givenBB_neut, data.frame(intensity = xseq), type = "response")

# Curve
curve_all_neut <- data.frame(xseq, yseq_all_neut)
curve_givenAA_neut <- data.frame(xseq, yseq_givenAA_neut)
curve_givenAB_neut <- data.frame(xseq, yseq_givenAB_neut)
curve_givenBA_neut <- data.frame(xseq, yseq_givenBA_neut)
curve_givenBB_neut <- data.frame(xseq, yseq_givenBB_neut)

names(curve_all_neut)[names(curve_all_neut) == 'yseq_all_neut'] <- 'yseq'
names(curve_givenAA_neut)[names(curve_givenAA_neut) == 'yseq_givenAA_neut'] <- 'yseq'
names(curve_givenAB_neut)[names(curve_givenAB_neut) == 'yseq_givenAB_neut'] <- 'yseq'
names(curve_givenBA_neut)[names(curve_givenBA_neut) == 'yseq_givenBA_neut'] <- 'yseq'
names(curve_givenBB_neut)[names(curve_givenBB_neut) == 'yseq_givenBB_neut'] <- 'yseq'

curve_all_neut$Responses <- factor("all")
curve_givenAA_neut$Responses <- factor("preceding AA")
curve_givenAB_neut$Responses <- factor("preceding AB")
curve_givenBA_neut$Responses <- factor("preceding BA")
curve_givenBB_neut$Responses <- factor("preceding BB")

curve_neut <- rbind(curve_givenBB_neut, curve_givenAB_neut, curve_all_neut, curve_givenBA_neut, curve_givenAA_neut)

p1_visual <- p1_visual +
  geom_line(data = curve_neut, aes(x = xseq, y = yseq, colour = Responses)) +
  theme_classic() + 
  geom_hline(aes(yintercept = 0.5), linetype = 3, alpha = 0.75) +
  geom_vline(aes(xintercept = 0), linetype = 3, alpha = 0.75) +  
  xlab(" ") +
  ylab("P(<i>B</i>)") +
  labs(title = "A",
       subtitle = "Neutral") +
  scale_color_manual(name = "Responses",
                     labels = c("preceding BB",
                                "preceding AB",
                                "all",
                                "preceding BA",
                                "preceding AA"),
                     values = c("#FDE725FF", "#5DC863FF", "black", "#3B528BFF", "#440154FF")) +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(-0.6,0,0.6)) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))


p1_visual <- p1_visual + theme(axis.title.y = element_markdown())







# p2: Repetition-baised

df1_all_rep$Responses <- factor("all")
df1_givenAA_rep$Responses <- factor("preceding AA")
df1_givenAB_rep$Responses <- factor("preceding AB")
df1_givenBA_rep$Responses <- factor("preceding BA")
df1_givenBB_rep$Responses <- factor("preceding BB")


df1_rep <- rbind(df1_givenBB_rep, df1_givenAB_rep, df1_all_rep, df1_givenBA_rep, df1_givenAA_rep)


p2_visual <- ggplot() +
  geom_point(data = df1_rep, aes(x = intensity, y = pRight, colour = Responses), show.legend = F)




# Fit glm
model_all_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_rep, family = binomial())
model_givenAA_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_rep, family = binomial())
model_givenAB_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_rep, family = binomial())
model_givenBA_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_rep, family = binomial())
model_givenBB_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_rep, family = binomial())

# Create data points
xseq <- seq(-0.6, 0.6, len = 1000)

# Predict
yseq_all_rep <- predict(model_all_rep, data.frame(intensity = xseq), type = "response")
yseq_givenAA_rep <- predict(model_givenAA_rep, data.frame(intensity = xseq), type = "response")
yseq_givenAB_rep <- predict(model_givenAB_rep, data.frame(intensity = xseq), type = "response")
yseq_givenBA_rep <- predict(model_givenBA_rep, data.frame(intensity = xseq), type = "response")
yseq_givenBB_rep <- predict(model_givenBB_rep, data.frame(intensity = xseq), type = "response")

# Curve
curve_all_rep <- data.frame(xseq, yseq_all_rep)
curve_givenAA_rep <- data.frame(xseq, yseq_givenAA_rep)
curve_givenAB_rep <- data.frame(xseq, yseq_givenAB_rep)
curve_givenBA_rep <- data.frame(xseq, yseq_givenBA_rep)
curve_givenBB_rep <- data.frame(xseq, yseq_givenBB_rep)

names(curve_all_rep)[names(curve_all_rep) == 'yseq_all_rep'] <- 'yseq'
names(curve_givenAA_rep)[names(curve_givenAA_rep) == 'yseq_givenAA_rep'] <- 'yseq'
names(curve_givenAB_rep)[names(curve_givenAB_rep) == 'yseq_givenAB_rep'] <- 'yseq'
names(curve_givenBA_rep)[names(curve_givenBA_rep) == 'yseq_givenBA_rep'] <- 'yseq'
names(curve_givenBB_rep)[names(curve_givenBB_rep) == 'yseq_givenBB_rep'] <- 'yseq'

curve_all_rep$Responses <- factor("all")
curve_givenAA_rep$Responses <- factor("preceding AA")
curve_givenAB_rep$Responses <- factor("preceding AB")
curve_givenBA_rep$Responses <- factor("preceding BA")
curve_givenBB_rep$Responses <- factor("preceding BB")

curve_rep <- rbind(curve_givenBB_rep, curve_givenAB_rep, curve_all_rep, curve_givenBA_rep, curve_givenAA_rep)

p2_visual <- p2_visual +
  geom_line(data = curve_rep, aes(x = xseq, y = yseq, colour = Responses)) +
  theme_classic() + 
  geom_hline(aes(yintercept = 0.5), linetype = 3, alpha = 0.75) +
  geom_vline(aes(xintercept = 0), linetype = 3, alpha = 0.75) +  
  xlab("Motion coherence") +
  ylab(" ") +
  labs(title = "B",
       subtitle = "Repetitive") +
  scale_color_manual(name = "Responses",
                     labels = c("preceding BB",
                                "preceding AB",
                                "all",
                                "preceding BA",
                                "preceding AA"),
                     values = c("#FDE725FF", "#5DC863FF", "black", "#3B528BFF", "#440154FF")) +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(-0.6,0,0.6)) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))


p2_visual <- p2_visual + theme(axis.title.y = element_markdown())



# p3: Alternation-baised

df1_all_alt$Responses <- factor("all")
df1_givenAA_alt$Responses <- factor("preceding AA")
df1_givenAB_alt$Responses <- factor("preceding AB")
df1_givenBA_alt$Responses <- factor("preceding BA")
df1_givenBB_alt$Responses <- factor("preceding BB")


df1_alt <- rbind(df1_givenBB_alt, df1_givenAB_alt, df1_all_alt, df1_givenBA_alt, df1_givenAA_alt)


p3 <- ggplot() +
  geom_point(data = df1_alt, aes(x = intensity, y = pRight, colour = Responses), show.legend = F)




# Fit glm
model_all_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_alt, family = binomial())
model_givenAA_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_alt, family = binomial())
model_givenAB_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_alt, family = binomial())
model_givenBA_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_alt, family = binomial())
model_givenBB_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_alt, family = binomial())

# Create data points
xseq <- seq(-0.6, 0.6, len = 1000)

# Predict
yseq_all_alt <- predict(model_all_alt, data.frame(intensity = xseq), type = "response")
yseq_givenAA_alt <- predict(model_givenAA_alt, data.frame(intensity = xseq), type = "response")
yseq_givenAB_alt <- predict(model_givenAB_alt, data.frame(intensity = xseq), type = "response")
yseq_givenBA_alt <- predict(model_givenBA_alt, data.frame(intensity = xseq), type = "response")
yseq_givenBB_alt <- predict(model_givenBB_alt, data.frame(intensity = xseq), type = "response")

# Curve
curve_all_alt <- data.frame(xseq, yseq_all_alt)
curve_givenAA_alt <- data.frame(xseq, yseq_givenAA_alt)
curve_givenAB_alt <- data.frame(xseq, yseq_givenAB_alt)
curve_givenBA_alt <- data.frame(xseq, yseq_givenBA_alt)
curve_givenBB_alt <- data.frame(xseq, yseq_givenBB_alt)

names(curve_all_alt)[names(curve_all_alt) == 'yseq_all_alt'] <- 'yseq'
names(curve_givenAA_alt)[names(curve_givenAA_alt) == 'yseq_givenAA_alt'] <- 'yseq'
names(curve_givenAB_alt)[names(curve_givenAB_alt) == 'yseq_givenAB_alt'] <- 'yseq'
names(curve_givenBA_alt)[names(curve_givenBA_alt) == 'yseq_givenBA_alt'] <- 'yseq'
names(curve_givenBB_alt)[names(curve_givenBB_alt) == 'yseq_givenBB_alt'] <- 'yseq'

curve_all_alt$Responses <- factor("all")
curve_givenAA_alt$Responses <- factor("preceding AA")
curve_givenAB_alt$Responses <- factor("preceding AB")
curve_givenBA_alt$Responses <- factor("preceding BA")
curve_givenBB_alt$Responses <- factor("preceding BB")

curve_alt <- rbind(curve_givenBB_alt, curve_givenAB_alt, curve_all_alt, curve_givenBA_alt, curve_givenAA_alt)

p3_visual <- p3 +
  geom_line(data = curve_alt, aes(x = xseq, y = yseq, colour = Responses)) +
  theme_classic() + 
  geom_hline(aes(yintercept = 0.5), linetype = 3, alpha = 0.75) +
  geom_vline(aes(xintercept = 0), linetype = 3, alpha = 0.75) +  
  xlab("Motion coherence") +
  ylab("P(<i>B</i>)") +
  labs(title = "C",
       subtitle = "Alternating") +
  scale_color_manual(name = "Responses",
                     labels = c("preceding BB",
                                "preceding AB",
                                "all",
                                "preceding BA",
                                "preceding AA"),
                     values = c("#FDE725FF", "#5DC863FF", "black", "#3B528BFF", "#440154FF")) +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(-0.6,0,0.6)) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))


p3_visual <- p3_visual + theme(axis.title.y = element_markdown())





p4_visual <- guide_area() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        plot.subtitle = element_text(size=10),
        legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))


layout_visualPlot <- 
  "
     AB
     CD
"


visualPatchwork <- (p1_visual + p2_visual + p3_visual + p4_visual) +
  plot_layout(design = layout_visualPlot, guides = "collect") +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", color = NA),
                                panel.background = element_rect(fill = "transparent", color = NA),
                                legend.background = element_rect(fill = "transparent", color = NA)))





visualPatchwork




ggsave(filename = "plot_visualShift.png", 
       plot = visualPatchwork, 
       width = 18, 
       height = 18, 
       path = "/Users/daniel.schlunegger/Dropbox/LaTeX/iScience-2023/Figures",
       dpi = 300, 
       bg = "transparent", 
       units = "cm")

