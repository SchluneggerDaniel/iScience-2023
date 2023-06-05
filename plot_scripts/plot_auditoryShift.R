# Conditional probabilities and corresponding correlation matrices for conditions

# Note: # factor levels are crucial for correct plotting!

library(tidyverse)
library(patchwork)
library(ggtext)


source("functions/CountConditionalProbabilitiesExtended.R")

# Data pre-processing 
load("processed_data/allValues/allValuesAuditory.Rda")

# Remove variables 
df_auditory <- allValuesAuditory |> 
  select(-(starts_with("pred")))

# Auditory
df_auditory <- df_auditory |> 
  mutate(rightresponse = response,
         leftresponse = if_else(response == 0, 1, 0)) |> 
  tibble()

# Create subdataframes
df_auditory_fs <- df_auditory |> 
  filter(condition %in% "Fully stochastic")

df_auditory_freq <- df_auditory |> 
  filter(condition %in% "Frequency-biased")

df_auditory_rep <- df_auditory |> 
  filter(condition %in% "Repetition-biased")

df_auditory_alt <- df_auditory |> 
  filter(condition %in% "Alternation-biased")


# Condition on the preceding response
df_function_fs <- CountConditionalProbabilityExtended(df_auditory_fs$response)
df_function_freq <- CountConditionalProbabilityExtended(df_auditory_freq$response)
df_function_rep <- CountConditionalProbabilityExtended(df_auditory_rep$response)
df_function_alt <- CountConditionalProbabilityExtended(df_auditory_alt$response)
  
# Combine dataframes
df_fs <- bind_cols(df_auditory_fs, df_function_fs)
df_freq <- bind_cols(df_auditory_freq, df_function_freq)
df_rep <- bind_cols(df_auditory_rep, df_function_rep)
df_alt <- bind_cols(df_auditory_alt, df_function_alt)

df_fs <- df_fs |> 
  mutate(across(c(starts_with("given")), factor))

df_freq <- df_freq |> 
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
df_givenAA_fs <- filtergivenAA(df_fs)
df_givenAB_fs <- filtergivenAB(df_fs)
df_givenBA_fs <- filtergivenBA(df_fs)
df_givenBB_fs <- filtergivenBB(df_fs)

# Frequency biased condtion
df_givenAA_freq <- filtergivenAA(df_freq)
df_givenAB_freq <- filtergivenAB(df_freq)
df_givenBA_freq <- filtergivenBA(df_freq)
df_givenBB_freq <- filtergivenBB(df_freq)

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
df1_all_fs <- df_fs |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAA_fs <- df_givenAA_fs |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAB_fs <- df_givenAB_fs |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBA_fs <- df_givenBA_fs |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBB_fs <- df_givenBB_fs |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))


# Frequency-biased
df1_all_freq <- df_freq |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAA_freq <- df_givenAA_freq |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenAB_freq <- df_givenAB_freq |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBA_freq <- df_givenBA_freq |> group_by(intensity) |> 
  summarise(nRight = sum(rightresponse),
            nLeft = sum(leftresponse))

df1_givenBB_freq <- df_givenBB_freq |> group_by(intensity) |> 
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
df1_all_fs$pRight <- df1_all_fs$nRight / (df1_all_fs$nRight + df1_all_fs$nLeft)
df1_givenAA_fs$pRight <- df1_givenAA_fs$nRight / (df1_givenAA_fs$nRight + df1_givenAA_fs$nLeft)
df1_givenAB_fs$pRight <- df1_givenAB_fs$nRight / (df1_givenAB_fs$nRight + df1_givenAB_fs$nLeft)
df1_givenBA_fs$pRight <- df1_givenBA_fs$nRight / (df1_givenBA_fs$nRight + df1_givenBA_fs$nLeft)
df1_givenBB_fs$pRight <- df1_givenBB_fs$nRight / (df1_givenBB_fs$nRight + df1_givenBB_fs$nLeft)

# Frequency-baised: Add proportion
df1_all_freq$pRight <- df1_all_freq$nRight / (df1_all_freq$nRight + df1_all_freq$nLeft)
df1_givenAA_freq$pRight <- df1_givenAA_freq$nRight / (df1_givenAA_freq$nRight + df1_givenAA_freq$nLeft)
df1_givenAB_freq$pRight <- df1_givenAB_freq$nRight / (df1_givenAB_freq$nRight + df1_givenAB_freq$nLeft)
df1_givenBA_freq$pRight <- df1_givenBA_freq$nRight / (df1_givenBA_freq$nRight + df1_givenBA_freq$nLeft)
df1_givenBB_freq$pRight <- df1_givenBB_freq$nRight / (df1_givenBB_freq$nRight + df1_givenBB_freq$nLeft)

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



# p1: Fully stochastic

df1_all_fs$Responses <- factor("all")
df1_givenAA_fs$Responses <- factor("preceding AA")
df1_givenAB_fs$Responses <- factor("preceding AB")
df1_givenBA_fs$Responses <- factor("preceding BA")
df1_givenBB_fs$Responses <- factor("preceding BB")


# df1_fs <- rbind(df1_all_fs, df1_givenAA_fs, df1_givenAB_fs, df1_givenBA_fs, df1_givenBB_fs)
df1_fs <- rbind(df1_givenBB_fs, df1_givenAB_fs, df1_all_fs, df1_givenBA_fs, df1_givenAA_fs)


p1 <- ggplot() +
  geom_point(data = df1_fs, aes(x = intensity, y = pRight, colour = Responses), show.legend = F)


# try to fix this!

# Fit glm
model_all_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_fs, family = binomial())
model_givenAA_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_fs, family = binomial())
model_givenAB_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_fs, family = binomial())
model_givenBA_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_fs, family = binomial())
model_givenBB_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_fs, family = binomial())

# Create data points
xseq <- seq(-6, 6, len = 1000)

# Predict
yseq_all_fs <- predict(model_all_fs, data.frame(intensity = xseq), type = "response")
yseq_givenAA_fs <- predict(model_givenAA_fs, data.frame(intensity = xseq), type = "response")
yseq_givenAB_fs <- predict(model_givenAB_fs, data.frame(intensity = xseq), type = "response")
yseq_givenBA_fs <- predict(model_givenBA_fs, data.frame(intensity = xseq), type = "response")
yseq_givenBB_fs <- predict(model_givenBB_fs, data.frame(intensity = xseq), type = "response")

# Curve
curve_all_fs <- data.frame(xseq, yseq_all_fs)
curve_givenAA_fs <- data.frame(xseq, yseq_givenAA_fs)
curve_givenAB_fs <- data.frame(xseq, yseq_givenAB_fs)
curve_givenBA_fs <- data.frame(xseq, yseq_givenBA_fs)
curve_givenBB_fs <- data.frame(xseq, yseq_givenBB_fs)

names(curve_all_fs)[names(curve_all_fs) == 'yseq_all_fs'] <- 'yseq'
names(curve_givenAA_fs)[names(curve_givenAA_fs) == 'yseq_givenAA_fs'] <- 'yseq'
names(curve_givenAB_fs)[names(curve_givenAB_fs) == 'yseq_givenAB_fs'] <- 'yseq'
names(curve_givenBA_fs)[names(curve_givenBA_fs) == 'yseq_givenBA_fs'] <- 'yseq'
names(curve_givenBB_fs)[names(curve_givenBB_fs) == 'yseq_givenBB_fs'] <- 'yseq'

curve_all_fs$Responses <- factor("all")
curve_givenAA_fs$Responses <- factor("preceding AA")
curve_givenAB_fs$Responses <- factor("preceding AB")
curve_givenBA_fs$Responses <- factor("preceding BA")
curve_givenBB_fs$Responses <- factor("preceding BB")

curve_fs <- rbind(curve_givenBB_fs, curve_givenAB_fs, curve_all_fs, curve_givenBA_fs, curve_givenAA_fs)


p1 <- p1 +
  geom_line(data = curve_fs, aes(x = xseq, y = yseq, colour = Responses)) +
  theme_classic() + 
  geom_hline(aes(yintercept = 0.5), linetype = 3, alpha = 0.75) +
  geom_vline(aes(xintercept = 0), linetype = 3, alpha = 0.75) +  
  xlab(" ") +
  ylab("P(<i>B</i>)") +
  labs(title = "A",
       subtitle = "Fully stochastic") +
  scale_color_manual(name = "Responses",
                     labels = c("preceding BB",
                                "preceding AB",
                                "all",
                                "preceding BA",
                                "preceding AA"),
                     values = c("#FDE725FF", "#5DC863FF", "black", "#3B528BFF", "#440154FF")) +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(-6,0,6)) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))


p1 <- p1 + theme(axis.title.y = element_markdown())

p1






# p2: Frequency-baised

df1_all_freq$Responses <- factor("all")
df1_givenAA_freq$Responses <- factor("preceding AA")
df1_givenAB_freq$Responses <- factor("preceding AB")
df1_givenBA_freq$Responses <- factor("preceding BA")
df1_givenBB_freq$Responses <- factor("preceding BB")


df1_freq <- rbind(df1_givenBB_freq, df1_givenAB_freq, df1_all_freq, df1_givenBA_freq, df1_givenAA_freq)


p2 <- ggplot() +
  geom_point(data = df1_freq, aes(x = intensity, y = pRight, colour = Responses), show.legend = F)




# Fit glm
model_all_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_freq, family = binomial())
model_givenAA_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_freq, family = binomial())
model_givenAB_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_freq, family = binomial())
model_givenBA_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_freq, family = binomial())
model_givenBB_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_freq, family = binomial())

# Create data points
xseq <- seq(-6, 6, len = 1000)

# Predict
yseq_all_freq <- predict(model_all_freq, data.frame(intensity = xseq), type = "response")
yseq_givenAA_freq <- predict(model_givenAA_freq, data.frame(intensity = xseq), type = "response")
yseq_givenAB_freq <- predict(model_givenAB_freq, data.frame(intensity = xseq), type = "response")
yseq_givenBA_freq <- predict(model_givenBA_freq, data.frame(intensity = xseq), type = "response")
yseq_givenBB_freq <- predict(model_givenBB_freq, data.frame(intensity = xseq), type = "response")

# Curve
curve_all_freq <- data.frame(xseq, yseq_all_freq)
curve_givenAA_freq <- data.frame(xseq, yseq_givenAA_freq)
curve_givenAB_freq <- data.frame(xseq, yseq_givenAB_freq)
curve_givenBA_freq <- data.frame(xseq, yseq_givenBA_freq)
curve_givenBB_freq <- data.frame(xseq, yseq_givenBB_freq)

names(curve_all_freq)[names(curve_all_freq) == 'yseq_all_freq'] <- 'yseq'
names(curve_givenAA_freq)[names(curve_givenAA_freq) == 'yseq_givenAA_freq'] <- 'yseq'
names(curve_givenAB_freq)[names(curve_givenAB_freq) == 'yseq_givenAB_freq'] <- 'yseq'
names(curve_givenBA_freq)[names(curve_givenBA_freq) == 'yseq_givenBA_freq'] <- 'yseq'
names(curve_givenBB_freq)[names(curve_givenBB_freq) == 'yseq_givenBB_freq'] <- 'yseq'

curve_all_freq$Responses <- factor("all")
curve_givenAA_freq$Responses <- factor("preceding AA")
curve_givenAB_freq$Responses <- factor("preceding AB")
curve_givenBA_freq$Responses <- factor("preceding BA")
curve_givenBB_freq$Responses <- factor("preceding BB")

curve_freq <- rbind(curve_givenBB_freq, curve_givenAB_freq, curve_all_freq, curve_givenBA_freq, curve_givenAA_freq)

p2 <- p2 +
  geom_line(data = curve_freq, aes(x = xseq, y = yseq, colour = Responses)) +
  theme_classic() + 
  geom_hline(aes(yintercept = 0.5), linetype = 3, alpha = 0.75) +
  geom_vline(aes(xintercept = 0), linetype = 3, alpha = 0.75) +  
  xlab(" ") +
  ylab(" ") +
  labs(title = "B",
       subtitle = "Frequency-biased") +
  scale_color_manual(name = "Responses", 
                     labels = c("preceding BB",
                                "preceding AB",
                                "all",
                                "preceding BA",
                                "preceding AA"),
                     values = c("#FDE725FF", "#5DC863FF", "black", "#3B528BFF", "#440154FF")) +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(-6,0,6)) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

p2 <- p2 + theme(axis.title.y = element_markdown())





# p3: Repetition-baised

df1_all_rep$Responses <- factor("all")
df1_givenAA_rep$Responses <- factor("preceding AA")
df1_givenAB_rep$Responses <- factor("preceding AB")
df1_givenBA_rep$Responses <- factor("preceding BA")
df1_givenBB_rep$Responses <- factor("preceding BB")

df1_rep <- rbind(df1_givenBB_rep, df1_givenAB_rep, df1_all_rep, df1_givenBA_rep, df1_givenAA_rep)


p3 <- ggplot() +
  geom_point(data = df1_rep, aes(x = intensity, y = pRight, colour = Responses), show.legend = F)




# Fit glm
model_all_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_rep, family = binomial())
model_givenAA_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_rep, family = binomial())
model_givenAB_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_rep, family = binomial())
model_givenBA_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_rep, family = binomial())
model_givenBB_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_rep, family = binomial())

# Create data points
xseq <- seq(-6, 6, len = 1000)

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

p3 <- p3 +
  geom_line(data = curve_rep, aes(x = xseq, y = yseq, colour = Responses)) +
  theme_classic() + 
  geom_hline(aes(yintercept = 0.5), linetype = 3, alpha = 0.75) +
  geom_vline(aes(xintercept = 0), linetype = 3, alpha = 0.75) +  
  xlab("Stimulus intensity") +
  ylab("P(<i>B</i>)") +
  labs(title = "C",
       subtitle = "Repetition-biased") +
  scale_color_manual(name = "Responses", 
                     labels = c("preceding BB",
                                "preceding AB",
                                "all",
                                "preceding BA",
                                "preceding AA"),
                     values = c("#FDE725FF", "#5DC863FF", "black", "#3B528BFF", "#440154FF")) +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(-6,0,6)) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))


p3 <- p3 + theme(axis.title.y = element_markdown())


# p4: Alternation-baised

df1_all_alt$Responses <- factor("all")
df1_givenAA_alt$Responses <- factor("preceding AA")
df1_givenAB_alt$Responses <- factor("preceding AB")
df1_givenBA_alt$Responses <- factor("preceding BA")
df1_givenBB_alt$Responses <- factor("preceding BB")


df1_alt <- rbind(df1_givenBB_alt, df1_givenAB_alt, df1_all_alt, df1_givenBA_alt, df1_givenAA_alt)


p4 <- ggplot() +
  geom_point(data = df1_alt, aes(x = intensity, y = pRight, colour = Responses), show.legend = F)




# Fit glm
model_all_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_alt, family = binomial())
model_givenAA_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_alt, family = binomial())
model_givenAB_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_alt, family = binomial())
model_givenBA_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_alt, family = binomial())
model_givenBB_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_alt, family = binomial())

# Create data points
xseq <- seq(-6, 6, len = 1000)

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

p4 <- p4 +
  geom_line(data = curve_alt, aes(x = xseq, y = yseq, colour = Responses)) +
  theme_classic() + 
  geom_hline(aes(yintercept = 0.5), linetype = 3, alpha = 0.75) +
  geom_vline(aes(xintercept = 0), linetype = 3, alpha = 0.75) +  
  xlab("Stimulus intensity") +
  ylab(" ") +
  labs(title = "D",
       subtitle = "Alternation-biased") +
  scale_color_manual(name = "Responses", 
                     labels = c("preceding BB",
                                "preceding AB",
                                "all",
                                "preceding BA",
                                "preceding AA"),
                     values = c("#FDE725FF", "#5DC863FF", "black", "#3B528BFF", "#440154FF")) +
  scale_y_continuous(breaks = c(0,1)) +
  scale_x_continuous(breaks = c(-6,0,6)) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        text = element_text(family = "Avenir"),
        plot.title = element_text(size=12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))


p4 <- p4 + theme(axis.title.y = element_markdown())



# Layout for patchwork
layout_psychometricAuditory <- 
  "
     AA
     BB
"


auditory_shift_Patchwork <-   ((p1 | p2) /
                                 (p3 | p4) +
                                 plot_layout(design = layout_psychometricAuditory, 
                                             widths = c(1,1),
                                             guides = "collect")) +
  plot_annotation()



ggsave(filename = "Figure5.tiff", 
       plot = auditory_shift_Patchwork, 
       width = 18, 
       height = 15, 
       path = "/Users/daniel.schlunegger/Desktop/Figures_iScience/",
       dpi = 300, 
       bg = "transparent", 
       units = "cm")
