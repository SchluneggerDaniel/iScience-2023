# Significance Tests for shifts in psychometric curves

library(tidyverse)

load("processed_data/Psychometric/auditoryPsychometricData.Rda")

dfa <- auditoryPsychometricData

# Store the results of t-tests
ttest_aa_ba <- wilcox.test(dfa$auditory_shift_givenAA_fs - dfa$auditory_shift_givenBA_fs, alternative = "greater", exact = FALSE)
ttest_bb_ab <- t.test(dfa$auditory_shift_givenBB_fs - dfa$auditory_shift_givenAB_fs, alternative = "less")
ttest_aa_ba_freq <- t.test(dfa$auditory_shift_givenAA_freq - dfa$auditory_shift_givenBA_freq, alternative = "greater")
ttest_bb_ab_freq <- t.test(dfa$auditory_shift_givenBB_freq - dfa$auditory_shift_givenAB_freq, alternative = "less")
ttest_aa_ba_rep <- t.test(dfa$auditory_shift_givenAA_rep - dfa$auditory_shift_givenBA_rep, alternative = "greater")
ttest_bb_ab_rep <- t.test(dfa$auditory_shift_givenBB_rep - dfa$auditory_shift_givenAB_rep, alternative = "less")
ttest_aa_ba_alt <- t.test(dfa$auditory_shift_givenAA_alt - dfa$auditory_shift_givenBA_alt, alternative = "greater")
ttest_bb_ab_alt <- t.test(dfa$auditory_shift_givenBB_alt - dfa$auditory_shift_givenAB_alt, alternative = "less")

# Create a data frame to store the results
results_df_auditory <- data.frame(Comparison = c("AA-BA (Fully stochastic)", 
                                        "BB-AB (Fully stochastic)", 
                                        "AA-BA (Frequency-biased)", 
                                        "BB-AB (Frequency-biased)", 
                                        "AA-BA (Repetition-biased)", 
                                        "BB-AB (Repetition-biased)",
                                        "AA-BA (Alternation-biased)", 
                                        "BB-AB (Alternation-biased)"),
                         Test = c("Wilcoxon", rep("T.test", times = 7)),
                         Estimate = c(ttest_aa_ba$statistic, 
                                             ttest_bb_ab$estimate, 
                                             ttest_aa_ba_freq$estimate, 
                                             ttest_bb_ab_freq$estimate, 
                                             ttest_aa_ba_rep$estimate, 
                                             ttest_bb_ab_rep$estimate,
                                             ttest_aa_ba_alt$estimate, 
                                             ttest_bb_ab_alt$estimate),
                         P.Value = round(c(ttest_aa_ba$p.value, 
                                           ttest_bb_ab$p.value, 
                                           ttest_aa_ba_freq$p.value, 
                                           ttest_bb_ab_freq$p.value, 
                                           ttest_aa_ba_rep$p.value, 
                                           ttest_bb_ab_rep$p.value,
                                           ttest_aa_ba_alt$p.value, 
                                           ttest_bb_ab_alt$p.value),
                                           digits = 5))




# Print the results data frame
print(results_df_auditory)

# Create LaTeX code
latex_table <- xtable::xtable(results_df_auditory, digits = 5)

# Print the LaTeX code
print(latex_table, include.rownames = FALSE)





# Visual 
load("processed_data/Psychometric/visualPsychometricData.Rda")

dfv <- visualPsychometricData

# Fix one NaN value
dfv$visual_shift_givenAB_alt[length(dfv$visual_shift_givenAB_alt)] <- mean(dfv$visual_shift_givenAB_alt, na.rm = TRUE)



ttest_aa_ba <- t.test(dfv$visual_shift_givenAA_neut - dfv$visual_shift_givenBA_neut, alternative = "greater")
ttest_bb_ab <- t.test(dfv$visual_shift_givenBB_neut - dfv$visual_shift_givenAB_neut, alternative = "less")
ttest_aa_ba_rep <- wilcox.test(dfv$visual_shift_givenAA_rep - dfv$visual_shift_givenBA_rep, alternative = "greater", exact = FALSE)
ttest_bb_ab_rep <- t.test(dfv$visual_shift_givenBB_rep - dfv$visual_shift_givenAB_rep, alternative = "less")
ttest_aa_ba_alt <- wilcox.test(dfv$visual_shift_givenAA_alt - dfv$visual_shift_givenBA_alt, alternative = "greater", exact = FALSE)
ttest_bb_ab_alt <- t.test(dfv$visual_shift_givenBB_alt - dfv$visual_shift_givenAB_alt, alternative = "less")


results_df_visual <- data.frame(Comparison = c("AA-BA (Neutral)", 
                                                 "BB-AB (Neutral)", 
                                                 "AA-BA (Repetitive)", 
                                                 "BB-AB (Repetitive)",
                                                 "AA-BA (Alternating)", 
                                                 "BB-AB (Alternating)"),
                                  Test = c("T.test", "T.test", "Wilcoxon", "T.test", "Wilcoxon", "T.test"),
                                  Estimate = c(ttest_aa_ba$estimate, 
                                               ttest_bb_ab$estimate, 
                                               ttest_aa_ba_rep$statistic, 
                                               ttest_bb_ab_rep$estimate,
                                               ttest_aa_ba_alt$statistic, 
                                               ttest_bb_ab_alt$estimate),
                                  P.Value = round(c(ttest_aa_ba$p.value, 
                                                    ttest_bb_ab$p.value, 
                                                    ttest_aa_ba_rep$p.value, 
                                                    ttest_bb_ab_rep$p.value,
                                                    ttest_aa_ba_alt$p.value, 
                                                    ttest_bb_ab_alt$p.value),
                                                  digits = 5))


# Print the results data frame
print(results_df_visual)

# Create LaTeX code
latex_table <- xtable::xtable(results_df_visual, digits = 5)

# Print the LaTeX code
print(latex_table, include.rownames = FALSE)


