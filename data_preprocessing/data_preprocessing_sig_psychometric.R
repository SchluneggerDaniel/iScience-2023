# Significance Tests for shifts in psychometric curves

library(tidyverse)
library(coin)

# Load the data
load("processed_data/Psychometric/auditoryPsychometricData.Rda")

# Reallocate df
dfa <- auditoryPsychometricData

aa_ba_fs <-   wilcoxsign_test(dfa$auditory_shift_givenAA_fs ~   dfa$auditory_shift_givenBA_fs,   distribution = "exact")
bb_ab_fs <-   wilcoxsign_test(dfa$auditory_shift_givenBB_fs ~   dfa$auditory_shift_givenAB_fs,   distribution = "exact")
aa_ba_freq <- wilcoxsign_test(dfa$auditory_shift_givenAA_freq ~ dfa$auditory_shift_givenBA_freq, distribution = "exact")
bb_ab_freq <- wilcoxsign_test(dfa$auditory_shift_givenBB_freq ~ dfa$auditory_shift_givenAB_freq, distribution = "exact")
aa_ba_rep <-  wilcoxsign_test(dfa$auditory_shift_givenAA_rep ~  dfa$auditory_shift_givenBA_rep,  distribution = "exact")
bb_ab_rep <-  wilcoxsign_test(dfa$auditory_shift_givenBB_rep ~  dfa$auditory_shift_givenAB_rep,  distribution = "exact")
aa_ba_alt <-  wilcoxsign_test(dfa$auditory_shift_givenAA_alt ~  dfa$auditory_shift_givenBA_alt,  distribution = "exact")
bb_ab_alt <-  wilcoxsign_test(dfa$auditory_shift_givenBB_alt ~  dfa$auditory_shift_givenAB_alt,  distribution = "exact")

# Create a data frame to store the results
results_df_auditory <- data.frame(Comparison = c("AA-BA (Fully stochastic)", 
                                                 "BB-AB (Fully stochastic)", 
                                                 "AA-BA (Frequency-biased)", 
                                                 "BB-AB (Frequency-biased)", 
                                                 "AA-BA (Repetition-biased)", 
                                                 "BB-AB (Repetition-biased)",
                                                 "AA-BA (Alternation-biased)", 
                                                 "BB-AB (Alternation-biased)"),
                                  Test = c(rep("Wilcoxon", times = 8)),
                                  Estimate = c(statistic(aa_ba_fs), 
                                               statistic(bb_ab_fs), 
                                               statistic(aa_ba_freq), 
                                               statistic(bb_ab_freq), 
                                               statistic(aa_ba_rep), 
                                               statistic(bb_ab_rep),
                                               statistic(aa_ba_alt), 
                                               statistic(bb_ab_alt)),
                                  P.Value = round(c(midpvalue(aa_ba_fs), 
                                                    midpvalue(bb_ab_fs), 
                                                    midpvalue(aa_ba_freq), 
                                                    midpvalue(bb_ab_freq), 
                                                    midpvalue(aa_ba_rep), 
                                                    midpvalue(bb_ab_rep),
                                                    midpvalue(aa_ba_alt), 
                                                    midpvalue(bb_ab_alt)),
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

visual_aa_ba_neut <- wilcoxsign_test(dfv$visual_shift_givenAA_neut ~ dfv$visual_shift_givenBA_neut, distribution = "exact")
visual_bb_ab_neut <- wilcoxsign_test(dfv$visual_shift_givenBB_neut ~ dfv$visual_shift_givenAB_neut, distribution = "exact")
visual_aa_ba_rep <-  wilcoxsign_test(dfv$visual_shift_givenAA_rep ~  dfv$visual_shift_givenBA_rep,  distribution = "exact")
visual_bb_ab_rep <-  wilcoxsign_test(dfv$visual_shift_givenBB_rep ~  dfv$visual_shift_givenAB_rep,  distribution = "exact")
visual_aa_ba_alt <-  wilcoxsign_test(dfv$visual_shift_givenAA_alt ~  dfv$visual_shift_givenBA_alt,  distribution = "exact")
visual_bb_ab_alt <-  wilcoxsign_test(dfv$visual_shift_givenBB_alt ~  dfv$visual_shift_givenAB_alt,  distribution = "exact")


results_df_visual <- data.frame(Comparison = c("AA-BA (Neutral)", 
                                               "BB-AB (Neutral)", 
                                               "AA-BA (Repetitive)", 
                                               "BB-AB (Repetitive)",
                                               "AA-BA (Alternating)", 
                                               "BB-AB (Alternating)"),
                                  Test = c(rep("Wilcoxon", times = 6)),
                                  Estimate = c(statistic(visual_aa_ba_neut), 
                                               statistic(visual_bb_ab_neut), 
                                               statistic(visual_aa_ba_rep), 
                                               statistic(visual_bb_ab_rep),
                                               statistic(visual_aa_ba_alt), 
                                               statistic(visual_bb_ab_alt)),
                                  P.Value = round(c(midpvalue(visual_aa_ba_neut), 
                                                    midpvalue(visual_bb_ab_neut), 
                                                    midpvalue(visual_aa_ba_rep), 
                                                    midpvalue(visual_bb_ab_rep),
                                                    midpvalue(visual_aa_ba_alt), 
                                                    midpvalue(visual_bb_ab_alt)),
                                                  digits = 5))


# Print the results data frame
print(results_df_visual)

# Create LaTeX code
latex_table <- xtable::xtable(results_df_visual, digits = 5)

# Print the LaTeX code
print(latex_table, include.rownames = FALSE)


