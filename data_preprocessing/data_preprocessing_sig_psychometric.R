# Significance Tests for shifts in psychometric curves

library(tidyverse)

# Load the data
load("processed_data/Psychometric/auditoryPsychometricData.Rda")

# Reallocate df
dfa <- auditoryPsychometricData

aa_ba_fs <-   wilcox.test(dfa$auditory_shift_givenAA_fs -   dfa$auditory_shift_givenBA_fs,   exact = FALSE)
bb_ab_fs <-   wilcox.test(dfa$auditory_shift_givenBB_fs -   dfa$auditory_shift_givenAB_fs,   exact = FALSE)
aa_ba_freq <- wilcox.test(dfa$auditory_shift_givenAA_freq - dfa$auditory_shift_givenBA_freq, exact = FALSE)
bb_ab_freq <- wilcox.test(dfa$auditory_shift_givenBB_freq - dfa$auditory_shift_givenAB_freq, exact = FALSE)
aa_ba_rep <-  wilcox.test(dfa$auditory_shift_givenAA_rep -  dfa$auditory_shift_givenBA_rep,  exact = FALSE)
bb_ab_rep <-  wilcox.test(dfa$auditory_shift_givenBB_rep -  dfa$auditory_shift_givenAB_rep,  exact = FALSE)
aa_ba_alt <-  wilcox.test(dfa$auditory_shift_givenAA_alt -  dfa$auditory_shift_givenBA_alt,  exact = FALSE)
bb_ab_alt <-  wilcox.test(dfa$auditory_shift_givenBB_alt -  dfa$auditory_shift_givenAB_alt,  exact = FALSE)

# Create a data frame to store the results
results_df_auditory <- data.frame(Comparison = c("AA-BA Fully stochastic", 
                                                 "BB-AB Fully stochastic", 
                                                 "AA-BA Frequency-biased", 
                                                 "BB-AB Frequency-biased", 
                                                 "AA-BA Repetition-biased", 
                                                 "BB-AB Repetition-biased",
                                                 "AA-BA Alternation-biased", 
                                                 "BB-AB Alternation-biased"),
                                  Test = c(rep("Wilcoxon", times = 8)),
                                  Estimate = c(aa_ba_fs$statistic, 
                                               bb_ab_fs$statistic, 
                                               aa_ba_freq$statistic, 
                                               bb_ab_freq$statistic, 
                                               aa_ba_rep$statistic, 
                                               bb_ab_rep$statistic,
                                               aa_ba_alt$statistic, 
                                               bb_ab_alt$statistic),
                                  P.Value = round(c(aa_ba_fs$p.value, 
                                                    bb_ab_fs$p.value, 
                                                    aa_ba_freq$p.value, 
                                                    bb_ab_freq$p.value, 
                                                    aa_ba_rep$p.value, 
                                                    bb_ab_rep$p.value,
                                                    aa_ba_alt$p.value, 
                                                    bb_ab_alt$p.value),
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

visual_aa_ba_neut <- wilcox.test(dfv$visual_shift_givenAA_neut - dfv$visual_shift_givenBA_neut, exact = FALSE)
visual_bb_ab_neut <- wilcox.test(dfv$visual_shift_givenBB_neut - dfv$visual_shift_givenAB_neut, exact = FALSE)
visual_aa_ba_rep <-  wilcox.test(dfv$visual_shift_givenAA_rep -  dfv$visual_shift_givenBA_rep,  exact = FALSE)
visual_bb_ab_rep <-  wilcox.test(dfv$visual_shift_givenBB_rep -  dfv$visual_shift_givenAB_rep,  exact = FALSE)
visual_aa_ba_alt <-  wilcox.test(dfv$visual_shift_givenAA_alt -  dfv$visual_shift_givenBA_alt,  exact = FALSE)
visual_bb_ab_alt <-  wilcox.test(dfv$visual_shift_givenBB_alt -  dfv$visual_shift_givenAB_alt,  exact = FALSE)


results_df_visual <- data.frame(Comparison = c("AA-BA (Neutral)", 
                                               "BB-AB (Neutral)", 
                                               "AA-BA (Repetitive)", 
                                               "BB-AB (Repetitive)",
                                               "AA-BA (Alternating)", 
                                               "BB-AB (Alternating)"),
                                  Test = c(rep("Wilcoxon", times = 6)),
                                  Estimate = c(visual_aa_ba_neut$statistic, 
                                               visual_bb_ab_neut$statistic, 
                                               visual_aa_ba_rep$statistic, 
                                               visual_bb_ab_rep$statistic,
                                               visual_aa_ba_alt$statistic, 
                                               visual_bb_ab_alt$statistic),
                                  P.Value = round(c(visual_aa_ba_neut$p.value, 
                                                    visual_bb_ab_neut$p.value, 
                                                    visual_aa_ba_rep$p.value, 
                                                    visual_bb_ab_rep$p.value,
                                                    visual_aa_ba_alt$p.value, 
                                                    visual_bb_ab_alt$p.value),
                                                  digits = 5))


# Print the results data frame
print(results_df_visual)

# Create LaTeX code
latex_table <- xtable::xtable(results_df_visual, digits = 5)

# Print the LaTeX code
print(latex_table, include.rownames = FALSE)


