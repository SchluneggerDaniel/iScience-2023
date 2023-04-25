iScience-2023
================

This is the repository for the article: *Probabilistic integration of
preceding responses explains response bias in perceputal decision
making* by Schlunegger & Mast (2023) in *iScience* **(under review)**.

## Reproduce results and figures

To replicate the results and figures reported in the manuscript, execute
the R-scripts in the specified order as provided below. The same applies
to all scripts ending with “\_generative”.

Essential Data Preprocessing:

1.  data_preprocessing_pt1.R
2.  data_preprocessing_pt2.R

GLM Analysis:

3.  analysis_GLM.R
4.  analysis_GLM_Easy_vs_Difficult.R
5.  analysis_GLM_LearningModels.R
6.  analysis_GLM_RepPref.R

Log-Likelihood Analysis:

7.  analysis_LLhood.R  
8.  analysis_LLhood_Easy_vs_Difficult.R
9.  analysis_LLhood_LearningModels.R
10. analysis_LLhood_LearningModels_Easy_vs_Difficult.R

Data Preprocessing Results:

11. data_preprocessing_LLhood_delta.R
12. data_preprocessing_LLhood_absolute.R
13. data_preprocessing_sig_LLhood_absolute.R
14. data_preprocessing_LLhood_absolute_vs_Stim.R
15. data_preprocessing_sig_LLhood_absolute_vs_Stim.R
16. data_preprocessing_GLM_absolute.R
17. data_preprocessing_sig_GLM_absolute.R
18. data_preprocessing_orthogonalPredictions.R
19. data_preprocessing_psychometricAuditory.R
20. data_preprocessing_psychometricVisual.R
21. data_preprocessing_sig_psychometric.R

Figures Main Text:

22. plot_LLhood.R
23. plot_LLhood_vs_Stim.R
24. plot_orthogonalPredictions.R
25. plot_auditoryShift.R

Supplemental Information:

26. plot_accuracy.R
27. plot_LLhood_Omega.R
28. plot_GLM_absolute.R
29. plot_GLM_coefficients.R
30. plot_visualShift.R
