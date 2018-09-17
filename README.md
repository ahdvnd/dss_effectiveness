### README

All the code are in the code folder. The main analysis code is in the `matching_analaysis.R` folder. This is the explanation for each file.

* `survey_cleaning.R` tidies the survey data from Survey Monkey.
* `coursera_cleaning.R` imports and tidies the Coursera data mainly from `users`, `course_grades`, and `course_progress` tables.
* `data_prep.R` merges the two sources of data (survey and Coursera) and creates all the tables needed for the analysis.
* `amelia_var_preparation.R` cleans the data from Ameilia imputation process. Amelia is sentitive to the type of objects so this code makes variables as factors.
* `matching_analysis.R` runs the matching analysis.
* `nonbipartite_match.R` runs the matching analysis for various treatment groups (different number of courses passed from the specialization)
* `r_programming_stats.R` returns some statistics about the course R Programming. The students who enrolled in this course were the ones who were contacted for the survey.
* `balance_statustics_tables.R` compares the two population (survey respondents and Coursera learners) and returns the balance statistics.
* `diff_in_diff.R` code performs differences in differences method on the data.
* `graphs.R` creates all the graphs for the paper. The code used for creating exploratory graphs are in the folder `/raw_code/raw_graph.R`.