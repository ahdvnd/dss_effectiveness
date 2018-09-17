# Loading all the packages ------------------------------------------------

packages <- c("dplyr", "readr", "plyr", "IPtoCountry", "ggplot2", "gridExtra", "survey", "causaldrf",
              "MatchIt", "cobalt", "skimr", "stargazer", "Amelia", "summarytools", "Zelig", "WeightIt")
instapack <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
suppressMessages(suppressWarnings(instapack(packages)))


## sample selection params
nn = 10 ## no. of countries in the sample
incl_india_china = TRUE
cutoff = 50
cutoff1 = 0
cutoff2 = 50 # cutoff of the the percentage of courses to have passed to count as passing the specialization
## imputation params
mutate <- FALSE # mutate or not to mutate
mute_num <- 3 # the number of mutations in ameila
whichm <- 2 # which mutation to use for the matching algorithm


source("code/prop_wt.R") # includes the functions for propensity weighting
source("code/prop_match.R") # includes the functions for propensity matching
source("code/prep_fns.R") # loads the data and includes the functions for data preparation
source("code/mutation.R") # includes the function for mutation


# loads the data based on the cutoff point and the number of countries
clean.progress.matching <- var_prep(ctry_cutoff(cutoff = cutoff, nn = nn)) %>% 
    dplyr::mutate(changed.job = ifelse(same.job == "No", 1, 0)) %>% 
    dplyr::mutate(switch.jobs.1yr.norm = as.numeric(switch.jobs.1yr)/5) %>% 
    dplyr::mutate(switch.jobs.2yr.norm = as.numeric(switch.jobs.2yr)/5) %>%
    dplyr::mutate(changed.jobs.num = as.numeric(num.jobs.changed))

# mutation algorithm and data preparation (making them as factor)
clean.progress.mutated <- mutation(clean.progress.matching)


## analysis of continuous treatment
#------------------------------------------------------

dfmob <- clean.progress.mutated %>% 
    mutate(log.new.income.num = log(new.income.num+1)) %>% 
    as.data.frame()


## hirano-imbens estimator
summary(causaldrf::hi_est(Y = changed.job,
                              treat = passed,
                              treat_formula = passed ~ expertise.in.ds +
                                  were.employed +
                                  gender +
                                  age +
                                  white +
                                  as.factor(education.level) +
                                  major +
                                  major.related.ds +
                                  motive.career +
                                  country +
                                  only.jhu,
                              outcome_formula = changed.job ~ passed +
                                  gps +
                                  as.factor(dfmob$education.level) +
                              dfmob$gender +
                              dfmob$age +
                              dfmob$major +
                              dfmob$country +
                              dfmob$white,
                              data = dfmob,
                              grid_val = seq(0, 100, by = 10),
                              treat_mod = "Normal"))



summary(causaldrf::hi_est(Y = changed.jobs.num,
                          treat = passed,
                          treat_formula = passed ~ expertise.in.ds +
                              were.employed +
                              gender +
                              age +
                              white +
                              as.factor(education.level) +
                              major +
                              major.related.ds +
                              motive.career +
                              country +
                              only.jhu,
                          outcome_formula = changed.jobs.num ~ passed +
                              gps +
                              as.factor(dfmob$education.level) +
                              dfmob$gender +
                              dfmob$age +
                              dfmob$major +
                              dfmob$country +
                              dfmob$white,
                          data = dfmob,
                          grid_val = seq(0, 100, by = 10),
                          treat_mod = "Normal"))



# summary(lm(changed.job ~ pass.fail + 
#                education.level +
#                gender +
#                age +
#                major +
#                country +
#                white,
#            data = clean.progress.mutated))
# 
# summary(lm(changed.jobs.num ~ pass.fail + 
#                education.level +
#                gender +
#                age +
#                major +
#                country +
#                white,
#            data = clean.progress.mutated))


# summary(lm(switch.jobs.1yr.norm ~ pass.fail +
#                education.level +
#                gender +
#                age +
#                major +
#                country +
#                white,
#            data = clean.progress.mutated))
# # 
# summary(lm(switch.jobs.2yr.norm ~ pass.fail +
#                education.level +
#                gender +
#                age +
#                major +
#                country +
#                white,
#            data = clean.progress.mutated))
