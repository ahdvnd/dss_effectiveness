# Loading all the packages ------------------------------------------------

packages <- c("dplyr", "readr", "plyr", "IPtoCountry", "ggplot2", "gridExtra", "survey", "causaldrf",
              "MatchIt", "cobalt", "skimr", "stargazer", "Amelia", "summarytools", "Zelig", "WeightIt",
              "scales")
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
clean.progress.matching <- var_prep(ctry_cutoff(cutoff = cutoff, nn = nn))

## removes the vars for job mobility analysis
if (all(mobility_vars %in% colnames(clean.progress.matching))){
    clean.progress.matching <- clean.progress.matching %>% 
        dplyr::select(-one_of(mobility_vars))
}

# mutation algorithm and data preparation (making them as factor)
clean.progress.mutated <- mutation(clean.progress.matching)

## Propensity weighted data and regression results
prop_wt_out <- prop_wt(clean.progress.mutated)
clean.progress.weighted <- survey::svydesign(ids =~1, data = prop_wt_out, weights = prop_wt_out$ate)
model <- survey::svyglm(new.income.num ~ 
                          pass.fail + 
                          education.level +
                          gender +
                          age +
                          major +
                          country +
                          white,
                      design=clean.progress.weighted)
summary(model)
#summ(model, confint = TRUE, model.info = FALSE, model.fit = FALSE) 
#fit.svyglm(model)




# mean new.income.num for gender groups unweighted
aggregate(new.income.num ~ education.level, clean.progress.mutated, mean)
# mean new.income.num for gender groups weighted
svyby(~new.income.num, by=~education.level, design = clean.progress.weighted, FUN = svymean)



# out <- summary(lm(new.income.num ~ pass.fail + 
#                           education.level +
#                           gender +
#                           age +
#                           major +
#                           country +
#                           white,
#                       data=prop_wt_out,
#                   weights = (ate)))




## Regression results from propensity matching
#prop_match(clean.progress.mutated)




#library(cobalt)
# Checking balance before and after matching:
#bal.tab(m.out, m.threshold = 0.1, un = TRUE)
#bal.plot(m.out, var.name = "gender")

# interesting for plotting https://cran.r-project.org/web/packages/cobalt/README.html and 
# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt_A0_basic_use.html



## how to use lm in a pipe and how not to have to pass all vars in lm
#df<-data.frame(y=rnorm(10),x1=rnorm(10),x2=rnorm(10))
#df %>% select(y,x1) %>% lm(formula = y~.)




