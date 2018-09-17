## code for checking the effect of DSS on 
## low-income and low-education individuals.


packages <- c("dplyr", "readr", "plyr", "IPtoCountry", "ggplot2", "gridExtra", 
              "MatchIt", "cobalt", "skimr", "stargazer", "Amelia", "summarytools")
instapack <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
suppressMessages(suppressWarnings(instapack(packages)))



clean <- readRDS("./data/tidy_data/cleaned_data.rds")

df <- clean %>% 
    filter(education %in% c("Associate degree", 
                            "High school degree or equivalent (e.g., GED)",
                            "Less than high school degree",
                            "Some college but no degree")) %>% 
    mutate(income_change = new.income.num - old.income.num) %>% 
    mutate(income_pct = 100*income_change/old.income.num) %>% 
    filter(income_pct >= 100)



table(df$income_change)





