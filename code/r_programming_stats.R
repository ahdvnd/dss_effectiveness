# Loading all the packages ------------------------------------------------

packages <- c("dplyr", "readr", "plyr", "IPtoCountry", "ggplot2", "gridExtra", 
              "MatchIt", "cobalt", "skimr", "stargazer", "Amelia", "summarytools", "crsra")
instapack <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
suppressMessages(suppressWarnings(instapack(packages)))


## importing course data for R programming
rprog <- crsra_import_course(workdir = "./data/raw_data/databases/r_programming_1505758002450")
# rprog <- readRDS("./data/tidy_data/r_programming_users.rds")

## the number of enrolled students based on the table course_grades
rprog %>% 
    .[["course_grades"]] %>% 
    dplyr::summarise(n=n())

## the number of enrolled students based on the table users
rprog %>% 
    .[["users"]] %>% 
    dplyr::summarise(n=n())

## the number of enrolled students based on the table course_memberships
rprog %>% 
    .[["course_memberships"]] %>%
    dplyr::group_by(course_membership_role) %>% 
    distinct(jhu_user_id) %>% 
    dplyr::summarise(n=n())

## the number of enrolled who have at least taken 1 course item
rprog %>% 
    .[["course_grades"]] %>% 
    dplyr::filter(course_grade_overall_passed_items >= 7) %>% 
    dplyr::summarise(n=n())

    